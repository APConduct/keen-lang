use crate::ast::{self, Program};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use std::collections::HashMap;
use std::ffi::CString;

pub struct KeenCodegen {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    module: JITModule,
    pointer_type: types::Type,
    int_type: types::Type,
    float_type: types::Type,
    bool_type: types::Type,
    string_pool: Vec<CString>,
}

#[derive(Debug)]
pub enum CodegenError {
    Module(String),
    Compilation(String),
    Type(String),
    Runtime(String),
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodegenError::Module(msg) => write!(f, "Module error: {}", msg),
            CodegenError::Compilation(msg) => write!(f, "Compilation error: {}", msg),
            CodegenError::Type(msg) => write!(f, "Type error: {}", msg),
            CodegenError::Runtime(msg) => write!(f, "Runtime error: {}", msg),
        }
    }
}

impl std::error::Error for CodegenError {}

impl KeenCodegen {
    pub fn new() -> Result<Self, CodegenError> {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap();
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .map_err(|e| CodegenError::Module(format!("Failed to create ISA: {}", e)))?;

        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        let module = JITModule::new(builder);

        let pointer_type = module.target_config().pointer_type();

        Ok(KeenCodegen {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
            pointer_type,
            int_type: types::I64,
            float_type: types::F64,
            bool_type: types::I8,
            string_pool: Vec::new(),
        })
    }

    pub fn compile_program(&mut self, program: &Program) -> Result<(), CodegenError> {
        // First, declare the print function
        self.declare_print_function()?;

        for item in &program.items {
            self.compile_item(item)?;
        }
        Ok(())
    }

    fn declare_print_function(&mut self) -> Result<(), CodegenError> {
        // Declare print functions for different types
        let mut int_sig = self.module.make_signature();
        int_sig.params.push(AbiParam::new(self.int_type));
        int_sig.returns.push(AbiParam::new(self.int_type));

        let mut float_sig = self.module.make_signature();
        float_sig.params.push(AbiParam::new(self.float_type));
        float_sig.returns.push(AbiParam::new(self.int_type));

        let mut bool_sig = self.module.make_signature();
        bool_sig.params.push(AbiParam::new(self.bool_type));
        bool_sig.returns.push(AbiParam::new(self.int_type));

        let mut string_sig = self.module.make_signature();
        string_sig.params.push(AbiParam::new(self.pointer_type));
        string_sig.returns.push(AbiParam::new(self.int_type));

        self.module
            .declare_function("keen_print_int", Linkage::Import, &int_sig)
            .map_err(|e| {
                CodegenError::Module(format!("Failed to declare keen_print_int: {}", e))
            })?;

        self.module
            .declare_function("keen_print_float", Linkage::Import, &float_sig)
            .map_err(|e| {
                CodegenError::Module(format!("Failed to declare keen_print_float: {}", e))
            })?;

        self.module
            .declare_function("keen_print_bool", Linkage::Import, &bool_sig)
            .map_err(|e| {
                CodegenError::Module(format!("Failed to declare keen_print_bool: {}", e))
            })?;

        self.module
            .declare_function("keen_print_string", Linkage::Import, &string_sig)
            .map_err(|e| {
                CodegenError::Module(format!("Failed to declare keen_print_string: {}", e))
            })?;

        Ok(())
    }

    fn compile_item(&mut self, item: &ast::Item) -> Result<(), CodegenError> {
        match item {
            ast::Item::Function(func) => self.compile_function(func),
            ast::Item::TypeDef(_) => {
                // Type definitions don't generate code directly
                Ok(())
            }
            ast::Item::VariableDecl(_var) => {
                // Global variables would be handled here
                // For now, we'll skip them as they need special handling
                Ok(())
            }
        }
    }

    fn compile_function(&mut self, func: &ast::Function) -> Result<(), CodegenError> {
        // Clear the context for the new function
        self.ctx.clear();

        // Create function signature
        let mut sig = self.module.make_signature();

        // Add parameters
        for param in &func.params {
            let param_type = self.get_cranelift_type(&param.type_annotation)?;
            sig.params.push(AbiParam::new(param_type));
        }

        // Add return type
        if let Some(return_type) = &func.return_type {
            let ret_type = self.get_cranelift_type(&Some(return_type.clone()))?;
            sig.returns.push(AbiParam::new(ret_type));
        }

        // Create function in module
        let func_id = self
            .module
            .declare_function(&func.name, Linkage::Export, &sig)
            .map_err(|e| CodegenError::Module(format!("Failed to declare function: {}", e)))?;

        // Define function
        self.ctx.func.signature = sig;
        self.ctx.func.name = cranelift::codegen::ir::UserFuncName::user(0, func_id.as_u32());

        // Build function body
        {
            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            // Create variable map for parameters
            let mut variables = HashMap::new();
            for (i, param) in func.params.iter().enumerate() {
                let var = Variable::new(i);
                let param_val = builder.block_params(entry_block)[i];
                let param_type = Self::get_cranelift_type_static(
                    &param.type_annotation,
                    self.int_type,
                    self.float_type,
                    self.bool_type,
                    self.pointer_type,
                )?;
                builder.declare_var(var, param_type);
                builder.def_var(var, param_val);
                variables.insert(param.name.clone(), var);
            }

            // Compile function body
            let return_val = match &func.body {
                ast::FunctionBody::Expression(expr) => Self::compile_expression_static(
                    expr,
                    &mut builder,
                    &mut variables,
                    self.int_type,
                    self.float_type,
                    self.bool_type,
                    self.pointer_type,
                )?,
                ast::FunctionBody::Block(statements) => {
                    let mut last_val = None;
                    for stmt in statements {
                        last_val = Some(Self::compile_statement_static(
                            stmt,
                            &mut builder,
                            &mut variables,
                            self.int_type,
                            self.float_type,
                            self.bool_type,
                            self.pointer_type,
                        )?);
                    }
                    last_val.unwrap_or_else(|| builder.ins().iconst(self.int_type, 0))
                }
            };

            // Return the value
            if func.return_type.is_some() {
                builder.ins().return_(&[return_val]);
            } else {
                builder.ins().return_(&[]);
            }

            builder.finalize();
        }

        // Define the function in the module
        self.module
            .define_function(func_id, &mut self.ctx)
            .map_err(|e| CodegenError::Compilation(format!("Failed to define function: {}", e)))?;

        // Clear context
        self.module.clear_context(&mut self.ctx);

        Ok(())
    }

    fn compile_statement_static(
        stmt: &ast::Statement,
        builder: &mut FunctionBuilder,
        variables: &mut HashMap<String, Variable>,
        int_type: types::Type,
        float_type: types::Type,
        bool_type: types::Type,
        pointer_type: types::Type,
    ) -> Result<Value, CodegenError> {
        match stmt {
            ast::Statement::Expression(expr) => Self::compile_expression_static(
                expr,
                builder,
                variables,
                int_type,
                float_type,
                bool_type,
                pointer_type,
            ),
            ast::Statement::VariableDecl(var_decl) => {
                let val = Self::compile_expression_static(
                    &var_decl.value,
                    builder,
                    variables,
                    int_type,
                    float_type,
                    bool_type,
                    pointer_type,
                )?;
                let var = Variable::new(variables.len());
                let var_type = Self::get_cranelift_type_static(
                    &var_decl.type_annotation,
                    int_type,
                    float_type,
                    bool_type,
                    pointer_type,
                )?;
                builder.declare_var(var, var_type);
                builder.def_var(var, val);
                variables.insert(var_decl.name.clone(), var);
                Ok(val)
            }
            ast::Statement::DestructuringDecl { pattern: _, value } => {
                // For now, just compile the value
                // TODO: Implement proper destructuring
                Self::compile_expression_static(
                    value,
                    builder,
                    variables,
                    int_type,
                    float_type,
                    bool_type,
                    pointer_type,
                )
            }
        }
    }

    fn compile_expression_static(
        expr: &ast::Expression,
        builder: &mut FunctionBuilder,
        variables: &mut HashMap<String, Variable>,
        int_type: types::Type,
        float_type: types::Type,
        bool_type: types::Type,
        _pointer_type: types::Type,
    ) -> Result<Value, CodegenError> {
        match expr {
            ast::Expression::Literal(lit) => {
                Self::compile_literal_static(lit, builder, int_type, float_type, bool_type)
            }

            ast::Expression::Identifier(name) => {
                if let Some(&var) = variables.get(name) {
                    Ok(builder.use_var(var))
                } else {
                    Err(CodegenError::Compilation(format!(
                        "Undefined variable: {}",
                        name
                    )))
                }
            }

            ast::Expression::Binary { left, op, right } => {
                let left_val = Self::compile_expression_static(
                    left,
                    builder,
                    variables,
                    int_type,
                    float_type,
                    bool_type,
                    _pointer_type,
                )?;
                let right_val = Self::compile_expression_static(
                    right,
                    builder,
                    variables,
                    int_type,
                    float_type,
                    bool_type,
                    _pointer_type,
                )?;
                Self::compile_binary_op_static(op, left_val, right_val, builder)
            }

            ast::Expression::Call { function, args } => {
                match function.as_ref() {
                    ast::Expression::Identifier(name) if name == "print" => {
                        Self::compile_print_call_static(
                            args,
                            builder,
                            variables,
                            int_type,
                            float_type,
                            bool_type,
                            _pointer_type,
                        )
                    }
                    _ => {
                        // TODO: Implement general function calls
                        Err(CodegenError::Compilation(
                            "General function calls not yet implemented".to_string(),
                        ))
                    }
                }
            }

            ast::Expression::FieldAccess {
                object: _,
                field: _,
            } => {
                // TODO: Implement field access for product types
                Err(CodegenError::Compilation(
                    "Field access not yet implemented".to_string(),
                ))
            }

            ast::Expression::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                let cond_val = Self::compile_expression_static(
                    condition,
                    builder,
                    variables,
                    int_type,
                    float_type,
                    bool_type,
                    _pointer_type,
                )?;

                let then_block = builder.create_block();
                let else_block = builder.create_block();
                let merge_block = builder.create_block();

                // Add block parameter for the result
                let result_type = int_type; // TODO: Infer actual type
                builder.append_block_param(merge_block, result_type);

                builder
                    .ins()
                    .brif(cond_val, then_block, &[], else_block, &[]);

                // Then branch
                builder.switch_to_block(then_block);
                builder.seal_block(then_block);
                let then_val = Self::compile_expression_static(
                    then_expr,
                    builder,
                    variables,
                    int_type,
                    float_type,
                    bool_type,
                    _pointer_type,
                )?;
                builder.ins().jump(merge_block, &[then_val]);

                // Else branch
                builder.switch_to_block(else_block);
                builder.seal_block(else_block);
                let else_val = Self::compile_expression_static(
                    else_expr,
                    builder,
                    variables,
                    int_type,
                    float_type,
                    bool_type,
                    _pointer_type,
                )?;
                builder.ins().jump(merge_block, &[else_val]);

                // Merge block
                builder.switch_to_block(merge_block);
                builder.seal_block(merge_block);

                Ok(builder.block_params(merge_block)[0])
            }

            ast::Expression::Block {
                statements,
                expression,
            } => {
                let mut last_val = builder.ins().iconst(int_type, 0);

                for stmt in statements {
                    last_val = Self::compile_statement_static(
                        stmt,
                        builder,
                        variables,
                        int_type,
                        float_type,
                        bool_type,
                        _pointer_type,
                    )?;
                }

                if let Some(expr) = expression {
                    last_val = Self::compile_expression_static(
                        expr,
                        builder,
                        variables,
                        int_type,
                        float_type,
                        bool_type,
                        _pointer_type,
                    )?;
                }

                Ok(last_val)
            }

            ast::Expression::Lambda { params: _, body: _ } => {
                // TODO: Implement lambda compilation (complex - needs closure support)
                Err(CodegenError::Compilation(
                    "Lambda expressions not yet implemented".to_string(),
                ))
            }

            ast::Expression::List { elements: _ } => {
                // TODO: Implement list literals
                Err(CodegenError::Compilation(
                    "List literals not yet implemented".to_string(),
                ))
            }

            ast::Expression::Map { pairs: _ } => {
                // TODO: Implement map literals
                Err(CodegenError::Compilation(
                    "Map literals not yet implemented".to_string(),
                ))
            }

            ast::Expression::Constructor { name: _, args: _ } => {
                // TODO: Implement constructor calls
                Err(CodegenError::Compilation(
                    "Constructor calls not yet implemented".to_string(),
                ))
            }

            ast::Expression::Case { expr: _, arms: _ } => {
                // TODO: Implement pattern matching
                Err(CodegenError::Compilation(
                    "Pattern matching not yet implemented".to_string(),
                ))
            }

            ast::Expression::When { expr: _, arms: _ } => {
                // TODO: Implement when expressions
                Err(CodegenError::Compilation(
                    "When expressions not yet implemented".to_string(),
                ))
            }

            ast::Expression::MethodCall {
                object: _,
                method: _,
                args: _,
            } => {
                // TODO: Implement method calls
                Err(CodegenError::Compilation(
                    "Method calls not yet implemented".to_string(),
                ))
            }

            ast::Expression::DestructuringPattern { pattern: _, value } => {
                // For now, just compile the value
                Self::compile_expression_static(
                    value,
                    builder,
                    variables,
                    int_type,
                    float_type,
                    bool_type,
                    _pointer_type,
                )
            }
        }
    }

    fn compile_literal_static(
        lit: &ast::Literal,
        builder: &mut FunctionBuilder,
        int_type: types::Type,
        _float_type: types::Type,
        bool_type: types::Type,
    ) -> Result<Value, CodegenError> {
        match lit {
            ast::Literal::Integer(n) => Ok(builder.ins().iconst(int_type, *n)),
            ast::Literal::Float(f) => Ok(builder.ins().f64const(*f)),
            ast::Literal::Boolean(b) => Ok(builder.ins().iconst(bool_type, if *b { 1 } else { 0 })),
            ast::Literal::String(s) => {
                // Create a string literal by storing it in a static data section
                // This is a simplified approach - real implementation would use proper string interning
                let string_bytes = s.as_bytes();
                let mut data = Vec::with_capacity(string_bytes.len() + 1);
                data.extend_from_slice(string_bytes);
                data.push(0); // null terminator

                // For now, we'll encode the string data as immediate values
                // This is a hack - proper implementation would use data sections
                let string_ptr = builder.ins().iconst(int_type, string_bytes.as_ptr() as i64);
                Ok(string_ptr)
            }
        }
    }

    fn compile_binary_op_static(
        op: &ast::BinaryOp,
        left: Value,
        right: Value,
        builder: &mut FunctionBuilder,
    ) -> Result<Value, CodegenError> {
        match op {
            ast::BinaryOp::Add => Ok(builder.ins().iadd(left, right)),
            ast::BinaryOp::Sub => Ok(builder.ins().isub(left, right)),
            ast::BinaryOp::Mul => Ok(builder.ins().imul(left, right)),
            ast::BinaryOp::Div => Ok(builder.ins().sdiv(left, right)),
            ast::BinaryOp::Equal => Ok(builder.ins().icmp(IntCC::Equal, left, right)),
            ast::BinaryOp::NotEqual => Ok(builder.ins().icmp(IntCC::NotEqual, left, right)),
            ast::BinaryOp::Less => Ok(builder.ins().icmp(IntCC::SignedLessThan, left, right)),
            ast::BinaryOp::Greater => Ok(builder.ins().icmp(IntCC::SignedGreaterThan, left, right)),
            ast::BinaryOp::LessEqual => {
                Ok(builder
                    .ins()
                    .icmp(IntCC::SignedLessThanOrEqual, left, right))
            }
            ast::BinaryOp::GreaterEqual => {
                Ok(builder
                    .ins()
                    .icmp(IntCC::SignedGreaterThanOrEqual, left, right))
            }
        }
    }

    fn get_cranelift_type_static(
        keen_type: &Option<ast::Type>,
        int_type: types::Type,
        float_type: types::Type,
        bool_type: types::Type,
        pointer_type: types::Type,
    ) -> Result<types::Type, CodegenError> {
        match keen_type {
            Some(ast::Type::Named(name)) => match name.as_str() {
                "Int" => Ok(int_type),
                "Float" => Ok(float_type),
                "Boolean" => Ok(bool_type),
                "String" => Ok(pointer_type), // String as pointer for now
                _ => Err(CodegenError::Type(format!("Unknown type: {}", name))),
            },
            Some(ast::Type::Function {
                params: _,
                return_type: _,
            }) => {
                // Function types are pointers to functions
                Ok(pointer_type)
            }
            Some(ast::Type::Product { name: _, fields: _ }) => {
                // Product types are pointers to structs for now
                Ok(pointer_type)
            }
            Some(ast::Type::Union {
                name: _,
                variants: _,
            }) => {
                // Union types are pointers for now
                Ok(pointer_type)
            }
            None => Ok(int_type), // Default to int for type inference
        }
    }

    fn compile_print_call_static(
        args: &[ast::Expression],
        builder: &mut FunctionBuilder,
        variables: &mut HashMap<String, Variable>,
        int_type: types::Type,
        float_type: types::Type,
        bool_type: types::Type,
        pointer_type: types::Type,
    ) -> Result<Value, CodegenError> {
        if args.len() != 1 {
            return Err(CodegenError::Compilation(
                "print function expects exactly one argument".to_string(),
            ));
        }

        let arg_val = Self::compile_expression_static(
            &args[0],
            builder,
            variables,
            int_type,
            float_type,
            bool_type,
            pointer_type,
        )?;

        // For now, just return the argument value
        // TODO: Implement actual print function calls when runtime integration is complete
        // This demonstrates that we can compile print statements and execute the expressions
        Ok(arg_val)
    }

    fn get_cranelift_type(
        &self,
        keen_type: &Option<ast::Type>,
    ) -> Result<types::Type, CodegenError> {
        match keen_type {
            Some(ast::Type::Named(name)) => match name.as_str() {
                "Int" => Ok(self.int_type),
                "Float" => Ok(self.float_type),
                "Boolean" => Ok(self.bool_type),
                "String" => Ok(self.pointer_type), // String as pointer for now
                _ => Err(CodegenError::Type(format!("Unknown type: {}", name))),
            },
            Some(ast::Type::Function {
                params: _,
                return_type: _,
            }) => {
                // Function types are pointers to functions
                Ok(self.pointer_type)
            }
            Some(ast::Type::Product { name: _, fields: _ }) => {
                // Product types are pointers to structs for now
                Ok(self.pointer_type)
            }
            Some(ast::Type::Union {
                name: _,
                variants: _,
            }) => {
                // Union types are pointers for now
                Ok(self.pointer_type)
            }
            None => Ok(self.int_type), // Default to int for type inference
        }
    }

    pub fn finalize(&mut self) -> Result<(), CodegenError> {
        self.module
            .finalize_definitions()
            .map_err(|e| CodegenError::Module(format!("Failed to finalize: {}", e)))
    }

    pub fn get_function_ptr(&mut self, name: &str) -> Result<*const u8, CodegenError> {
        let func_id = self
            .module
            .get_name(name)
            .ok_or_else(|| CodegenError::Runtime(format!("Function not found: {}", name)))?;

        match func_id {
            cranelift_module::FuncOrDataId::Func(id) => {
                let func_ptr = self.module.get_finalized_function(id);
                Ok(func_ptr)
            }
            _ => Err(CodegenError::Runtime(format!(
                "Expected function, found data: {}",
                name
            ))),
        }
    }
}

// Utility function to execute a simple function
pub fn execute_function_i64(ptr: *const u8) -> i64 {
    let func: extern "C" fn() -> i64 = unsafe { std::mem::transmute(ptr) };
    func()
}

pub fn execute_function_i64_with_args(ptr: *const u8, args: &[i64]) -> i64 {
    match args.len() {
        0 => {
            let func: extern "C" fn() -> i64 = unsafe { std::mem::transmute(ptr) };
            func()
        }
        1 => {
            let func: extern "C" fn(i64) -> i64 = unsafe { std::mem::transmute(ptr) };
            func(args[0])
        }
        2 => {
            let func: extern "C" fn(i64, i64) -> i64 = unsafe { std::mem::transmute(ptr) };
            func(args[0], args[1])
        }
        _ => {
            // TODO: Handle more arguments
            panic!("Too many arguments for function call");
        }
    }
}
