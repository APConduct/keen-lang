use crate::ast::{
    BinaryOp, Expression, Function, FunctionBody, Item, Literal, Mutability, Parameter, Pattern,
    ProductField, Program, Statement, Type, TypeDef, UnionVariant, VariableDecl,
};
use crate::lexer::Token;
use crate::manual_parser::ManualParser;
use chumsky::prelude::*;

pub fn parser() -> impl Parser<Token, Program, Error = Simple<Token>> {
    let program = item_parser()
        .repeated()
        .then_ignore(end())
        .map(|items| Program { items });

    program
}

// Helper enum for chaining operations
#[derive(Debug, Clone)]
enum ChainOp {
    MethodCall {
        method: String,
        args: Vec<Expression>,
    },
    FieldAccess {
        field: String,
    },
}

fn item_parser() -> impl Parser<Token, Item, Error = Simple<Token>> {
    choice((
        function_parser(),
        type_def_parser(),
        destructuring_assignment_parser(),
        variable_decl_parser().map(Item::VariableDecl),
    ))
}

fn function_parser() -> impl Parser<Token, Item, Error = Simple<Token>> {
    let ident = select! { Token::Identifier(name) => name };

    let mutability = choice((
        just(Token::Live).to(Mutability::Live),
        just(Token::Keep).to(Mutability::Keep),
    ))
    .or_not();

    let param = mutability
        .then(ident.clone())
        .then(just(Token::Colon).ignore_then(type_parser()).or_not())
        .map(|((mutability, name), type_annotation)| Parameter {
            name,
            type_annotation,
            mutability,
        });

    let params = param
        .separated_by(just(Token::Comma))
        .delimited_by(just(Token::LeftParen), just(Token::RightParen));

    let return_type = just(Token::Colon).ignore_then(type_parser()).or_not();

    let block_body = statement_parser()
        .repeated()
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
        .map(FunctionBody::Block);

    let expr_body = just(Token::Assign)
        .ignore_then(expression_parser())
        .map(FunctionBody::Expression);

    let body = choice((block_body, expr_body));

    ident
        .then(params)
        .then(return_type)
        .then(body)
        .map(|(((name, params), return_type), body)| {
            Item::Function(Function {
                name,
                params,
                return_type,
                body,
            })
        })
}

fn type_def_parser() -> impl Parser<Token, Item, Error = Simple<Token>> {
    let ident = select! { Token::Identifier(name) => name };

    // Product field: name: Type
    let product_field = || {
        ident
            .clone()
            .then_ignore(just(Token::Colon))
            .then(type_parser())
            .map(|(name, field_type)| ProductField { name, field_type })
    };

    // Union variant: Name(field1: Type, field2: Type)
    let union_variant = ident
        .clone()
        .then(
            product_field()
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .or_not()
                .map(|fields| fields.unwrap_or_default()),
        )
        .map(|(name, fields)| UnionVariant { name, fields });

    // Product type: type Name = Name(field1: Type, field2: Type)
    let product_type = just(Token::Type)
        .ignore_then(ident.clone())
        .then_ignore(just(Token::Assign))
        .then(ident.clone())
        .then(
            product_field()
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
        )
        .map(|((name, _constructor_name), fields)| {
            Item::TypeDef(TypeDef::Product { name, fields })
        });

    // Union type: type Name = Variant1(fields) | Variant2(fields)
    let union_type = just(Token::Type)
        .ignore_then(ident.clone())
        .then_ignore(just(Token::Assign))
        .then(union_variant.separated_by(just(Token::Pipe)))
        .map(|(name, variants)| Item::TypeDef(TypeDef::Union { name, variants }));

    // Type alias: type Name = [distinct] Type
    let alias_type = {
        let distinct = just(Token::Distinct).or_not().map(|d| d.is_some());

        just(Token::Type)
            .ignore_then(ident)
            .then(just(Token::Assign).ignore_then(distinct))
            .then(type_parser())
            .map(|((name, is_distinct), underlying_type)| {
                Item::TypeDef(TypeDef::Alias {
                    name,
                    is_distinct,
                    underlying_type,
                })
            })
    };

    choice((union_type, product_type, alias_type))
}

fn variable_decl_parser() -> impl Parser<Token, VariableDecl, Error = Simple<Token>> {
    let ident = select! { Token::Identifier(name) => name };

    let mutability = choice((
        just(Token::Live).to(Mutability::Live),
        just(Token::Keep).to(Mutability::Keep),
    ))
    .or_not()
    .map(|m| m.unwrap_or(Mutability::Immutable));

    // Support both "name = value" and "name: Type = value" syntax
    choice((
        // name: Type = value
        mutability
            .clone()
            .then(ident.clone())
            .then_ignore(just(Token::Colon))
            .then(type_parser())
            .then_ignore(just(Token::Assign))
            .then(expression_parser())
            .map(
                |(((mutability, name), type_annotation), value)| VariableDecl {
                    name,
                    mutability,
                    type_annotation: Some(type_annotation),
                    value,
                },
            ),
        // name = value
        mutability
            .then(ident)
            .then_ignore(just(Token::Assign))
            .then(expression_parser())
            .map(|((mutability, name), value)| VariableDecl {
                name,
                mutability,
                type_annotation: None,
                value,
            }),
    ))
}

fn type_parser() -> impl Parser<Token, Type, Error = Simple<Token>> {
    recursive(|type_parser| {
        let ident = select! { Token::Identifier(name) => name };
        let named_type = ident.map(Type::Named);

        // Function type: (param_types) -> return_type
        let function_type = type_parser
            .clone()
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            .then_ignore(just(Token::Arrow))
            .then(type_parser)
            .map(|(params, return_type)| Type::Function {
                params,
                return_type: Box::new(return_type),
            });

        choice((function_type, named_type))
    })
}

fn statement_parser() -> impl Parser<Token, Statement, Error = Simple<Token>> {
    choice((
        variable_decl_parser().map(Statement::VariableDecl),
        destructuring_statement_parser(),
        expression_parser().map(Statement::Expression),
    ))
}

fn destructuring_assignment_parser() -> impl Parser<Token, Item, Error = Simple<Token>> {
    pattern_parser()
        .then_ignore(just(Token::Assign))
        .then(expression_parser())
        .map(|(pattern, value)| {
            Item::VariableDecl(VariableDecl {
                name: extract_pattern_name(&pattern),
                mutability: Mutability::Immutable,
                type_annotation: None,
                value: Expression::DestructuringPattern {
                    pattern,
                    value: Box::new(value),
                },
            })
        })
}

fn destructuring_statement_parser() -> impl Parser<Token, Statement, Error = Simple<Token>> {
    pattern_parser()
        .then_ignore(just(Token::Assign))
        .then(expression_parser())
        .map(|(pattern, value)| Statement::DestructuringDecl { pattern, value })
}

fn extract_pattern_name(pattern: &Pattern) -> String {
    match pattern {
        Pattern::Identifier(name) => name.clone(),
        Pattern::Constructor { name, .. } => format!("destructured_{}", name),
        _ => "destructured".to_string(),
    }
}

fn pattern_parser() -> impl Parser<Token, Pattern, Error = Simple<Token>> {
    recursive(|pattern| {
        let ident = select! { Token::Identifier(name) => name };

        choice((
            // Constructor pattern: Name(p1, p2, ...)
            ident
                .clone()
                .then(
                    pattern
                        .clone()
                        .separated_by(just(Token::Comma))
                        .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                        .or_not()
                        .map(|args| args.unwrap_or_default()),
                )
                .map(|(name, args)| Pattern::Constructor { name, args }),
            // Identifier pattern
            ident.map(Pattern::Identifier),
            // Wildcard pattern
            just(Token::Underscore).to(Pattern::Wildcard),
            // Literal patterns
            choice((
                select! { Token::Integer(n) => Literal::Integer(n) },
                select! { Token::Float(f) => Literal::Float(f) },
                select! { Token::String(s) => Literal::String(s) },
                just(Token::True).to(Literal::Boolean(true)),
                just(Token::False).to(Literal::Boolean(false)),
            ))
            .map(Pattern::Literal),
        ))
    })
}

fn expression_parser() -> impl Parser<Token, Expression, Error = Simple<Token>> {
    recursive(|expr| {
        let literal = choice((
            select! { Token::Integer(n) => Literal::Integer(n) },
            select! { Token::Float(f) => Literal::Float(f) },
            select! { Token::String(s) => Literal::String(s) },
            just(Token::True).to(Literal::Boolean(true)),
            just(Token::False).to(Literal::Boolean(false)),
        ))
        .map(Expression::Literal);

        let ident = select! { Token::Identifier(name) => Expression::Identifier(name) };

        let atom = choice((
            literal,
            ident,
            expr.clone()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
        ));

        // Function calls
        let call = atom.then(
            expr.clone()
                .separated_by(just(Token::Comma))
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .repeated(),
        );

        let call_expr = call.map(|(base, call_lists)| {
            call_lists
                .into_iter()
                .fold(base, |acc, args| Expression::Call {
                    function: Box::new(acc),
                    args,
                })
        });

        // Field access and method chaining
        let field_and_method = call_expr.then(
            choice((
                // Method call: .method(args)
                just(Token::Dot)
                    .ignore_then(select! { Token::Identifier(method) => method })
                    .then(
                        expr.clone()
                            .separated_by(just(Token::Comma))
                            .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
                    )
                    .map(|(method, args)| ChainOp::MethodCall { method, args }),
                // Field access: .field
                just(Token::Dot)
                    .ignore_then(select! { Token::Identifier(field) => field })
                    .map(|field| ChainOp::FieldAccess { field }),
            ))
            .repeated(),
        );

        let field_expr = field_and_method.map(|(base, chains)| {
            chains
                .into_iter()
                .fold(base, |acc, chain_op| match chain_op {
                    ChainOp::MethodCall { method, args } => Expression::MethodCall {
                        object: Box::new(acc),
                        method,
                        args,
                    },
                    ChainOp::FieldAccess { field } => Expression::FieldAccess {
                        object: Box::new(acc),
                        field,
                    },
                })
        });

        // Binary operations with precedence
        let factor = field_expr;

        let term = factor
            .clone()
            .then(
                choice((
                    just(Token::Multiply).to(BinaryOp::Mul),
                    just(Token::Divide).to(BinaryOp::Div),
                    just(Token::Modulo).to(BinaryOp::Mod),
                ))
                .then(factor)
                .repeated(),
            )
            .foldl(|left, (op, right)| Expression::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            });

        let sum = term
            .clone()
            .then(
                choice((
                    just(Token::Plus).to(BinaryOp::Add),
                    just(Token::Minus).to(BinaryOp::Sub),
                ))
                .then(term)
                .repeated(),
            )
            .foldl(|left, (op, right)| Expression::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            });

        let comparison = sum
            .clone()
            .then(
                choice((
                    just(Token::Equal).to(BinaryOp::Equal),
                    just(Token::NotEqual).to(BinaryOp::NotEqual),
                    just(Token::LessEqual).to(BinaryOp::LessEqual),
                    just(Token::GreaterEqual).to(BinaryOp::GreaterEqual),
                    just(Token::Less).to(BinaryOp::Less),
                    just(Token::Greater).to(BinaryOp::Greater),
                ))
                .then(sum)
                .repeated(),
            )
            .foldl(|left, (op, right)| Expression::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            });

        // Pipeline operator: expr |> function
        let pipeline = comparison
            .clone()
            .then(just(Token::Pipeline).ignore_then(comparison).repeated())
            .foldl(|left, right| Expression::Call {
                function: Box::new(right),
                args: vec![left],
            });

        pipeline
    })
}

// Hybrid parser that integrates manual parsing for complex expressions
pub fn parse_with_manual_fallback(tokens: Vec<Token>) -> Result<Program, Vec<String>> {
    // First, detect if we have case or when expressions
    if has_complex_expressions(&tokens) {
        parse_hybrid(tokens)
    } else {
        // Use regular chumsky parser
        let chumsky_parser = parser();
        chumsky_parser
            .parse(tokens)
            .map_err(|errors| errors.into_iter().map(|e| format!("{:?}", e)).collect())
    }
}

fn has_complex_expressions(tokens: &[Token]) -> bool {
    // Look for specific patterns that require the manual parser
    let mut i = 0;
    while i < tokens.len() {
        match &tokens[i] {
            // These always need manual parser
            Token::Case | Token::When => return true,

            // String interpolation needs manual parser
            Token::String(s) if crate::lexer::has_interpolation(s) => return true,

            // Ternary operator needs manual parser
            Token::Question => return true,

            // Lambda expressions need manual parser (|param| expr)
            Token::Pipe => {
                // Check if this looks like a lambda: |...|
                if let Some(_pipe_end) = find_matching_pipe(&tokens, i) {
                    return true;
                }
            }

            // Pipeline operator needs manual parser
            Token::Pipeline => return true,

            // Only consider LeftBrace complex if it's NOT part of a function body
            Token::LeftBrace => {
                // Check if this is a function definition pattern
                if is_function_block_body(&tokens, i) {
                    // This is a function body, let the regular parser handle it
                } else {
                    // This is a standalone block expression, needs manual parser
                    return true;
                }
            }

            // Check for = { pattern which should always use manual parser
            Token::Assign => {
                if i + 1 < tokens.len() {
                    if let Some(Token::LeftBrace) = tokens.get(i + 1) {
                        return true; // Force manual parser for = { patterns
                    }
                }
            }

            // Check for function patterns that use blocks
            Token::RightParen => {
                if i + 1 < tokens.len() && i + 2 < tokens.len() {
                    // Pattern: ) = {
                    if matches!(tokens.get(i + 1), Some(Token::Assign))
                        && matches!(tokens.get(i + 2), Some(Token::LeftBrace))
                    {
                        return true; // Force manual parser for function block syntax
                    }
                }
            }

            _ => {}
        }
        i += 1;
    }

    // Check for constructor expressions
    if tokens.iter().any(|_| has_constructor_expression(tokens)) {
        return true;
    }
    false
}

// Helper function to detect if a LeftBrace is part of a function definition
fn is_function_block_body(tokens: &[Token], brace_pos: usize) -> bool {
    // Pattern 1: identifier ( params ) { ... } (function block body)
    if brace_pos >= 3 {
        if let (Some(Token::Identifier(_)), Some(Token::LeftParen), Some(Token::RightParen)) = (
            tokens.get(brace_pos - 3),
            tokens.get(brace_pos - 2),
            tokens.get(brace_pos - 1),
        ) {
            return true;
        }
    }

    // Pattern 2: identifier ( ... params ... ) { ... } (function with parameters)
    if brace_pos >= 2 {
        if let Some(Token::RightParen) = tokens.get(brace_pos - 1) {
            // Look backwards for matching LeftParen
            let mut paren_depth = 1;
            let mut i = brace_pos - 2;
            loop {
                if i == 0 {
                    break;
                }
                match tokens.get(i) {
                    Some(Token::RightParen) => paren_depth += 1,
                    Some(Token::LeftParen) => {
                        paren_depth -= 1;
                        if paren_depth == 0 {
                            // Found matching paren, check if preceded by identifier
                            if i > 0 {
                                if let Some(Token::Identifier(_)) = tokens.get(i - 1) {
                                    return true;
                                }
                            }
                            break;
                        }
                    }
                    _ => {}
                }
                i -= 1;
            }
        }
    }

    // Pattern 3: = { ... } (expression body with block) - ALWAYS triggers manual parser
    if brace_pos > 0 {
        if let Some(Token::Assign) = tokens.get(brace_pos - 1) {
            return false; // Force manual parser for block expressions
        }
    }

    false
}

// Helper function to find matching pipe for lambda detection
fn find_matching_pipe(tokens: &[Token], start: usize) -> Option<usize> {
    for i in (start + 1)..tokens.len() {
        match &tokens[i] {
            Token::Pipe => return Some(i),
            Token::LeftParen | Token::RightParen | Token::LeftBrace | Token::RightBrace => {
                return None
            }
            _ => continue,
        }
    }
    None
}

fn parse_hybrid(tokens: Vec<Token>) -> Result<Program, Vec<String>> {
    let mut items = Vec::new();
    let mut i = 0;

    while i < tokens.len() {
        // Try to parse an item
        match parse_item_hybrid(&tokens, &mut i) {
            Ok(item) => items.push(item),
            Err(e) => return Err(vec![e]),
        }
    }

    Ok(Program { items })
}

fn parse_item_hybrid(tokens: &[Token], position: &mut usize) -> Result<Item, String> {
    let start = *position;

    // Find the end of this item (look for next top-level construct or end)
    let end = find_item_end(tokens, start);

    // Prevent infinite loops
    if end <= start {
        return Err("Could not find item end".to_string());
    }

    let item_tokens = tokens[start..end].to_vec();
    *position = end; // Update position immediately

    // Check the type of item first
    let item_type = detect_item_type(&item_tokens, 0);

    match item_type {
        ItemType::Function => {
            // Check if ANY function in the entire file has complex expressions
            // If so, use manual parser for ALL functions to avoid conflicts
            let file_has_complex = has_complex_expressions(tokens);

            if file_has_complex
                || has_string_interpolation(&item_tokens)
                || has_block_or_lambda(&item_tokens)
            {
                parse_item_with_manual_parser(item_tokens)
            } else {
                // Simple functions use chumsky parser
                let chumsky_parser = parser();
                match chumsky_parser.parse(item_tokens) {
                    Ok(mut program) => {
                        if let Some(item) = program.items.pop() {
                            Ok(item)
                        } else {
                            Err("Failed to parse item".to_string())
                        }
                    }
                    Err(_) => Err("Failed to parse item with chumsky".to_string()),
                }
            }
        }
        ItemType::Variable => {
            // Variables with complex expressions use manual parser
            if is_variable_decl_with_complex_expr(&item_tokens) {
                parse_variable_with_complex_expr(item_tokens)
            } else {
                // Simple variables use chumsky parser
                let chumsky_parser = parser();
                match chumsky_parser.parse(item_tokens) {
                    Ok(mut program) => {
                        if let Some(item) = program.items.pop() {
                            Ok(item)
                        } else {
                            Err("Failed to parse item".to_string())
                        }
                    }
                    Err(_) => Err("Failed to parse item with chumsky".to_string()),
                }
            }
        }
        _ => {
            // Type definitions and unknown items use chumsky parser
            let chumsky_parser = parser();
            match chumsky_parser.parse(item_tokens) {
                Ok(mut program) => {
                    if let Some(item) = program.items.pop() {
                        Ok(item)
                    } else {
                        Err("Failed to parse item".to_string())
                    }
                }
                Err(_) => Err("Failed to parse item with chumsky".to_string()),
            }
        }
    }
}

fn find_item_end(tokens: &[Token], start: usize) -> usize {
    if start >= tokens.len() {
        return tokens.len();
    }

    let mut i = start;
    let mut brace_depth = 0;
    let mut paren_depth = 0;

    // Detect what kind of item this is
    let item_type = detect_item_type(tokens, start);

    match item_type {
        ItemType::Function => find_function_end(tokens, start),
        ItemType::TypeDef => find_type_def_end(tokens, start),
        ItemType::Variable => find_variable_end(tokens, start),
        ItemType::Unknown => {
            // Fallback: try to find next top-level identifier
            i = start + 1;
            while i < tokens.len() {
                if matches!(tokens[i], Token::Identifier(_)) {
                    // Check if this looks like the start of a new item
                    if is_new_item_start(tokens, i) {
                        return i;
                    }
                }
                i += 1;
            }
            tokens.len()
        }
    }
}

#[derive(Debug)]
enum ItemType {
    Function,
    TypeDef,
    Variable,
    Unknown,
}

fn detect_item_type(tokens: &[Token], start: usize) -> ItemType {
    if start >= tokens.len() {
        return ItemType::Unknown;
    }

    // Type definition: "type Name = ..."
    if matches!(tokens.get(start), Some(Token::Type)) {
        return ItemType::TypeDef;
    }

    // Function definition: "name(...) ..." or "name(...): Type ..."
    if let Some(Token::Identifier(_)) = tokens.get(start) {
        if let Some(Token::LeftParen) = tokens.get(start + 1) {
            return ItemType::Function;
        }
    }

    // Variable declaration with type: "name: Type = ..."
    if let Some(Token::Identifier(_)) = tokens.get(start) {
        if let Some(Token::Colon) = tokens.get(start + 1) {
            // Look for assignment after type annotation
            let mut i = start + 2;
            while i < tokens.len() && !matches!(tokens[i], Token::Assign) {
                i += 1;
            }
            if i < tokens.len() && matches!(tokens[i], Token::Assign) {
                return ItemType::Variable;
            }
        }
    }

    // Simple variable declaration: "name = ..."
    if let Some(Token::Identifier(_)) = tokens.get(start) {
        if let Some(Token::Assign) = tokens.get(start + 1) {
            return ItemType::Variable;
        }
    }

    ItemType::Unknown
}

fn find_function_end(tokens: &[Token], start: usize) -> usize {
    let mut i = start;
    let mut paren_depth = 0;
    let mut brace_depth = 0;

    // Skip function name and parameters
    while i < tokens.len() {
        match &tokens[i] {
            Token::LeftParen => paren_depth += 1,
            Token::RightParen => {
                paren_depth -= 1;
                if paren_depth == 0 {
                    i += 1; // Move past the closing paren
                    break;
                }
            }
            _ => {}
        }
        i += 1;
    }

    // Skip optional return type annotation
    if i < tokens.len() && matches!(tokens[i], Token::Colon) {
        i += 1; // Skip ':'
        while i < tokens.len() && !matches!(tokens[i], Token::Assign | Token::LeftBrace) {
            i += 1;
        }
    }

    // Handle function body
    if i < tokens.len() {
        match &tokens[i] {
            Token::Assign => {
                // Expression body: name() = expr
                i += 1;
                find_expression_end(tokens, i)
            }
            Token::LeftBrace => {
                // Block body: name() { ... }
                brace_depth = 1;
                i += 1;
                while i < tokens.len() && brace_depth > 0 {
                    match &tokens[i] {
                        Token::LeftBrace => brace_depth += 1,
                        Token::RightBrace => brace_depth -= 1,
                        _ => {}
                    }
                    i += 1;
                }
                i // Already includes the closing brace
            }
            _ => i,
        }
    } else {
        i
    }
}

fn find_type_def_end(tokens: &[Token], start: usize) -> usize {
    let mut i = start;
    let mut brace_depth = 0;
    let mut paren_depth = 0;

    // Skip "type Name ="
    while i < tokens.len() && !matches!(tokens[i], Token::Assign) {
        i += 1;
    }
    if i < tokens.len() {
        i += 1; // Skip '='
    }

    // Find end of type definition
    while i < tokens.len() {
        match &tokens[i] {
            Token::LeftBrace => brace_depth += 1,
            Token::RightBrace => {
                brace_depth -= 1;
                if brace_depth == 0 {
                    return i + 1;
                }
            }
            Token::LeftParen => paren_depth += 1,
            Token::RightParen => paren_depth -= 1,
            Token::Pipe if brace_depth == 0 && paren_depth == 0 => {
                // Union type continues
            }
            _ if brace_depth == 0 && paren_depth == 0 => {
                // Check if we've hit the next item
                if is_new_item_start(tokens, i) {
                    return i;
                }
            }
            _ => {}
        }
        i += 1;
    }
    tokens.len()
}

fn find_variable_end(tokens: &[Token], start: usize) -> usize {
    let mut i = start;
    let mut paren_depth = 0;
    let mut brace_depth = 0;
    let mut bracket_depth = 0;
    let mut question_count = 0; // Track ternary operators

    // Skip to assignment
    while i < tokens.len() && !matches!(tokens[i], Token::Assign) {
        i += 1;
    }
    if i < tokens.len() {
        i += 1; // Skip '='
    }

    // Find end of variable expression more carefully
    while i < tokens.len() {
        match &tokens[i] {
            Token::LeftParen => paren_depth += 1,
            Token::RightParen => paren_depth -= 1,
            Token::LeftBrace => brace_depth += 1,
            Token::RightBrace => {
                brace_depth -= 1;
                if brace_depth == 0 {
                    return i + 1;
                }
            }
            Token::LeftBracket => bracket_depth += 1,
            Token::RightBracket => bracket_depth -= 1,
            Token::Question => question_count += 1, // Count ternary operators
            Token::Colon if question_count > 0 => question_count -= 1, // Match ternary colons
            Token::Identifier(_) if paren_depth == 0 && brace_depth == 0 && bracket_depth == 0 => {
                // At top level, check if this looks like a real new item
                if i + 1 < tokens.len() {
                    match (&tokens[i], tokens.get(i + 1)) {
                        // Function definition: name(
                        (Token::Identifier(_), Some(Token::LeftParen)) => {
                            // Check if we're already past the assignment in a variable declaration
                            let mut assignment_pos = None;
                            for j in start..i {
                                if matches!(tokens[j], Token::Assign) {
                                    assignment_pos = Some(j);
                                    break;
                                }
                            }

                            // If we found an assignment and we're past it, this is a new item
                            if let Some(assign_pos) = assignment_pos {
                                if i > assign_pos {
                                    return i;
                                }
                            } else {
                                // No assignment found, this is a new function
                                return i;
                            }
                        }
                        // Variable with type: name: (but not if we're in a ternary expression)
                        (Token::Identifier(_), Some(Token::Colon)) if question_count == 0 => {
                            // Check if we're already past an assignment (would be a new variable)
                            let mut found_assignment = false;
                            for j in start..i {
                                if matches!(tokens[j], Token::Assign) {
                                    found_assignment = true;
                                    break;
                                }
                            }
                            if found_assignment {
                                return i;
                            }
                        }
                        // Simple variable: name =
                        (Token::Identifier(_), Some(Token::Assign)) => {
                            // Check if we're already past an assignment (would be a new variable)
                            let mut found_assignment = false;
                            for j in start..i {
                                if matches!(tokens[j], Token::Assign) {
                                    found_assignment = true;
                                    break;
                                }
                            }
                            if found_assignment {
                                return i;
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
        i += 1;
    }
    tokens.len()
}

fn find_expression_end(tokens: &[Token], start: usize) -> usize {
    let mut i = start;
    let mut brace_depth = 0;
    let mut paren_depth = 0;
    let mut bracket_depth = 0;

    while i < tokens.len() {
        match &tokens[i] {
            Token::LeftBrace => brace_depth += 1,
            Token::RightBrace => {
                brace_depth -= 1;
                if brace_depth == 0 {
                    return i + 1;
                }
            }
            Token::LeftParen => paren_depth += 1,
            Token::RightParen => paren_depth -= 1,
            Token::LeftBracket => bracket_depth += 1,
            Token::RightBracket => bracket_depth -= 1,
            _ if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 => {
                // At top level, check if we've hit a new item
                if is_new_item_start(tokens, i) {
                    return i;
                }
            }
            _ => {}
        }
        i += 1;
    }
    tokens.len()
}

fn is_new_item_start(tokens: &[Token], pos: usize) -> bool {
    if pos >= tokens.len() {
        return false;
    }

    match &tokens[pos] {
        Token::Type => true,
        Token::Identifier(_) => {
            // Check for function: name(
            if let Some(Token::LeftParen) = tokens.get(pos + 1) {
                return true;
            }
            // Check for variable with type: name:
            if let Some(Token::Colon) = tokens.get(pos + 1) {
                return true;
            }
            // Check for simple variable: name =
            if let Some(Token::Assign) = tokens.get(pos + 1) {
                return true;
            }
            false
        }
        _ => false,
    }
}

fn is_variable_decl_with_complex_expr(tokens: &[Token]) -> bool {
    // Check if this is a variable declaration (identifier = ...)
    if tokens.len() >= 3 {
        // Handle both "name = value" and "name: Type = value" patterns
        let assignment_pos = tokens.iter().position(|t| matches!(t, Token::Assign));

        if let Some(assign_idx) = assignment_pos {
            if assign_idx > 0 && matches!(tokens.get(0), Some(Token::Identifier(_))) {
                // Check if the expression after = contains complex features
                let expr_tokens = &tokens[(assign_idx + 1)..];

                let has_basic_complex = expr_tokens.iter().any(|t| {
                    matches!(
                        t,
                        Token::Case
                            | Token::When
                            | Token::Question
                            | Token::Pipeline
                            | Token::Pipe
                            | Token::LeftBrace
                            | Token::LeftBracket
                    ) || match t {
                        Token::String(s) => crate::lexer::has_interpolation(s),
                        _ => false,
                    }
                });

                let has_constructor = has_constructor_expression(expr_tokens);

                return has_basic_complex || has_constructor;
            }
        }
    }
    false
}

fn has_constructor_expression(tokens: &[Token]) -> bool {
    // Look for pattern: Identifier(name: value, ...)
    for i in 0..tokens.len().saturating_sub(2) {
        if let (Some(Token::Identifier(_)), Some(Token::LeftParen)) =
            (tokens.get(i), tokens.get(i + 1))
        {
            // Look for named arguments (name: value pattern)
            let mut j = i + 2;
            let mut paren_depth = 1;
            while j < tokens.len() && paren_depth > 0 {
                match &tokens[j] {
                    Token::LeftParen => paren_depth += 1,
                    Token::RightParen => paren_depth -= 1,
                    Token::Identifier(_) if paren_depth == 1 => {
                        // Check if followed by colon (named argument)
                        if let Some(Token::Colon) = tokens.get(j + 1) {
                            return true;
                        }
                    }
                    _ => {}
                }
                j += 1;
            }
        }
    }
    false
}

fn has_block_or_lambda(tokens: &[Token]) -> bool {
    tokens
        .iter()
        .any(|token| matches!(token, Token::LeftBrace | Token::Pipe | Token::Pipeline))
}

fn has_string_interpolation(tokens: &[Token]) -> bool {
    use crate::lexer::has_interpolation;
    tokens.iter().any(|token| match token {
        Token::String(s) => has_interpolation(s),
        _ => false,
    })
}

fn parse_item_with_manual_parser(tokens: Vec<Token>) -> Result<Item, String> {
    // Check if this is a function definition: name(...) = expr or name(...) { ... }
    if is_function_definition(&tokens) {
        return parse_function_with_manual_parser(tokens);
    }

    // Check if this is a variable declaration pattern: name = complex_expr or name: Type = complex_expr
    if tokens.len() >= 3 {
        let name = if let Some(Token::Identifier(n)) = tokens.get(0) {
            n.clone()
        } else {
            return Err("Expected identifier at start".to_string());
        };

        // Find the assignment position
        let assignment_pos = tokens.iter().position(|t| matches!(t, Token::Assign));

        if let Some(assign_idx) = assignment_pos {
            let expr_tokens = tokens[(assign_idx + 1)..].to_vec();

            let mut manual_parser = ManualParser::new(expr_tokens);
            match manual_parser.parse_expression() {
                Ok(expr) => {
                    return Ok(Item::VariableDecl(VariableDecl {
                        name,
                        mutability: Mutability::Immutable,
                        type_annotation: None,
                        value: expr,
                    }));
                }
                Err(_e) => {}
            }
        }
    }

    Err("Failed to parse with manual parser".to_string())
}

fn is_function_definition(tokens: &[Token]) -> bool {
    if tokens.len() < 2 {
        return false;
    }

    if let Some(Token::Identifier(_)) = tokens.get(0) {
        if let Some(Token::LeftParen) = tokens.get(1) {
            return true;
        }
    }
    false
}

fn parse_function_with_manual_parser(tokens: Vec<Token>) -> Result<Item, String> {
    // Extract function name
    let name = if let Some(Token::Identifier(n)) = tokens.get(0) {
        n.clone()
    } else {
        return Err("Expected function name".to_string());
    };

    // Find parameters (simplified - just extract parameter names)
    let mut params = Vec::new();
    let mut i = 2; // Skip name and LeftParen
    while i < tokens.len() {
        if let Some(Token::RightParen) = tokens.get(i) {
            i += 1;
            break;
        }
        if let Some(Token::Identifier(param_name)) = tokens.get(i) {
            params.push(Parameter {
                name: param_name.clone(),
                type_annotation: None,
                mutability: None,
            });
        }
        i += 1;
    }

    // Skip optional return type annotation
    if i < tokens.len() && matches!(tokens.get(i), Some(Token::Colon)) {
        while i < tokens.len() && !matches!(tokens.get(i), Some(Token::Assign | Token::LeftBrace)) {
            i += 1;
        }
    }

    // Parse function body
    let body = if i < tokens.len() {
        match tokens.get(i) {
            Some(Token::Assign) => {
                // Expression body: parse the expression after =
                let expr_tokens = tokens[(i + 1)..].to_vec();
                let mut manual_parser = ManualParser::new(expr_tokens);
                match manual_parser.parse_expression() {
                    Ok(expr) => FunctionBody::Expression(expr),
                    Err(e) => {
                        return Err(format!(
                            "Failed to parse function expression: {}",
                            e.message
                        ))
                    }
                }
            }
            Some(Token::LeftBrace) => {
                // Block body: parse statements inside {}
                let block_tokens = tokens[(i + 1)..(tokens.len() - 1)].to_vec(); // Remove { and }
                let mut statements = Vec::new();

                // Simple statement parsing - just treat each assignment as a statement
                let mut j = 0;
                while j < block_tokens.len() {
                    if let Some(Token::Identifier(var_name)) = block_tokens.get(j) {
                        if let Some(Token::Assign) = block_tokens.get(j + 1) {
                            // Find end of this statement
                            let mut stmt_end = j + 2;
                            let mut depth = 0;
                            while stmt_end < block_tokens.len() {
                                match block_tokens.get(stmt_end) {
                                    Some(
                                        Token::LeftParen | Token::LeftBrace | Token::LeftBracket,
                                    ) => depth += 1,
                                    Some(
                                        Token::RightParen | Token::RightBrace | Token::RightBracket,
                                    ) => depth -= 1,
                                    Some(Token::Identifier(_))
                                        if depth == 0 && stmt_end + 1 < block_tokens.len() =>
                                    {
                                        if matches!(
                                            block_tokens.get(stmt_end + 1),
                                            Some(Token::Assign)
                                        ) {
                                            break; // Next statement
                                        }
                                    }
                                    _ => {}
                                }
                                stmt_end += 1;
                            }

                            // Parse this statement's expression
                            let stmt_expr_tokens = block_tokens[(j + 2)..stmt_end].to_vec();
                            let mut manual_parser = ManualParser::new(stmt_expr_tokens);
                            match manual_parser.parse_expression() {
                                Ok(expr) => {
                                    statements.push(Statement::VariableDecl(VariableDecl {
                                        name: var_name.clone(),
                                        mutability: Mutability::Immutable,
                                        type_annotation: None,
                                        value: expr,
                                    }));
                                }
                                Err(_) => {
                                    // Skip this statement
                                }
                            }
                            j = stmt_end;
                        } else {
                            j += 1;
                        }
                    } else {
                        j += 1;
                    }
                }

                FunctionBody::Block(statements)
            }
            _ => return Err("Expected function body".to_string()),
        }
    } else {
        return Err("Missing function body".to_string());
    };

    Ok(Item::Function(Function {
        name,
        params,
        return_type: None,
        body,
    }))
}

fn parse_variable_with_complex_expr(tokens: Vec<Token>) -> Result<Item, String> {
    if tokens.len() < 3 {
        return Err("Invalid variable declaration".to_string());
    }

    let name = match &tokens[0] {
        Token::Identifier(n) => n.clone(),
        _ => return Err("Expected identifier".to_string()),
    };

    // Find the assignment position (handles both "name = expr" and "name: Type = expr")
    let assignment_pos = tokens.iter().position(|t| matches!(t, Token::Assign));

    let assign_idx = match assignment_pos {
        Some(idx) => idx,
        None => return Err("Expected assignment".to_string()),
    };

    // Parse the expression part with manual parser
    let expr_tokens = tokens[(assign_idx + 1)..].to_vec();

    let value = if matches!(expr_tokens.first(), Some(Token::Case)) {
        let mut manual_parser = ManualParser::new(expr_tokens);
        manual_parser.parse_case_expression()
    } else if matches!(expr_tokens.first(), Some(Token::When)) {
        let mut manual_parser = ManualParser::new(expr_tokens);
        manual_parser.parse_when_expression()
    } else if matches!(expr_tokens.first(), Some(Token::LeftBrace)) {
        let mut manual_parser = ManualParser::new(expr_tokens);
        manual_parser.parse_block_expression()
    } else if matches!(expr_tokens.first(), Some(Token::Pipe)) {
        let mut manual_parser = ManualParser::new(expr_tokens);
        manual_parser.parse_lambda_expression()
    } else if expr_tokens.iter().any(|t| matches!(t, Token::Question)) {
        let mut manual_parser = ManualParser::new(expr_tokens);
        manual_parser.parse_expression()
    } else if expr_tokens.iter().any(|t| match t {
        Token::String(s) => crate::lexer::has_interpolation(s),
        _ => false,
    }) {
        let mut manual_parser = ManualParser::new(expr_tokens);
        manual_parser.parse_expression()
    } else if expr_tokens
        .iter()
        .any(|t| matches!(t, Token::Pipeline))
    {
        let mut manual_parser = ManualParser::new(expr_tokens);
        manual_parser.parse_expression()
    } else if has_constructor_expression(&expr_tokens) {
        let mut manual_parser = ManualParser::new(expr_tokens);
        manual_parser.parse_expression()
    } else {
        return Err(
            "Expected case, when, ternary, block, lambda, pipeline, string interpolation, or constructor expression"
                .to_string(),
        );
    }
    .map_err(|e| format!("Parse error in '{}': {}", name, e.message))?;

    Ok(Item::VariableDecl(VariableDecl {
        name,
        mutability: Mutability::Immutable, // Default for hybrid parser
        type_annotation: None,
        value,
    }))
}
