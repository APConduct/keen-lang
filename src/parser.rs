use crate::ast::{
    BinaryOp, Expression, Function, FunctionBody, Item, Literal, Mutability, Parameter, Pattern,
    ProductField, Program, Statement, Type, TypeDef, UnionVariant, VariableDecl,
};
use crate::lexer::Token;
use crate::manual_parser::ManualParser;

// Simplified single parser approach - use manual parser for everything
pub fn parse_with_manual_fallback(tokens: Vec<Token>) -> Result<Program, Vec<String>> {
    eprintln!("DEBUG: Using manual parser for all expressions");
    let mut parser = ManualParser::new(tokens);
    match parser.parse_program() {
        Ok(program) => {
            eprintln!(
                "DEBUG: Manual parser succeeded with {} items",
                program.items.len()
            );
            Ok(program)
        }
        Err(e) => {
            eprintln!("DEBUG: Manual parser failed: {}", e.message);
            Err(vec![e.message])
        }
    }
}

// Remove unused helper functions since we're using manual parser for everything
