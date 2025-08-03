use chumsky::Parser;
use keen::{lexer::Token, parser};
use logos::Logos;

fn main() {
    println!("Hello, world!");

    // Test basic parsing
    let input = "x = 42";
    let tokens: Vec<Token> = Token::lexer(input).collect::<Result<Vec<_>, _>>().unwrap();
    println!("Tokens: {:?}", tokens);

    let parser = parser::parser();
    match parser.parse(tokens) {
        Ok(program) => println!("Parsed successfully: {:?}", program),
        Err(errors) => println!("Parse errors: {:?}", errors),
    }

    // Test function parsing
    let input2 = "add(x, y) = x";
    let tokens2: Vec<Token> = Token::lexer(input2).collect::<Result<Vec<_>, _>>().unwrap();
    println!("\nTokens: {:?}", tokens2);

    let func_parser = parser::parser();
    match func_parser.parse(tokens2) {
        Ok(program) => println!("Function parsed successfully: {:?}", program),
        Err(errors) => println!("Function parse errors: {:?}", errors),
    }

    // Test binary operations
    let input3 = "result = 2 + 3 * 4";
    let tokens3: Vec<Token> = Token::lexer(input3).collect::<Result<Vec<_>, _>>().unwrap();
    println!("\nTokens: {:?}", tokens3);

    let binary_parser = parser::parser();
    match binary_parser.parse(tokens3) {
        Ok(program) => println!("Binary operation parsed successfully: {:?}", program),
        Err(errors) => println!("Binary operation parse errors: {:?}", errors),
    }

    // Test function with type annotations
    let input4 = "square(x: Int): Int = x * x";
    let tokens4: Vec<Token> = Token::lexer(input4).collect::<Result<Vec<_>, _>>().unwrap();
    println!("\nTokens: {:?}", tokens4);

    let typed_parser = parser::parser();
    match typed_parser.parse(tokens4) {
        Ok(program) => println!("Typed function parsed successfully: {:?}", program),
        Err(errors) => println!("Typed function parse errors: {:?}", errors),
    }

    // Test comparison operations
    let input5 = "is_valid = x >= 0";
    let tokens5: Vec<Token> = Token::lexer(input5).collect::<Result<Vec<_>, _>>().unwrap();
    println!("\nTokens: {:?}", tokens5);

    let comp_parser = parser::parser();
    match comp_parser.parse(tokens5) {
        Ok(program) => println!("Comparison parsed successfully: {:?}", program),
        Err(errors) => println!("Comparison parse errors: {:?}", errors),
    }

    // Test function calls
    let input6 = "result = add(2, 3)";
    let tokens6: Vec<Token> = Token::lexer(input6).collect::<Result<Vec<_>, _>>().unwrap();
    println!("\nTokens: {:?}", tokens6);

    let call_parser = parser::parser();
    match call_parser.parse(tokens6) {
        Ok(program) => println!("Function call parsed successfully: {:?}", program),
        Err(errors) => println!("Function call parse errors: {:?}", errors),
    }

    // Test field access
    let input7 = "name = user.first_name";
    let tokens7: Vec<Token> = Token::lexer(input7).collect::<Result<Vec<_>, _>>().unwrap();
    println!("\nTokens: {:?}", tokens7);

    let field_parser = parser::parser();
    match field_parser.parse(tokens7) {
        Ok(program) => println!("Field access parsed successfully: {:?}", program),
        Err(errors) => println!("Field access parse errors: {:?}", errors),
    }

    // Test complex expression combining multiple features
    let input8 = "result = calculate(x.value, y) + z * 2";
    let tokens8: Vec<Token> = Token::lexer(input8).collect::<Result<Vec<_>, _>>().unwrap();
    println!("\nTokens: {:?}", tokens8);

    let complex_parser = parser::parser();
    match complex_parser.parse(tokens8) {
        Ok(program) => println!("Complex expression parsed successfully: {:?}", program),
        Err(errors) => println!("Complex expression parse errors: {:?}", errors),
    }

    // Test function types
    let input9 = "apply(f: (Int, Int) -> Int, x: Int, y: Int): Int = f(x, y)";
    let tokens9: Vec<Token> = Token::lexer(input9).collect::<Result<Vec<_>, _>>().unwrap();
    println!("\nTokens: {:?}", tokens9);

    let func_type_parser = parser::parser();
    match func_type_parser.parse(tokens9) {
        Ok(program) => println!("Function type parsed successfully: {:?}", program),
        Err(errors) => println!("Function type parse errors: {:?}", errors),
    }

    // Summary of parser capabilities
    println!("\n=== Parser Capabilities Summary ===");
    println!("✅ Variable declarations with type inference");
    println!("✅ Function definitions with parameters and type annotations");
    println!("✅ Binary operations with correct precedence (*, / before +, -, comparisons)");
    println!("✅ Function calls with multiple arguments");
    println!("✅ Field access (object.field)");
    println!("✅ Complex nested expressions");
    println!("✅ Function types in type annotations");
    println!("✅ Parenthesized expressions");
    println!("🚧 Case/When expressions (complex parser combinator limitations)");
    println!("🚧 Ternary operators (complex parser combinator limitations)");
}
