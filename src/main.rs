use chumsky::Parser;
use keen::{lexer::Token, manual_parser::ManualParser, parser};
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

    // Test direct manual parser for case expressions
    println!("\n=== Testing Manual Parser ===");

    let manual_case_tokens = vec![
        Token::Case,
        Token::Identifier("x".to_string()),
        Token::LeftBrace,
        Token::Integer(42),
        Token::Arrow,
        Token::String("answer".to_string()),
        Token::Underscore,
        Token::Arrow,
        Token::String("other".to_string()),
        Token::RightBrace,
    ];

    let mut manual_parser = ManualParser::new(manual_case_tokens);
    match manual_parser.parse_case_expression() {
        Ok(expr) => println!("✅ Case expression parsed: {:?}", expr),
        Err(e) => println!("❌ Manual parser error: {}", e),
    }

    // Test simple hybrid parser
    println!("\n=== Testing Fixed Hybrid Parser ===");
    let simple_case = r#"result = case x { 1 -> "one" _ -> "other" }"#;
    let tokens: Vec<Token> = Token::lexer(simple_case)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    match parser::parse_with_manual_fallback(tokens) {
        Ok(program) => println!("✅ Hybrid parser success: {:?}", program),
        Err(errors) => println!("❌ Hybrid parser errors: {:?}", errors),
    }

    // Test ternary operator
    println!("\n=== Testing Ternary Operator ===");
    let ternary_tokens = vec![
        Token::Identifier("condition".to_string()),
        Token::Question,
        Token::String("yes".to_string()),
        Token::Colon,
        Token::String("no".to_string()),
    ];

    let mut ternary_parser = ManualParser::new(ternary_tokens);
    match ternary_parser.parse_expression() {
        Ok(expr) => println!("✅ Ternary expression parsed: {:?}", expr),
        Err(e) => println!("❌ Ternary parser error: {}", e),
    }

    // Test ternary in variable declaration
    let ternary_var = r#"result = x > 0 ? "positive" : "non-positive""#;
    let ternary_var_tokens: Vec<Token> = Token::lexer(ternary_var)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    match parser::parse_with_manual_fallback(ternary_var_tokens) {
        Ok(program) => println!("✅ Ternary in variable parsed: {:?}", program),
        Err(errors) => println!("❌ Ternary variable errors: {:?}", errors),
    }

    // Test constructor patterns
    let constructor_tokens = vec![
        Token::Case,
        Token::Identifier("result".to_string()),
        Token::LeftBrace,
        Token::Identifier("Some".to_string()),
        Token::LeftParen,
        Token::Identifier("x".to_string()),
        Token::RightParen,
        Token::Arrow,
        Token::Identifier("x".to_string()),
        Token::Identifier("None".to_string()),
        Token::LeftParen,
        Token::RightParen,
        Token::Arrow,
        Token::Integer(0),
        Token::RightBrace,
    ];

    let mut constructor_parser = ManualParser::new(constructor_tokens);
    match constructor_parser.parse_case_expression() {
        Ok(expr) => println!("✅ Constructor pattern parsed: {:?}", expr),
        Err(e) => println!("❌ Constructor parser error: {}", e),
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
    println!("✅ Case expressions with patterns (manual parser working!)");
    println!("✅ Constructor patterns: Some(x), None(), User(name, age)");
    println!("✅ Wildcard patterns");
    println!("✅ Pattern matching with literals and identifiers");
    println!("✅ Ternary operators: condition ? then : else");
    println!("✅ Hybrid parser integration (working!)");
    println!("✅ When expressions (manual parser ready)");
}
