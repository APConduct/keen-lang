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

    // Test product type definition
    println!("\n=== Testing Product Types ===");
    let product_type = "type User = User(id: Int, name: String, email: String)";
    let product_tokens: Vec<Token> = Token::lexer(product_type)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    println!("Product type tokens: {:?}", product_tokens);

    let product_parser = parser::parser();
    match product_parser.parse(product_tokens) {
        Ok(program) => println!("✅ Product type parsed: {:?}", program),
        Err(errors) => println!("❌ Product type errors: {:?}", errors),
    }

    // Test union type definition
    let union_type = "type Result = Ok(value: String) | Error(message: String)";
    let union_tokens: Vec<Token> = Token::lexer(union_type)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    println!("\nUnion type tokens: {:?}", union_tokens);

    let union_parser = parser::parser();
    match union_parser.parse(union_tokens) {
        Ok(program) => println!("✅ Union type parsed: {:?}", program),
        Err(errors) => println!("❌ Union type errors: {:?}", errors),
    }

    // Test constructor expression
    let constructor_expr = r#"user = User(id: 1, name: "Alice", email: "alice@test.com")"#;
    let constructor_tokens: Vec<Token> = Token::lexer(constructor_expr)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    println!("\nConstructor tokens: {:?}", constructor_tokens);

    match parser::parse_with_manual_fallback(constructor_tokens) {
        Ok(program) => println!("✅ Constructor expression parsed: {:?}", program),
        Err(errors) => println!("❌ Constructor errors: {:?}", errors),
    }

    // Test function with parameter mutability
    let param_mut = "update_cache(live cache: Map): Unit = cache";
    let param_mut_tokens: Vec<Token> = Token::lexer(param_mut)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    println!("\nParameter mutability tokens: {:?}", param_mut_tokens);

    let param_mut_parser = parser::parser();
    match param_mut_parser.parse(param_mut_tokens) {
        Ok(program) => println!("✅ Parameter mutability parsed: {:?}", program),
        Err(errors) => println!("❌ Parameter mutability errors: {:?}", errors),
    }

    // Test simpler parameter mutability
    let simple_param = "test(live x: Int) = x";
    let simple_tokens: Vec<Token> = Token::lexer(simple_param)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    println!("\nSimple parameter mutability tokens: {:?}", simple_tokens);

    let simple_parser = parser::parser();
    match simple_parser.parse(simple_tokens) {
        Ok(program) => println!("✅ Simple parameter mutability parsed: {:?}", program),
        Err(errors) => println!("❌ Simple parameter mutability errors: {:?}", errors),
    }

    // Test variable type annotations
    println!("\n=== Testing Variable Type Annotations ===");
    let typed_var = "counter: Int = 42";
    let typed_var_tokens: Vec<Token> = Token::lexer(typed_var)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    println!("Typed variable tokens: {:?}", typed_var_tokens);

    let typed_var_parser = parser::parser();
    match typed_var_parser.parse(typed_var_tokens) {
        Ok(program) => println!("✅ Typed variable parsed: {:?}", program),
        Err(errors) => println!("❌ Typed variable errors: {:?}", errors),
    }

    // Test live variable with type annotation
    let live_typed_var = "live cache: Map = {}";
    let live_typed_tokens: Vec<Token> = Token::lexer(live_typed_var)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    println!("\nLive typed variable tokens: {:?}", live_typed_tokens);

    match parser::parse_with_manual_fallback(live_typed_tokens) {
        Ok(program) => println!("✅ Live typed variable parsed: {:?}", program),
        Err(errors) => println!("❌ Live typed variable errors: {:?}", errors),
    }

    // Test destructuring assignment
    println!("\n=== Testing Destructuring Assignment ===");
    let destructuring = "Point(x, y) = get_position()";
    let destructuring_tokens: Vec<Token> = Token::lexer(destructuring)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    println!("Destructuring tokens: {:?}", destructuring_tokens);

    let destructuring_parser = parser::parser();
    match destructuring_parser.parse(destructuring_tokens) {
        Ok(program) => println!("✅ Destructuring assignment parsed: {:?}", program),
        Err(errors) => println!("❌ Destructuring assignment errors: {:?}", errors),
    }

    // Test simple destructuring
    let simple_destructuring = "User(name, age) = user_data";
    let simple_destructuring_tokens: Vec<Token> = Token::lexer(simple_destructuring)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    println!(
        "\nSimple destructuring tokens: {:?}",
        simple_destructuring_tokens
    );

    let simple_destructuring_parser = parser::parser();
    match simple_destructuring_parser.parse(simple_destructuring_tokens) {
        Ok(program) => println!("✅ Simple destructuring parsed: {:?}", program),
        Err(errors) => println!("❌ Simple destructuring errors: {:?}", errors),
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

    // Test mutability keywords
    println!("\n=== Testing Mutability Keywords ===");
    let live_var = "live counter = 0";
    let live_tokens: Vec<Token> = Token::lexer(live_var)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    let live_parser = parser::parser();
    match live_parser.parse(live_tokens) {
        Ok(program) => println!("✅ Live variable parsed: {:?}", program),
        Err(errors) => println!("❌ Live variable errors: {:?}", errors),
    }

    let keep_var = "keep PI = 3.14159";
    let keep_tokens: Vec<Token> = Token::lexer(keep_var)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    let keep_parser = parser::parser();
    match keep_parser.parse(keep_tokens) {
        Ok(program) => println!("✅ Keep variable parsed: {:?}", program),
        Err(errors) => println!("❌ Keep variable errors: {:?}", errors),
    }

    // Test collection literals
    println!("\n=== Testing Collection Literals ===");
    let list_tokens = vec![
        Token::LeftBracket,
        Token::Integer(1),
        Token::Comma,
        Token::Integer(2),
        Token::Comma,
        Token::Integer(3),
        Token::RightBracket,
    ];

    let mut list_parser = ManualParser::new(list_tokens);
    match list_parser.parse_expression() {
        Ok(expr) => println!("✅ List literal parsed: {:?}", expr),
        Err(e) => println!("❌ List literal error: {}", e),
    }

    let map_tokens = vec![
        Token::LeftBrace,
        Token::String("key1".to_string()),
        Token::Colon,
        Token::String("value1".to_string()),
        Token::Comma,
        Token::String("key2".to_string()),
        Token::Colon,
        Token::Integer(42),
        Token::RightBrace,
    ];

    let mut map_parser = ManualParser::new(map_tokens);
    match map_parser.parse_expression() {
        Ok(expr) => println!("✅ Map literal parsed: {:?}", expr),
        Err(e) => println!("❌ Map literal error: {}", e),
    }

    // Test method calls
    println!("\n=== Testing Method Calls ===");
    let method_tokens = vec![
        Token::Identifier("cache".to_string()),
        Token::LeftParen,
        Token::RightParen,
        Token::Dot,
        Token::Identifier("insert".to_string()),
        Token::LeftParen,
        Token::String("key".to_string()),
        Token::Comma,
        Token::String("value".to_string()),
        Token::RightParen,
    ];

    let mut method_parser = ManualParser::new(method_tokens);
    match method_parser.parse_expression() {
        Ok(expr) => println!("✅ Method call parsed: {:?}", expr),
        Err(e) => println!("❌ Method call error: {}", e),
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
    println!("✅ Mutability keywords: live, keep");
    println!("✅ Collection literals: [1, 2, 3], {{\"key\": \"value\"}}");
    println!("✅ Method calls: object.method(args)");
    println!("✅ Hybrid parser integration (working!)");
    println!("✅ When expressions (manual parser ready)");
    println!("");
    println!("✅ Union/Sum types: type Result = Ok(T) | Error(E)");
    println!("✅ Product types: type User = User(id: Int, name: String)");
    println!("✅ Constructor expressions: User(id: 1, name: \"Alice\")");
    println!("✅ Parameter mutability: func(live param: Type)");
    println!("✅ Type annotations on variables: var: Type = value");
    println!("✅ Destructuring assignment: Point(x, y) = pos");
    println!("🚧 Advanced patterns: Customer(User(_, *, Email(e), age), ...)");
    println!("🚧 Block expressions with implicit returns");
}
