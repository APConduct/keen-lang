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
}
