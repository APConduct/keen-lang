use keen::lexer::Token;
use logos::Logos;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let content = match fs::read_to_string(&args[1]) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            std::process::exit(1);
        }
    };

    println!("File content:");
    println!("{}", content);
    println!("\nTokens:");

    let mut lexer = Token::lexer(&content);
    let mut position = 0;

    while let Some(token_result) = lexer.next() {
        match token_result {
            Ok(token) => {
                println!("{}: {:?}", position, token);
                position += 1;
            }
            Err(e) => {
                println!("{}: ERROR: {:?}", position, e);
                println!("Remaining text: {:?}", lexer.remainder());
                break;
            }
        }
    }

    println!("\nTotal tokens: {}", position);
}
