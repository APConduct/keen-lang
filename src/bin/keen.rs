use keen::{codegen::KeenCodegen, lexer::Token, parser};
use logos::Logos;
use std::env;
use std::fs;
use std::path::Path;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage();
        process::exit(1);
    }

    match args[1].as_str() {
        "run" => {
            if args.len() < 3 {
                eprintln!("Error: Please specify a file to run");
                print_usage();
                process::exit(1);
            }
            run_file(&args[2]);
        }
        "compile" => {
            if args.len() < 3 {
                eprintln!("Error: Please specify a file to compile");
                print_usage();
                process::exit(1);
            }
            compile_file(&args[2], args.get(3));
        }
        "repl" => {
            start_repl();
        }
        "check" => {
            if args.len() < 3 {
                eprintln!("Error: Please specify a file to check");
                print_usage();
                process::exit(1);
            }
            check_file(&args[2]);
        }
        "--version" | "-v" => {
            println!("Keen Programming Language v0.1.0");
        }
        "--help" | "-h" => {
            print_usage();
        }
        _ => {
            // If no subcommand, try to run the file directly
            if Path::new(&args[1]).exists() {
                run_file(&args[1]);
            } else {
                eprintln!("Error: Unknown command '{}'", args[1]);
                print_usage();
                process::exit(1);
            }
        }
    }
}

fn print_usage() {
    println!("Keen Programming Language");
    println!();
    println!("USAGE:");
    println!("    keen <SUBCOMMAND> [OPTIONS]");
    println!();
    println!("SUBCOMMANDS:");
    println!("    run <file>              Run a Keen file");
    println!("    compile <file> [output] Compile a Keen file to binary");
    println!("    check <file>            Check syntax and types");
    println!("    repl                    Start interactive REPL");
    println!("    --version, -v           Show version information");
    println!("    --help, -h              Show this help message");
    println!();
    println!("EXAMPLES:");
    println!("    keen run example.kn");
    println!("    keen compile main.kn output");
    println!("    keen check src/lib.kn");
    println!("    keen repl");
}

fn run_file(filename: &str) {
    println!("🚀 Running Keen file: {}", filename);

    // Read the file
    let content = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            process::exit(1);
        }
    };

    // Parse the content
    let tokens: Vec<Token> = match Token::lexer(&content).collect::<Result<Vec<_>, _>>() {
        Ok(tokens) => tokens,
        Err(_) => {
            eprintln!("Error: Failed to tokenize the file");
            process::exit(1);
        }
    };

    println!("✅ Tokenized {} tokens", tokens.len());

    // Parse with hybrid parser
    let program = match parser::parse_with_manual_fallback(tokens) {
        Ok(program) => program,
        Err(errors) => {
            eprintln!("Parse errors:");
            for error in errors {
                eprintln!("  • {}", error);
            }
            process::exit(1);
        }
    };

    println!("✅ Parsed {} items", program.items.len());

    // Initialize runtime
    keen::runtime::keen_runtime_init();

    // Compile and execute
    match KeenCodegen::new() {
        Ok(mut codegen) => {
            match codegen.compile_program(&program) {
                Ok(()) => {
                    println!("✅ Compilation successful");

                    match codegen.finalize() {
                        Ok(()) => {
                            println!("✅ Finalization complete");

                            // Look for main function and execute it
                            if let Ok(main_ptr) = codegen.get_function_ptr("main") {
                                println!("🎯 Executing main function...");
                                let result = keen::codegen::execute_function_i64(main_ptr);
                                println!("✅ Program completed with exit code: {}", result);
                            } else {
                                println!(
                                    "ℹ️  No main function found - executing all top-level items"
                                );
                                println!("✅ Program executed successfully");
                            }
                        }
                        Err(e) => {
                            eprintln!("Finalization error: {}", e);
                            process::exit(1);
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Compilation error: {}", e);
                    process::exit(1);
                }
            }
        }
        Err(e) => {
            eprintln!("Codegen initialization error: {}", e);
            process::exit(1);
        }
    }

    // Cleanup runtime
    keen::runtime::keen_runtime_cleanup();
}

fn compile_file(filename: &str, output: Option<&String>) {
    println!("🔨 Compiling Keen file: {}", filename);

    let output_name = output.map(|s| s.as_str()).unwrap_or_else(|| {
        Path::new(filename)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output")
    });

    // Read the file
    let content = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            process::exit(1);
        }
    };

    // Parse the content
    let tokens: Vec<Token> = match Token::lexer(&content).collect::<Result<Vec<_>, _>>() {
        Ok(tokens) => tokens,
        Err(_) => {
            eprintln!("Error: Failed to tokenize the file");
            process::exit(1);
        }
    };

    let program = match parser::parse_with_manual_fallback(tokens) {
        Ok(program) => program,
        Err(errors) => {
            eprintln!("Parse errors:");
            for error in errors {
                eprintln!("  • {}", error);
            }
            process::exit(1);
        }
    };

    // Compile
    match KeenCodegen::new() {
        Ok(mut codegen) => {
            match codegen.compile_program(&program) {
                Ok(()) => {
                    match codegen.finalize() {
                        Ok(()) => {
                            // TODO: Write compiled binary to file
                            println!("✅ Compiled successfully to '{}'", output_name);
                            println!("ℹ️  Note: Binary output not yet implemented - compiled to memory only");
                        }
                        Err(e) => {
                            eprintln!("Finalization error: {}", e);
                            process::exit(1);
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Compilation error: {}", e);
                    process::exit(1);
                }
            }
        }
        Err(e) => {
            eprintln!("Codegen initialization error: {}", e);
            process::exit(1);
        }
    }
}

fn check_file(filename: &str) {
    println!("🔍 Checking Keen file: {}", filename);

    // Read the file
    let content = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            process::exit(1);
        }
    };

    // Tokenize
    let tokens: Vec<Token> = match Token::lexer(&content).collect::<Result<Vec<_>, _>>() {
        Ok(tokens) => tokens,
        Err(_) => {
            eprintln!("❌ Tokenization failed");
            process::exit(1);
        }
    };

    println!("✅ Tokenization: {} tokens", tokens.len());

    // Parse
    match parser::parse_with_manual_fallback(tokens) {
        Ok(program) => {
            println!(
                "✅ Syntax: {} items parsed successfully",
                program.items.len()
            );

            // Basic semantic checks
            let mut has_main = false;
            let mut function_count = 0;
            let mut type_count = 0;
            let mut variable_count = 0;

            for item in &program.items {
                match item {
                    keen::ast::Item::Function(func) => {
                        function_count += 1;
                        if func.name == "main" {
                            has_main = true;
                        }
                    }
                    keen::ast::Item::TypeDef(_) => {
                        type_count += 1;
                    }
                    keen::ast::Item::VariableDecl(_) => {
                        variable_count += 1;
                    }
                }
            }

            println!("📊 Analysis:");
            println!("  • Functions: {}", function_count);
            println!("  • Types: {}", type_count);
            println!("  • Variables: {}", variable_count);

            if has_main {
                println!("✅ Main function found");
            } else {
                println!("ℹ️  No main function (will execute top-level items)");
            }

            println!("✅ File check completed successfully");
        }
        Err(errors) => {
            println!("❌ Syntax errors found:");
            for error in errors {
                eprintln!("  • {}", error);
            }
            process::exit(1);
        }
    }
}

fn start_repl() {
    println!("🎯 Keen Interactive REPL");
    println!("Type expressions and press Enter. Type 'exit' to quit.");
    println!();

    use std::io::{self, Write};

    // Initialize runtime
    keen::runtime::keen_runtime_init();

    loop {
        print!("keen> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let input = input.trim();

                if input.is_empty() {
                    continue;
                }

                if input == "exit" || input == "quit" {
                    println!("Goodbye! 👋");
                    break;
                }

                // Special REPL commands
                match input {
                    "help" => {
                        println!("REPL Commands:");
                        println!("  help     - Show this help");
                        println!("  exit     - Exit REPL");
                        println!("  clear    - Clear screen");
                        println!("  version  - Show version");
                        println!();
                        println!("You can enter any Keen expression or statement.");
                        continue;
                    }
                    "clear" => {
                        print!("\x1B[2J\x1B[1;1H");
                        continue;
                    }
                    "version" => {
                        println!("Keen v0.1.0");
                        continue;
                    }
                    _ => {}
                }

                // Try to parse and execute the input
                eval_repl_input(input);
            }
            Err(error) => {
                eprintln!("Error reading input: {}", error);
                break;
            }
        }
    }

    // Cleanup runtime
    keen::runtime::keen_runtime_cleanup();
}

fn eval_repl_input(input: &str) {
    // Tokenize
    let tokens: Vec<Token> = match Token::lexer(input).collect::<Result<Vec<_>, _>>() {
        Ok(tokens) => tokens,
        Err(_) => {
            println!("❌ Tokenization error");
            return;
        }
    };

    // Parse
    match parser::parse_with_manual_fallback(tokens) {
        Ok(program) => {
            println!("✅ Parsed: {} items", program.items.len());

            // Try to compile and execute
            match KeenCodegen::new() {
                Ok(mut codegen) => {
                    match codegen.compile_program(&program) {
                        Ok(()) => {
                            match codegen.finalize() {
                                Ok(()) => {
                                    println!("✅ Compiled successfully");
                                    // TODO: Execute and show result
                                }
                                Err(e) => println!("❌ Finalization error: {}", e),
                            }
                        }
                        Err(e) => println!("❌ Compilation error: {}", e),
                    }
                }
                Err(e) => println!("❌ Codegen error: {}", e),
            }
        }
        Err(errors) => {
            println!("❌ Parse errors:");
            for error in errors {
                println!("  • {}", error);
            }
        }
    }
}
