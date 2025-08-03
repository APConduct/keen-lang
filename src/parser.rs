use crate::ast::{
    BinaryOp, ConstructorArg, Expression, Function, FunctionBody, Item, Literal, Mutability,
    Parameter, Pattern, ProductField, Program, Statement, Type, TypeDef, UnionVariant,
    VariableDecl,
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
    tokens.iter().any(|token| {
        matches!(
            token,
            Token::Case
                | Token::When
                | Token::Question
                | Token::LeftBrace
                | Token::Pipe
                | Token::Pipeline
        ) || match token {
            Token::String(s) => crate::lexer::has_interpolation(s),
            _ => false,
        }
    })
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

    // Check if this is a variable declaration with complex expressions
    // Check if this is a variable declaration with case/when
    if is_variable_decl_with_complex_expr(&item_tokens) {
        parse_variable_with_complex_expr(item_tokens)
    } else if has_block_or_lambda(&item_tokens) {
        parse_item_with_manual_parser(item_tokens)
    } else if has_string_interpolation(&item_tokens) {
        parse_item_with_manual_parser(item_tokens)
    } else {
        // Use regular chumsky parser for this item
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

fn find_item_end(tokens: &[Token], start: usize) -> usize {
    if start >= tokens.len() {
        return tokens.len();
    }

    let mut i = start;
    let mut brace_depth = 0;
    let mut paren_depth = 0;

    // Simple heuristic: look for assignment and find the complete expression
    while i < tokens.len() {
        match &tokens[i] {
            Token::Assign if brace_depth == 0 && paren_depth == 0 => {
                // Found assignment, now find the end of the value expression
                i += 1; // skip the '='
                while i < tokens.len() {
                    match &tokens[i] {
                        Token::LeftBrace => brace_depth += 1,
                        Token::RightBrace => {
                            brace_depth -= 1;
                            if brace_depth == 0 {
                                return i + 1; // Include the closing brace
                            }
                        }
                        Token::LeftParen => paren_depth += 1,
                        Token::RightParen => paren_depth -= 1,
                        _ => {}
                    }
                    i += 1;
                }
                return tokens.len();
            }
            Token::LeftBrace => brace_depth += 1,
            Token::RightBrace => brace_depth -= 1,
            Token::LeftParen => paren_depth += 1,
            Token::RightParen => paren_depth -= 1,
            _ => {}
        }
        i += 1;
    }
    tokens.len()
}

fn is_variable_decl_with_complex_expr(tokens: &[Token]) -> bool {
    // Check if this is a variable declaration (identifier = ...)
    if tokens.len() >= 3 {
        if let (Some(Token::Identifier(_)), Some(Token::Assign)) = (tokens.get(0), tokens.get(1)) {
            // Check if the expression contains case, when, or ternary
            return tokens[2..]
                .iter()
                .any(|t| matches!(t, Token::Case | Token::When | Token::Question));
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
    // Check if this is a variable declaration pattern: name = complex_expr
    if tokens.len() >= 3 {
        if let (Some(Token::Identifier(name)), Some(Token::Assign)) = (tokens.get(0), tokens.get(1))
        {
            let name = name.clone();
            let expr_tokens = tokens[2..].to_vec();

            let mut manual_parser = ManualParser::new(expr_tokens);
            if let Ok(expr) = manual_parser.parse_expression() {
                return Ok(Item::VariableDecl(VariableDecl {
                    name,
                    mutability: Mutability::Immutable,
                    type_annotation: None,
                    value: expr,
                }));
            }
        }
    }

    Err("Failed to parse with manual parser".to_string())
}

fn parse_variable_with_complex_expr(tokens: Vec<Token>) -> Result<Item, String> {
    if tokens.len() < 3 {
        return Err("Invalid variable declaration".to_string());
    }

    let name = match &tokens[0] {
        Token::Identifier(n) => n.clone(),
        _ => return Err("Expected identifier".to_string()),
    };

    if !matches!(tokens[1], Token::Assign) {
        return Err("Expected assignment".to_string());
    }

    // Parse the expression part with manual parser
    let expr_tokens = tokens[2..].to_vec();

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
        // Parse ternary expression - need to parse from the beginning
        let mut manual_parser = ManualParser::new(expr_tokens);
        manual_parser.parse_expression()
    } else if expr_tokens.iter().any(|t| match t {
        Token::String(s) => crate::lexer::has_interpolation(s),
        _ => false,
    }) {
        // Parse string interpolation
        let mut manual_parser = ManualParser::new(expr_tokens);
        manual_parser.parse_expression()
    } else if expr_tokens
        .iter()
        .any(|t| matches!(t, Token::Pipeline))
    {
        // Parse pipeline expression
        let mut manual_parser = ManualParser::new(expr_tokens);
        manual_parser.parse_expression()
    } else {
        return Err(
            "Expected case, when, ternary, block, lambda, pipeline, or string interpolation expression"
                .to_string(),
        );
    }
    .map_err(|e| e.message)?;

    Ok(Item::VariableDecl(VariableDecl {
        name,
        mutability: Mutability::Immutable, // Default for hybrid parser
        type_annotation: None,
        value,
    }))
}
