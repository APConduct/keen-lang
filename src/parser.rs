use crate::ast::{
    BinaryOp, Expression, Function, FunctionBody, Item, Literal, Parameter, Program, Statement,
    Type, TypeDef, VariableDecl,
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

fn item_parser() -> impl Parser<Token, Item, Error = Simple<Token>> {
    choice((
        function_parser(),
        type_def_parser(),
        variable_decl_parser().map(Item::VariableDecl),
    ))
}

fn function_parser() -> impl Parser<Token, Item, Error = Simple<Token>> {
    let ident = select! { Token::Identifier(name) => name };

    let param = ident
        .clone()
        .then(just(Token::Colon).ignore_then(type_parser()).or_not())
        .map(|(name, type_annotation)| Parameter {
            name,
            type_annotation,
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

    let distinct = just(Token::Distinct).or_not().map(|d| d.is_some());

    just(Token::Type)
        .ignore_then(ident)
        .then(just(Token::Assign).ignore_then(distinct))
        .then(type_parser())
        .map(|((name, is_distinct), underlying_type)| {
            Item::TypeDef(TypeDef {
                name,
                is_distinct,
                underlying_type,
            })
        })
}

fn variable_decl_parser() -> impl Parser<Token, VariableDecl, Error = Simple<Token>> {
    let ident = select! { Token::Identifier(name) => name };

    ident
        .then(just(Token::Colon).ignore_then(type_parser()).or_not())
        .then_ignore(just(Token::Assign))
        .then(expression_parser())
        .map(|((name, type_annotation), value)| VariableDecl {
            name,
            type_annotation,
            value,
        })
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
        expression_parser().map(Statement::Expression),
    ))
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

        // Field access
        let field = call_expr.then(
            just(Token::Dot)
                .ignore_then(select! { Token::Identifier(field) => field })
                .repeated(),
        );

        let field_expr = field.map(|(base, fields)| {
            fields
                .into_iter()
                .fold(base, |acc, field| Expression::FieldAccess {
                    object: Box::new(acc),
                    field,
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

        comparison
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
    tokens
        .iter()
        .any(|token| matches!(token, Token::Case | Token::When))
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

    // Check if this is a variable declaration with case/when
    if is_variable_decl_with_complex_expr(&item_tokens) {
        parse_variable_with_complex_expr(item_tokens)
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
            // Check if the expression contains case or when
            return tokens[2..]
                .iter()
                .any(|t| matches!(t, Token::Case | Token::When));
        }
    }
    false
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
    } else {
        return Err("Expected case or when expression".to_string());
    }
    .map_err(|e| e.message)?;

    Ok(Item::VariableDecl(VariableDecl {
        name,
        type_annotation: None,
        value,
    }))
}
