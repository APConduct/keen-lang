use crate::ast::{
    CaseArm, Expression, Function, FunctionBody, Item, Literal, Parameter, Pattern, Program,
    Statement, Type, TypeDef, VariableDecl, WhenArm,
};
use crate::lexer::Token;
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
    let ident = select! { Token::Identifier(name) => name };
    ident.map(Type::Named)
}

fn statement_parser() -> impl Parser<Token, Statement, Error = Simple<Token>> {
    choice((
        variable_decl_parser().map(Statement::VariableDecl),
        expression_parser().map(Statement::Expression),
    ))
}

fn expression_parser() -> impl Parser<Token, Expression, Error = Simple<Token>> {
    let literal = choice((
        select! { Token::Integer(n) => Literal::Integer(n) },
        select! { Token::Float(f) => Literal::Float(f) },
        select! { Token::String(s) => Literal::String(s) },
        just(Token::True).to(Literal::Boolean(true)),
        just(Token::False).to(Literal::Boolean(false)),
    ))
    .map(Expression::Literal);

    let ident = select! { Token::Identifier(name) => Expression::Identifier(name) };

    // For now, return simple expressions without binary operations
    choice((literal, ident))
}

fn case_arm_parser() -> impl Parser<Token, CaseArm, Error = Simple<Token>> {
    pattern_parser()
        .then_ignore(just(Token::Arrow))
        .then(expression_parser())
        .map(|(pattern, body)| CaseArm { pattern, body })
}

fn when_arm_parser() -> impl Parser<Token, WhenArm, Error = Simple<Token>> {
    expression_parser()
        .then_ignore(just(Token::Arrow))
        .then(expression_parser())
        .map(|(condition, body)| WhenArm { condition, body })
}

fn pattern_parser() -> impl Parser<Token, Pattern, Error = Simple<Token>> {
    let ident = select! { Token::Identifier(name) => name };

    let literal_pattern = choice((
        select! { Token::Integer(n) => Literal::Integer(n) },
        select! { Token::Float(f) => Literal::Float(f) },
        select! { Token::String(s) => Literal::String(s) },
        just(Token::True).to(Literal::Boolean(true)),
        just(Token::False).to(Literal::Boolean(false)),
    ))
    .map(Pattern::Literal);

    let wildcard = just(Token::Underscore).to(Pattern::Wildcard);

    let identifier = ident.clone().map(Pattern::Identifier);

    // Simple constructor without recursion for now
    let constructor = ident
        .clone()
        .then(
            just(Token::LeftParen)
                .ignore_then(just(Token::RightParen))
                .to(vec![])
                .or_not(),
        )
        .map(|(name, args)| Pattern::Constructor {
            name,
            args: args.unwrap_or_default(),
        });

    choice((literal_pattern, wildcard, constructor, identifier))
}
