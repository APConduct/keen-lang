use crate::ast::{
    BinaryOp, Expression, Function, FunctionBody, Item, Literal, Parameter, Program, Statement,
    Type, TypeDef, VariableDecl,
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

// Note: Case/When expressions and pattern matching are not implemented
// due to complex parser combinator limitations that cause clone issues.
// These could be implemented in a future version with a different parsing approach.
