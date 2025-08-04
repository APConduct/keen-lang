use crate::ast::*;
use crate::lexer::{has_interpolation, Token};
use logos::Logos;

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub position: usize,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Parse error at position {}: {}",
            self.position, self.message
        )
    }
}

impl std::error::Error for ParseError {}

pub struct ManualParser {
    tokens: Vec<Token>,
    position: usize,
}

impl ManualParser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    pub fn parse_case_expression(&mut self) -> Result<Expression, ParseError> {
        // Expect 'case'
        self.expect_token(&Token::Case, "Expected 'case'")?;

        // Parse the expression to match against
        let expr = self.parse_expression()?;

        // Expect '{'
        self.expect_token(&Token::LeftBrace, "Expected '{' after case expression")?;

        // Parse case arms
        let mut arms = Vec::new();
        while !self.check_token(&Token::RightBrace) && !self.is_at_end() {
            arms.push(self.parse_case_arm()?);
        }

        if arms.is_empty() {
            return Err(ParseError {
                message: "Case expression must have at least one arm".to_string(),
                position: self.position,
            });
        }

        // Expect '}'
        self.expect_token(&Token::RightBrace, "Expected '}' after case arms")?;

        Ok(Expression::Case {
            expr: Box::new(expr),
            arms,
        })
    }

    pub fn parse_when_expression(&mut self) -> Result<Expression, ParseError> {
        // Expect 'when'
        self.expect_token(&Token::When, "Expected 'when'")?;

        // Parse the expression to evaluate
        let expr = self.parse_expression()?;

        // Expect '{'
        self.expect_token(&Token::LeftBrace, "Expected '{' after when expression")?;

        // Parse when arms
        let mut arms = Vec::new();
        while !self.check_token(&Token::RightBrace) && !self.is_at_end() {
            arms.push(self.parse_when_arm()?);
        }

        if arms.is_empty() {
            return Err(ParseError {
                message: "When expression must have at least one arm".to_string(),
                position: self.position,
            });
        }

        // Expect '}'
        self.expect_token(&Token::RightBrace, "Expected '}' after when arms")?;

        Ok(Expression::When {
            expr: Box::new(expr),
            arms,
        })
    }

    pub fn parse_ternary_expression(
        &mut self,
        condition: Expression,
    ) -> Result<Expression, ParseError> {
        // Expect '?'
        self.expect_token(&Token::Question, "Expected '?'")?;

        // Parse then expression
        let then_expr = self.parse_binary_expression()?;

        // Expect ':'
        self.expect_token(&Token::Colon, "Expected ':' after ternary then expression")?;

        // Parse else expression
        let else_expr = self.parse_binary_expression()?;

        Ok(Expression::Ternary {
            condition: Box::new(condition),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        })
    }

    fn parse_case_arm(&mut self) -> Result<CaseArm, ParseError> {
        let pattern = self.parse_pattern()?;

        self.expect_token(&Token::Arrow, "Expected '->' after pattern")?;

        let body = self.parse_expression()?;

        Ok(CaseArm { pattern, body })
    }

    fn parse_when_arm(&mut self) -> Result<WhenArm, ParseError> {
        let condition = self.parse_condition_expression()?;

        self.expect_token(&Token::Arrow, "Expected '->' after when condition")?;

        let body = self.parse_expression()?;

        Ok(WhenArm { condition, body })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        self.parse_pattern_primary()
    }

    fn parse_pattern_primary(&mut self) -> Result<Pattern, ParseError> {
        match self.current_token() {
            Some(Token::Integer(n)) => {
                let n = *n;
                self.advance();
                Ok(Pattern::Literal(Literal::Integer(n)))
            }
            Some(Token::Float(f)) => {
                let f = *f;
                self.advance();
                Ok(Pattern::Literal(Literal::Float(f)))
            }
            Some(Token::String(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Pattern::Literal(Literal::String(s)))
            }
            Some(Token::True) => {
                self.advance();
                Ok(Pattern::Literal(Literal::Boolean(true)))
            }
            Some(Token::False) => {
                self.advance();
                Ok(Pattern::Literal(Literal::Boolean(false)))
            }
            Some(Token::Underscore) => {
                self.advance();
                Ok(Pattern::Wildcard)
            }
            Some(Token::Identifier(name)) => {
                let name = name.clone();
                self.advance();

                // Check if this is a constructor pattern
                if self.check_token(&Token::LeftParen) {
                    self.parse_constructor_pattern(name)
                } else {
                    Ok(Pattern::Identifier(name))
                }
            }
            Some(Token::LeftParen) => {
                self.advance(); // consume '('
                let pattern = self.parse_pattern()?;
                self.expect_token(
                    &Token::RightParen,
                    "Expected ')' after parenthesized pattern",
                )?;
                Ok(pattern)
            }
            _ => Err(ParseError {
                message: "Expected pattern".to_string(),
                position: self.position,
            }),
        }
    }

    fn parse_constructor_pattern(&mut self, name: String) -> Result<Pattern, ParseError> {
        self.advance(); // consume '('

        let mut args = Vec::new();

        while !self.check_token(&Token::RightParen) && !self.is_at_end() {
            // Handle special wildcard syntax with multiple underscores: Customer(_, *, _)
            if self.check_token(&Token::Underscore) {
                self.advance();
                args.push(Pattern::Wildcard);
            } else if self.check_token(&Token::Multiply) {
                // Handle "rest" pattern: * means "match any remaining fields"
                self.advance();
                // For now, treat * as a wildcard - in a full implementation this would
                // be a special "rest" pattern that matches remaining constructor fields
                args.push(Pattern::Wildcard);
            } else {
                // Parse nested patterns recursively
                let nested_pattern = self.parse_pattern_primary()?;
                args.push(nested_pattern);
            }

            if self.check_token(&Token::Comma) {
                self.advance();
            } else if !self.check_token(&Token::RightParen) {
                return Err(ParseError {
                    message: "Expected ',' or ')' in constructor pattern".to_string(),
                    position: self.position,
                });
            }
        }

        self.expect_token(
            &Token::RightParen,
            "Expected ')' after constructor arguments",
        )?;

        Ok(Pattern::Constructor { name, args })
    }

    fn parse_condition_expression(&mut self) -> Result<Expression, ParseError> {
        // Parse a simple comparison expression for when conditions
        let left = self.parse_expression()?;

        // Check for comparison operators
        match self.current_token() {
            Some(Token::Equal) => {
                self.advance();
                let right = self.parse_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::Equal,
                    right: Box::new(right),
                })
            }
            Some(Token::NotEqual) => {
                self.advance();
                let right = self.parse_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::NotEqual,
                    right: Box::new(right),
                })
            }
            Some(Token::Less) => {
                self.advance();
                let right = self.parse_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::Less,
                    right: Box::new(right),
                })
            }
            Some(Token::Greater) => {
                self.advance();
                let right = self.parse_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::Greater,
                    right: Box::new(right),
                })
            }
            Some(Token::LessEqual) => {
                self.advance();
                let right = self.parse_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::LessEqual,
                    right: Box::new(right),
                })
            }
            Some(Token::GreaterEqual) => {
                self.advance();
                let right = self.parse_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::GreaterEqual,
                    right: Box::new(right),
                })
            }
            _ => Ok(left), // No comparison operator, return as-is
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        // Check for case expression first
        if self.check_token(&Token::Case) {
            self.parse_case_expression()
        } else if self.check_token(&Token::LeftBrace) {
            self.parse_block_expression()
        } else if self.check_token(&Token::Pipe) {
            // Check for lambda expression
            self.parse_lambda_expression()
        } else {
            let left = self.parse_binary_expression()?;

            // Check for pipeline operator
            if self.check_token(&Token::Pipeline) {
                self.parse_pipeline_expression(left)
            } else if self.check_token(&Token::Question) {
                // Check for ternary operator
                self.parse_ternary_expression(left)
            } else {
                Ok(left)
            }
        }
    }

    pub fn parse_block_expression(&mut self) -> Result<Expression, ParseError> {
        self.expect_token(&Token::LeftBrace, "Expected '{'")?;

        let mut statements = Vec::new();
        let mut final_expression = None;

        while !self.check_token(&Token::RightBrace) && !self.is_at_end() {
            // Try to parse as statement first
            if self.is_statement_start() {
                statements.push(self.parse_statement()?);
            } else {
                // This must be the final expression
                final_expression = Some(Box::new(self.parse_expression()?));
                break;
            }
        }

        self.expect_token(&Token::RightBrace, "Expected '}' after block")?;

        Ok(Expression::Block {
            statements,
            expression: final_expression,
        })
    }

    pub fn parse_lambda_expression(&mut self) -> Result<Expression, ParseError> {
        self.expect_token(&Token::Pipe, "Expected '|'")?;

        let mut params = Vec::new();
        while !self.check_token(&Token::Pipe) && !self.is_at_end() {
            if let Some(Token::Identifier(param)) = self.current_token() {
                params.push(param.clone());
                self.advance();

                if self.check_token(&Token::Comma) {
                    self.advance();
                } else if !self.check_token(&Token::Pipe) {
                    return Err(ParseError {
                        message: "Expected ',' or '|' in lambda parameters".to_string(),
                        position: self.position,
                    });
                }
            } else {
                return Err(ParseError {
                    message: "Expected parameter name in lambda".to_string(),
                    position: self.position,
                });
            }
        }

        self.expect_token(&Token::Pipe, "Expected closing '|'")?;

        let body = self.parse_expression()?;

        Ok(Expression::Lambda {
            params,
            body: Box::new(body),
        })
    }

    pub fn parse_pipeline_expression(
        &mut self,
        left: Expression,
    ) -> Result<Expression, ParseError> {
        let mut result = left;

        while self.check_token(&Token::Pipeline) {
            self.advance(); // consume '|>'

            // Parse the right side - use parse_simple_expression to handle lambdas
            let right = if self.check_token(&Token::Pipe) {
                // This is a lambda expression
                self.parse_lambda_expression()?
            } else {
                // This is a regular expression or function call
                self.parse_simple_expression()?
            };

            // Transform into a function call
            result = match right {
                // If right side is already a function call: add(5) -> add(left, 5)
                Expression::Call { function, args } => {
                    let mut new_args = vec![result];
                    new_args.extend(args);
                    Expression::Call {
                        function,
                        args: new_args,
                    }
                }
                // If right side is a simple expression: func -> func(left)
                _ => Expression::Call {
                    function: Box::new(right),
                    args: vec![result],
                },
            };
        }

        Ok(result)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        // Check if this is an assignment statement
        if let Some(Token::Identifier(var_name)) = self.current_token() {
            if let Some(Token::Assign) = self.peek_token() {
                // Parse assignment statement: identifier = expression
                let name = var_name.clone();
                self.advance(); // consume identifier
                self.advance(); // consume '='

                let value = self.parse_expression()?;

                return Ok(Statement::VariableDecl(VariableDecl {
                    name,
                    mutability: Mutability::Immutable,
                    type_annotation: None,
                    value,
                }));
            }
        }

        // Check for live/keep mutability keywords
        if matches!(self.current_token(), Some(Token::Live) | Some(Token::Keep)) {
            let mutability = match self.current_token() {
                Some(Token::Live) => Mutability::Live,
                Some(Token::Keep) => Mutability::Keep,
                _ => Mutability::Immutable,
            };
            self.advance(); // consume mutability keyword

            if let Some(Token::Identifier(var_name)) = self.current_token() {
                let name = var_name.clone();
                self.advance(); // consume identifier

                // Optional type annotation
                let type_annotation = if self.check_token(&Token::Colon) {
                    self.advance(); // consume ':'
                    Some(self.parse_type()?)
                } else {
                    None
                };

                self.expect_token(&Token::Assign, "Expected '=' after variable name")?;
                let value = self.parse_expression()?;

                return Ok(Statement::VariableDecl(VariableDecl {
                    name,
                    mutability,
                    type_annotation,
                    value,
                }));
            }
        }

        // Otherwise, parse as expression statement
        let expr = self.parse_expression()?;
        Ok(Statement::Expression(expr))
    }

    fn is_statement_start(&self) -> bool {
        // Simple heuristic: if the current sequence looks like it could be a statement
        // For now, we'll be conservative and only treat some patterns as statements
        match self.current_token() {
            Some(Token::Live) | Some(Token::Keep) => true,
            Some(Token::Identifier(_)) => {
                // Look ahead to see if this looks like an assignment
                if let Some(Token::Assign) = self.peek_token() {
                    true
                } else if let Some(Token::Colon) = self.peek_token() {
                    // Could be type annotation
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.current_token() {
            Some(Token::Identifier(name)) => {
                let type_name = name.clone();
                self.advance();
                Ok(Type::Named(type_name))
            }
            _ => Err(ParseError {
                message: "Expected type name".to_string(),
                position: self.position,
            }),
        }
    }

    fn parse_binary_expression(&mut self) -> Result<Expression, ParseError> {
        let left = self.parse_simple_expression()?;

        // Check for binary operators
        match self.current_token() {
            Some(Token::Equal) => {
                self.advance();
                let right = self.parse_simple_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::Equal,
                    right: Box::new(right),
                })
            }
            Some(Token::NotEqual) => {
                self.advance();
                let right = self.parse_simple_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::NotEqual,
                    right: Box::new(right),
                })
            }
            Some(Token::Less) => {
                self.advance();
                let right = self.parse_simple_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::Less,
                    right: Box::new(right),
                })
            }
            Some(Token::Greater) => {
                self.advance();
                let right = self.parse_simple_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::Greater,
                    right: Box::new(right),
                })
            }
            Some(Token::LessEqual) => {
                self.advance();
                let right = self.parse_simple_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::LessEqual,
                    right: Box::new(right),
                })
            }
            Some(Token::GreaterEqual) => {
                self.advance();
                let right = self.parse_simple_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::GreaterEqual,
                    right: Box::new(right),
                })
            }
            Some(Token::Plus) => {
                self.advance();
                let right = self.parse_simple_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::Add,
                    right: Box::new(right),
                })
            }
            Some(Token::Minus) => {
                self.advance();
                let right = self.parse_simple_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::Sub,
                    right: Box::new(right),
                })
            }
            Some(Token::Multiply) => {
                self.advance();
                let right = self.parse_simple_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::Mul,
                    right: Box::new(right),
                })
            }
            Some(Token::Divide) => {
                self.advance();
                let right = self.parse_simple_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::Div,
                    right: Box::new(right),
                })
            }
            Some(Token::Modulo) => {
                self.advance();
                let right = self.parse_simple_expression()?;
                Ok(Expression::Binary {
                    left: Box::new(left),
                    op: BinaryOp::Mod,
                    right: Box::new(right),
                })
            }
            _ => Ok(left), // No binary operator, return as-is
        }
    }

    fn parse_simple_expression(&mut self) -> Result<Expression, ParseError> {
        match self.current_token() {
            Some(Token::Integer(n)) => {
                let n = *n;
                self.advance();
                Ok(Expression::Literal(Literal::Integer(n)))
            }
            Some(Token::Float(f)) => {
                let f = *f;
                self.advance();
                Ok(Expression::Literal(Literal::Float(f)))
            }
            Some(Token::String(s)) => {
                let s = s.clone();
                self.advance();

                // Check if this string contains interpolation syntax
                if has_interpolation(&s) {
                    self.parse_string_interpolation_from_string(&s)
                } else {
                    Ok(Expression::Literal(Literal::String(s)))
                }
            }
            Some(Token::True) => {
                self.advance();
                Ok(Expression::Literal(Literal::Boolean(true)))
            }
            Some(Token::False) => {
                self.advance();
                Ok(Expression::Literal(Literal::Boolean(false)))
            }
            Some(Token::Identifier(name)) => {
                let name = name.clone();
                self.advance();

                // Check for constructor, function call or method call
                if self.check_token(&Token::LeftParen) {
                    self.advance(); // consume '('

                    let mut args = Vec::new();
                    let mut constructor_args = Vec::new();
                    let mut is_constructor = false;

                    // Check if this looks like a constructor (named arguments)
                    if !self.check_token(&Token::RightParen) {
                        // Peek ahead to see if we have "name:" pattern
                        if let Some(Token::Identifier(_)) = self.current_token() {
                            if let Some(Token::Colon) = self.peek_token() {
                                is_constructor = true;
                            }
                        }
                    }

                    if is_constructor {
                        // Parse constructor arguments with named fields
                        while !self.check_token(&Token::RightParen) && !self.is_at_end() {
                            if let Some(Token::Identifier(field_name)) = self.current_token() {
                                let field_name = field_name.clone();
                                self.advance();

                                if self.check_token(&Token::Colon) {
                                    self.advance(); // consume ':'
                                    let value = self.parse_expression()?;
                                    constructor_args.push(ConstructorArg {
                                        name: Some(field_name),
                                        value,
                                    });
                                } else {
                                    return Err(ParseError {
                                        message: "Expected ':' after field name in constructor"
                                            .to_string(),
                                        position: self.position,
                                    });
                                }
                            } else {
                                return Err(ParseError {
                                    message: "Expected field name in constructor".to_string(),
                                    position: self.position,
                                });
                            }

                            if self.check_token(&Token::Comma) {
                                self.advance();
                            } else if !self.check_token(&Token::RightParen) {
                                return Err(ParseError {
                                    message: "Expected ',' or ')' in constructor".to_string(),
                                    position: self.position,
                                });
                            }
                        }

                        self.expect_token(
                            &Token::RightParen,
                            "Expected ')' after constructor arguments",
                        )?;

                        return Ok(Expression::Constructor {
                            name,
                            args: constructor_args,
                        });
                    } else {
                        // Parse as regular function call
                        while !self.check_token(&Token::RightParen) && !self.is_at_end() {
                            args.push(self.parse_expression()?);

                            if self.check_token(&Token::Comma) {
                                self.advance();
                            } else if !self.check_token(&Token::RightParen) {
                                return Err(ParseError {
                                    message: "Expected ',' or ')' in function call".to_string(),
                                    position: self.position,
                                });
                            }
                        }

                        self.expect_token(
                            &Token::RightParen,
                            "Expected ')' after function arguments",
                        )?;
                    }

                    // Handle method chaining for function calls
                    let mut expr = Expression::Call {
                        function: Box::new(Expression::Identifier(name)),
                        args,
                    };

                    expr = self.parse_method_chain(expr)?;
                    Ok(expr)
                } else {
                    // Handle method chaining for identifiers
                    let mut expr = Expression::Identifier(name);
                    expr = self.parse_method_chain(expr)?;
                    Ok(expr)
                }
            }
            Some(Token::LeftParen) => {
                self.advance(); // consume '('
                let expr = self.parse_expression()?;
                self.expect_token(
                    &Token::RightParen,
                    "Expected ')' after parenthesized expression",
                )?;
                Ok(expr)
            }
            Some(Token::LeftBracket) => {
                self.advance(); // consume '['
                let mut elements = Vec::new();

                while !self.check_token(&Token::RightBracket) && !self.is_at_end() {
                    elements.push(self.parse_expression()?);

                    if self.check_token(&Token::Comma) {
                        self.advance();
                    } else if !self.check_token(&Token::RightBracket) {
                        return Err(ParseError {
                            message: "Expected ',' or ']' in list literal".to_string(),
                            position: self.position,
                        });
                    }
                }

                self.expect_token(&Token::RightBracket, "Expected ']' after list elements")?;
                Ok(Expression::List { elements })
            }
            Some(Token::LeftBrace) => {
                // Check if this is a block expression or a map literal
                // Look ahead to see if the first thing is an identifier followed by = (statement)
                // or an expression followed by : (map entry)
                let mut lookahead_pos = self.position + 1;
                let mut is_block = false;

                // Look at the first non-empty content after {
                if lookahead_pos < self.tokens.len() {
                    match &self.tokens[lookahead_pos] {
                        Token::Identifier(_) => {
                            // Check if next token is = (block) or : (map)
                            if lookahead_pos + 1 < self.tokens.len() {
                                match &self.tokens[lookahead_pos + 1] {
                                    Token::Assign => is_block = true,
                                    Token::Colon => is_block = false,
                                    _ => is_block = true, // Default to block for ambiguous cases
                                }
                            } else {
                                is_block = true;
                            }
                        }
                        Token::RightBrace => {
                            // Empty braces - treat as empty block
                            is_block = true;
                        }
                        _ => {
                            // If it doesn't start with identifier, it's likely a map or expression
                            is_block = false;
                        }
                    }
                }

                if is_block {
                    // Parse as block expression
                    self.parse_block_expression()
                } else {
                    // Parse as map literal
                    self.advance(); // consume '{'
                    let mut pairs = Vec::new();

                    while !self.check_token(&Token::RightBrace) && !self.is_at_end() {
                        let key = self.parse_expression()?;
                        self.expect_token(&Token::Colon, "Expected ':' after map key")?;
                        let value = self.parse_expression()?;
                        pairs.push((key, value));

                        if self.check_token(&Token::Comma) {
                            self.advance();
                        } else if !self.check_token(&Token::RightBrace) {
                            return Err(ParseError {
                                message: "Expected ',' or '}' in map literal".to_string(),
                                position: self.position,
                            });
                        }
                    }

                    self.expect_token(&Token::RightBrace, "Expected '}' after map elements")?;
                    Ok(Expression::Map { pairs })
                }
            }
            _ => Err(ParseError {
                message: "Expected expression".to_string(),
                position: self.position,
            }),
        }
    }

    // Helper methods for token management
    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn peek_token(&self) -> Option<&Token> {
        self.tokens.get(self.position + 1)
    }

    fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            self.position += 1;
        }
        self.previous_token()
    }

    fn previous_token(&self) -> Option<&Token> {
        if self.position > 0 {
            self.tokens.get(self.position - 1)
        } else {
            None
        }
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }

    fn check_token(&self, token: &Token) -> bool {
        if self.is_at_end() {
            false
        } else {
            std::mem::discriminant(self.current_token().unwrap()) == std::mem::discriminant(token)
        }
    }

    fn expect_token(&mut self, expected: &Token, message: &str) -> Result<(), ParseError> {
        if self.check_token(expected) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError {
                message: format!("{}, found {:?}", message, self.current_token()),
                position: self.position,
            })
        }
    }
}

// Utility function to extract tokens for case/when expressions from a token stream
pub fn extract_balanced_expression(
    tokens: &[Token],
    start: usize,
) -> Result<(Vec<Token>, usize), ParseError> {
    let mut depth = 0;
    let mut end = start;
    let mut result = Vec::new();

    while end < tokens.len() {
        match &tokens[end] {
            Token::LeftBrace => {
                depth += 1;
                result.push(tokens[end].clone());
            }
            Token::RightBrace => {
                if depth == 1 {
                    // Found the matching closing brace
                    result.push(tokens[end].clone());
                    return Ok((result, end + 1));
                } else {
                    depth -= 1;
                    result.push(tokens[end].clone());
                }
            }
            _ => {
                result.push(tokens[end].clone());
            }
        }
        end += 1;
    }

    Err(ParseError {
        message: "Unmatched opening brace".to_string(),
        position: start,
    })
}

impl ManualParser {
    fn parse_string_interpolation_from_string(
        &mut self,
        content: &str,
    ) -> Result<Expression, ParseError> {
        let mut parts = Vec::new();
        let mut current_literal = String::new();
        let mut chars = content.char_indices().peekable();
        let mut brace_count = 0;
        let mut in_interpolation = false;
        let mut interpolation_content = String::new();

        while let Some((_, ch)) = chars.next() {
            match ch {
                '{' if !in_interpolation => {
                    // Start of interpolation
                    if !current_literal.is_empty() {
                        parts.push(StringPart::Literal(current_literal.clone()));
                        current_literal.clear();
                    }
                    in_interpolation = true;
                    brace_count = 1;
                }
                '{' if in_interpolation => {
                    brace_count += 1;
                    interpolation_content.push(ch);
                }
                '}' if in_interpolation => {
                    brace_count -= 1;
                    if brace_count == 0 {
                        // End of interpolation
                        if !interpolation_content.is_empty() {
                            // Parse the interpolation content as an expression
                            let expr_tokens: Vec<Token> = Token::lexer(&interpolation_content)
                                .filter_map(|result| result.ok())
                                .collect();

                            if !expr_tokens.is_empty() {
                                // Parse the tokens into an expression
                                let mut expr_parser = ManualParser::new(expr_tokens);
                                match expr_parser.parse_expression() {
                                    Ok(expr) => {
                                        parts.push(StringPart::Expression(Box::new(expr)));
                                    }
                                    Err(_) => {
                                        // If parsing fails, treat as literal
                                        parts.push(StringPart::Literal(format!(
                                            "{{{}}}",
                                            interpolation_content
                                        )));
                                    }
                                }
                            }
                            interpolation_content.clear();
                        }
                        in_interpolation = false;
                    } else {
                        interpolation_content.push(ch);
                    }
                }
                _ if in_interpolation => {
                    interpolation_content.push(ch);
                }
                _ => {
                    current_literal.push(ch);
                }
            }
        }

        // Add any remaining literal content
        if !current_literal.is_empty() {
            parts.push(StringPart::Literal(current_literal));
        }

        // If no parts were found, return a simple literal
        if parts.is_empty() {
            parts.push(StringPart::Literal(content.to_string()));
        }

        Ok(Expression::StringInterpolation { parts })
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut items = Vec::new();

        while !self.is_at_end() {
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(e) => return Err(e),
            }
        }

        Ok(Program { items })
    }

    pub fn parse_item(&mut self) -> Result<Item, ParseError> {
        // Try to parse different types of items
        if let Some(Token::Identifier(_)) = self.current_token() {
            // Could be function or variable
            if let Some(Token::LeftParen) = self.peek_token() {
                self.parse_function()
            } else {
                self.parse_variable()
            }
        } else if matches!(self.current_token(), Some(Token::Live) | Some(Token::Keep)) {
            self.parse_variable()
        } else {
            Err(ParseError {
                message: "Expected function or variable declaration".to_string(),
                position: self.position,
            })
        }
    }

    fn parse_function(&mut self) -> Result<Item, ParseError> {
        // Function name
        let name = match self.current_token() {
            Some(Token::Identifier(n)) => n.clone(),
            _ => {
                return Err(ParseError {
                    message: "Expected function name".to_string(),
                    position: self.position,
                })
            }
        };
        self.advance();

        // Parameters
        self.expect_token(&Token::LeftParen, "Expected '(' after function name")?;
        let mut params = Vec::new();

        while !self.check_token(&Token::RightParen) && !self.is_at_end() {
            if let Some(Token::Identifier(param_name)) = self.current_token() {
                params.push(Parameter {
                    name: param_name.clone(),
                    type_annotation: None,
                    mutability: None,
                });
                self.advance();

                if self.check_token(&Token::Comma) {
                    self.advance();
                }
            } else {
                return Err(ParseError {
                    message: "Expected parameter name".to_string(),
                    position: self.position,
                });
            }
        }

        self.expect_token(&Token::RightParen, "Expected ')' after parameters")?;

        // Optional return type
        let return_type = if self.check_token(&Token::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        // Function body
        let body = if self.check_token(&Token::Assign) {
            self.advance();
            let expr = self.parse_expression()?;
            FunctionBody::Expression(expr)
        } else if self.check_token(&Token::LeftBrace) {
            self.advance();
            let mut statements = Vec::new();

            while !self.check_token(&Token::RightBrace) && !self.is_at_end() {
                statements.push(self.parse_statement()?);
            }

            self.expect_token(&Token::RightBrace, "Expected '}' after function body")?;
            FunctionBody::Block(statements)
        } else {
            return Err(ParseError {
                message: "Expected function body (= expr or { ... })".to_string(),
                position: self.position,
            });
        };

        Ok(Item::Function(Function {
            name,
            params,
            return_type,
            body,
        }))
    }

    fn parse_variable(&mut self) -> Result<Item, ParseError> {
        // Check for mutability keywords
        let mutability = if self.check_token(&Token::Live) {
            self.advance();
            Mutability::Live
        } else if self.check_token(&Token::Keep) {
            self.advance();
            Mutability::Keep
        } else {
            Mutability::Immutable
        };

        // Variable name
        let name = match self.current_token() {
            Some(Token::Identifier(n)) => n.clone(),
            _ => {
                return Err(ParseError {
                    message: "Expected variable name".to_string(),
                    position: self.position,
                })
            }
        };
        self.advance();

        // Optional type annotation
        let type_annotation = if self.check_token(&Token::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        // Assignment
        self.expect_token(&Token::Assign, "Expected '=' after variable name")?;
        let value = self.parse_expression()?;

        Ok(Item::VariableDecl(VariableDecl {
            name,
            mutability,
            type_annotation,
            value,
        }))
    }

    fn parse_method_chain(&mut self, mut expr: Expression) -> Result<Expression, ParseError> {
        while self.check_token(&Token::Dot) {
            self.advance(); // consume '.'

            if let Some(Token::Identifier(method)) = self.current_token() {
                let method = method.clone();
                self.advance();

                if self.check_token(&Token::LeftParen) {
                    // Method call: .method(args)
                    self.advance(); // consume '('

                    let mut method_args = Vec::new();
                    while !self.check_token(&Token::RightParen) && !self.is_at_end() {
                        method_args.push(self.parse_expression()?);

                        if self.check_token(&Token::Comma) {
                            self.advance();
                        } else if !self.check_token(&Token::RightParen) {
                            return Err(ParseError {
                                message: "Expected ',' or ')' in method call".to_string(),
                                position: self.position,
                            });
                        }
                    }

                    self.expect_token(&Token::RightParen, "Expected ')' after method arguments")?;

                    expr = Expression::MethodCall {
                        object: Box::new(expr),
                        method,
                        args: method_args,
                    };
                } else {
                    // Field access: .field
                    expr = Expression::FieldAccess {
                        object: Box::new(expr),
                        field: method,
                    };
                }
            } else {
                return Err(ParseError {
                    message: "Expected method name after '.'".to_string(),
                    position: self.position,
                });
            }
        }

        Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Token;

    #[test]
    fn test_simple_case_expression() {
        let tokens = vec![
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

        let mut parser = ManualParser::new(tokens);
        let result = parser.parse_case_expression().unwrap();

        match result {
            Expression::Case { expr, arms } => {
                assert!(matches!(expr.as_ref(), Expression::Identifier(_)));
                assert_eq!(arms.len(), 2);

                match &arms[0].pattern {
                    Pattern::Literal(Literal::Integer(42)) => (),
                    _ => panic!("Expected integer literal pattern"),
                }

                match &arms[1].pattern {
                    Pattern::Wildcard => (),
                    _ => panic!("Expected wildcard pattern"),
                }
            }
            _ => panic!("Expected case expression"),
        }
    }

    #[test]
    fn test_constructor_pattern() {
        let tokens = vec![
            Token::Case,
            Token::Identifier("result".to_string()),
            Token::LeftBrace,
            Token::Identifier("Some".to_string()),
            Token::LeftParen,
            Token::Identifier("value".to_string()),
            Token::RightParen,
            Token::Arrow,
            Token::Identifier("value".to_string()),
            Token::RightBrace,
        ];

        let mut parser = ManualParser::new(tokens);
        let result = parser.parse_case_expression().unwrap();

        match result {
            Expression::Case { arms, .. } => match &arms[0].pattern {
                Pattern::Constructor { name, args } => {
                    assert_eq!(name, "Some");
                    assert_eq!(args.len(), 1);
                    match &args[0] {
                        Pattern::Identifier(id) => assert_eq!(id, "value"),
                        _ => panic!("Expected identifier pattern"),
                    }
                }
                _ => panic!("Expected constructor pattern"),
            },
            _ => panic!("Expected case expression"),
        }
    }

    #[test]
    fn test_when_expression() {
        let tokens = vec![
            Token::When,
            Token::Identifier("score".to_string()),
            Token::LeftBrace,
            Token::Identifier("score".to_string()),
            Token::GreaterEqual,
            Token::Integer(90),
            Token::Arrow,
            Token::String("A".to_string()),
            Token::Identifier("score".to_string()),
            Token::GreaterEqual,
            Token::Integer(80),
            Token::Arrow,
            Token::String("B".to_string()),
            Token::RightBrace,
        ];

        let mut parser = ManualParser::new(tokens);
        let result = parser.parse_when_expression().unwrap();

        match result {
            Expression::When { expr, arms } => {
                assert!(matches!(expr.as_ref(), Expression::Identifier(_)));
                assert_eq!(arms.len(), 2);
            }
            _ => panic!("Expected when expression"),
        }
    }

    #[test]
    fn test_ternary_expression() {
        let tokens = vec![
            Token::Identifier("condition".to_string()),
            Token::Question,
            Token::String("yes".to_string()),
            Token::Colon,
            Token::String("no".to_string()),
        ];

        let mut parser = ManualParser::new(tokens);
        let result = parser.parse_expression().unwrap();

        match result {
            Expression::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                assert!(matches!(condition.as_ref(), Expression::Identifier(_)));
                assert!(matches!(
                    then_expr.as_ref(),
                    Expression::Literal(Literal::String(_))
                ));
                assert!(matches!(
                    else_expr.as_ref(),
                    Expression::Literal(Literal::String(_))
                ));
            }
            _ => panic!("Expected ternary expression"),
        }
    }

    #[test]
    fn test_nested_ternary() {
        let tokens = vec![
            Token::Identifier("x".to_string()),
            Token::Greater,
            Token::Integer(0),
            Token::Question,
            Token::Identifier("x".to_string()),
            Token::Colon,
            Token::Integer(0),
        ];

        let mut parser = ManualParser::new(tokens);
        let result = parser.parse_expression().unwrap();

        match result {
            Expression::Ternary { condition, .. } => {
                // The condition should be a binary expression (x > 0)
                assert!(matches!(condition.as_ref(), Expression::Binary { .. }));
            }
            _ => panic!("Expected ternary expression with binary condition"),
        }
    }
}
