use crate::ast::*;
use crate::lexer::Token;

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
        let expr = self.parse_simple_expression()?;

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
        let expr = self.parse_simple_expression()?;

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
        let then_expr = self.parse_simple_expression()?;

        // Expect ':'
        self.expect_token(&Token::Colon, "Expected ':' after ternary then expression")?;

        // Parse else expression
        let else_expr = self.parse_simple_expression()?;

        Ok(Expression::Ternary {
            condition: Box::new(condition),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        })
    }

    fn parse_case_arm(&mut self) -> Result<CaseArm, ParseError> {
        let pattern = self.parse_pattern()?;

        self.expect_token(&Token::Arrow, "Expected '->' after pattern")?;

        let body = self.parse_simple_expression()?;

        Ok(CaseArm { pattern, body })
    }

    fn parse_when_arm(&mut self) -> Result<WhenArm, ParseError> {
        let condition = self.parse_condition_expression()?;

        self.expect_token(&Token::Arrow, "Expected '->' after when condition")?;

        let body = self.parse_simple_expression()?;

        Ok(WhenArm { condition, body })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
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
                    self.advance(); // consume '('

                    let mut args = Vec::new();
                    while !self.check_token(&Token::RightParen) && !self.is_at_end() {
                        args.push(self.parse_pattern()?);

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
                } else {
                    Ok(Pattern::Identifier(name))
                }
            }
            _ => Err(ParseError {
                message: "Expected pattern".to_string(),
                position: self.position,
            }),
        }
    }

    fn parse_condition_expression(&mut self) -> Result<Expression, ParseError> {
        // Parse a simple comparison expression for when conditions
        let left = self.parse_simple_expression()?;

        // Check for comparison operators
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
            _ => Ok(left), // No comparison operator, return as-is
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
                Ok(Expression::Literal(Literal::String(s)))
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

                // Check for function call
                if self.check_token(&Token::LeftParen) {
                    self.advance(); // consume '('

                    let mut args = Vec::new();
                    while !self.check_token(&Token::RightParen) && !self.is_at_end() {
                        args.push(self.parse_simple_expression()?);

                        if self.check_token(&Token::Comma) {
                            self.advance();
                        } else if !self.check_token(&Token::RightParen) {
                            return Err(ParseError {
                                message: "Expected ',' or ')' in function call".to_string(),
                                position: self.position,
                            });
                        }
                    }

                    self.expect_token(&Token::RightParen, "Expected ')' after function arguments")?;

                    Ok(Expression::Call {
                        function: Box::new(Expression::Identifier(name)),
                        args,
                    })
                } else {
                    Ok(Expression::Identifier(name))
                }
            }
            Some(Token::LeftParen) => {
                self.advance(); // consume '('
                let expr = self.parse_simple_expression()?;
                self.expect_token(
                    &Token::RightParen,
                    "Expected ')' after parenthesized expression",
                )?;
                Ok(expr)
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
}
