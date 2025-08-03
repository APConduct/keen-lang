# Implementing Missing Parser Features

This guide provides detailed approaches for adding the missing features to your Keen language parser: case/when expressions, ternary operators, and complex pattern matching.

## Current Status

✅ **Working Features:**
- Variable declarations with type inference
- Function definitions with parameters and type annotations
- Binary operations with correct precedence
- Function calls with multiple arguments
- Field access (object.field)
- Complex nested expressions
- Function types in type annotations
- Parenthesized expressions

🚧 **Missing Features:**
- Case/When expressions
- Ternary operators
- Complex pattern matching
- Nested patterns in constructors

## The Problem

The issue stems from Chumsky's parser combinator design where complex recursive structures can't be cloned. When we try to use `.clone()` on parsers that contain deeply nested recursive expressions, the Rust compiler can't satisfy the trait bounds.

## Recommended Approaches (Ranked by Practicality)

### 1. Manual Recursive Descent Parser (Best for Complex Features)

**Pros:** Complete control, handles recursion naturally, excellent error messages
**Cons:** More code to write, need to handle precedence manually

Create a separate module for hand-written parsers:

```rust
// src/manual_parser.rs
use crate::ast::*;
use crate::lexer::Token;

pub struct ManualParser {
    tokens: Vec<Token>,
    position: usize,
}

impl ManualParser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, position: 0 }
    }

    pub fn parse_case_expression(&mut self) -> Result<Expression, String> {
        self.expect_token(Token::Case)?;
        let expr = self.parse_expression()?;
        self.expect_token(Token::LeftBrace)?;
        
        let mut arms = Vec::new();
        while !self.check_token(&Token::RightBrace) {
            arms.push(self.parse_case_arm()?);
        }
        
        self.expect_token(Token::RightBrace)?;
        Ok(Expression::Case {
            expr: Box::new(expr),
            arms,
        })
    }

    pub fn parse_ternary(&mut self, condition: Expression) -> Result<Expression, String> {
        self.expect_token(Token::Question)?;
        let then_expr = self.parse_expression()?;
        self.expect_token(Token::Colon)?;
        let else_expr = self.parse_expression()?;
        
        Ok(Expression::Ternary {
            condition: Box::new(condition),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        })
    }

    // Implement precedence parsing for expressions
    pub fn parse_expression(&mut self) -> Result<Expression, String> {
        self.parse_ternary_expression()
    }

    fn parse_ternary_expression(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_logical_or()?;
        
        if self.check_token(&Token::Question) {
            expr = self.parse_ternary(expr)?;
        }
        
        Ok(expr)
    }

    // Continue with precedence levels...
}
```

**Integration:**
```rust
// In your main parser.rs
pub fn parse_with_fallback(tokens: Vec<Token>) -> Result<Program, Vec<String>> {
    // Try chumsky parser first
    let chumsky_parser = parser();
    match chumsky_parser.parse(tokens.clone()) {
        Ok(program) => Ok(program),
        Err(_) => {
            // Fall back to manual parser for complex expressions
            let mut manual = ManualParser::new(tokens);
            manual.parse_program()
        }
    }
}
```

### 2. Two-Pass Parsing (Easiest to Implement)

**Pros:** Keep existing chumsky parser, minimal changes
**Cons:** More complex, requires token buffering

```rust
// First pass: Parse structure and collect complex expression tokens
#[derive(Debug)]
enum PartialExpression {
    Simple(Expression),
    CaseTokens(Vec<Token>),
    WhenTokens(Vec<Token>),
    TernaryTokens(Vec<Token>),
}

fn parse_expression_two_pass() -> impl Parser<Token, PartialExpression, Error = Simple<Token>> {
    choice((
        // Simple expressions
        literal().map(PartialExpression::Simple),
        ident().map(PartialExpression::Simple),
        
        // Complex expressions as token sequences
        just(Token::Case)
            .ignore_then(collect_until_balanced_brace())
            .map(PartialExpression::CaseTokens),
            
        just(Token::When)
            .ignore_then(collect_until_balanced_brace())
            .map(PartialExpression::WhenTokens),
    ))
}

// Second pass: Parse collected tokens with manual parser
fn resolve_partial_expression(partial: PartialExpression) -> Result<Expression, String> {
    match partial {
        PartialExpression::Simple(expr) => Ok(expr),
        PartialExpression::CaseTokens(tokens) => {
            let mut manual = ManualParser::new(tokens);
            manual.parse_case_expression()
        },
        // ... handle other cases
    }
}
```

### 3. Switch to Different Parser Library (Long-term Solution)

**Pros:** More flexible, better recursion handling, often faster
**Cons:** Rewrite existing parser, learning curve

**Option A: Pest (Grammar-based)**
```pest
// grammar.pest
program = { SOI ~ item* ~ EOI }
item = { function | variable_decl | type_def }

expression = { ternary }
ternary = { logical_or ~ ("?" ~ logical_or ~ ":" ~ logical_or)? }
logical_or = { logical_and ~ ("||" ~ logical_and)* }
// ... precedence levels

case_expr = { "case" ~ expression ~ "{" ~ case_arm* ~ "}" }
case_arm = { pattern ~ "->" ~ expression }
pattern = { literal | identifier | constructor | wildcard }
```

**Option B: Nom (Combinator-based)**
```rust
use nom::{
    branch::alt,
    bytes::complete::tag,
    multi::many0,
    sequence::{delimited, tuple},
    IResult,
};

fn parse_case_expression(input: &[Token]) -> IResult<&[Token], Expression> {
    let (input, _) = tag(&[Token::Case])(input)?;
    let (input, expr) = parse_expression(input)?;
    let (input, _) = tag(&[Token::LeftBrace])(input)?;
    let (input, arms) = many0(parse_case_arm)(input)?;
    let (input, _) = tag(&[Token::RightBrace])(input)?;
    
    Ok((input, Expression::Case {
        expr: Box::new(expr),
        arms,
    }))
}
```

**Option C: LALRPOP (LR parser generator)**
```rust
// grammar.lalrpop
use crate::ast::*;

grammar;

pub Program: Program = {
    <items:Item*> => Program { items }
};

Tier<Op,NextTier>: Expression = {
    Tier<Op,NextTier> Op NextTier => Expression::Binary { 
        left: Box::new(<>), 
        op: <>, 
        right: Box::new(<>) 
    },
    NextTier
};

pub Expression = Tier<CompareOp, Factor>;
Factor = Tier<FactorOp, Term>;
// ...
```

### 4. Operator Precedence Parser (Good for Expressions)

**Pros:** Excellent for expression parsing, handles precedence naturally
**Cons:** Limited to expressions, still need other approaches for statements

```rust
#[derive(Debug, Clone, Copy, PartialEq)]
enum Precedence {
    None,
    Assignment,  // =
    Ternary,     // ? :
    Or,          // ||
    And,         // &&
    Equality,    // == !=
    Comparison,  // < > <= >=
    Term,        // + -
    Factor,      // * /
    Unary,       // ! -
    Call,        // . ()
    Primary,
}

impl Precedence {
    fn next(self) -> Self {
        match self {
            Self::None => Self::Assignment,
            Self::Assignment => Self::Ternary,
            Self::Ternary => Self::Or,
            // ... continue
            Self::Primary => Self::Primary,
        }
    }
}

pub struct PrecedenceParser {
    tokens: Vec<Token>,
    current: usize,
}

impl PrecedenceParser {
    pub fn parse_expression(&mut self) -> Result<Expression, String> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<Expression, String> {
        let mut left = self.parse_prefix()?;

        while let Some(op_precedence) = self.get_precedence() {
            if precedence > op_precedence {
                break;
            }

            left = self.parse_infix(left)?;
        }

        Ok(left)
    }

    fn parse_ternary(&mut self, condition: Expression) -> Result<Expression, String> {
        let then_expr = self.parse_expression()?;
        self.consume(Token::Colon, "Expected ':' after ternary then expression")?;
        let else_expr = self.parse_precedence(Precedence::Ternary)?;

        Ok(Expression::Ternary {
            condition: Box::new(condition),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        })
    }
}
```

### 5. AST Post-processing (Simplest for Start)

**Pros:** Keep existing parser mostly unchanged
**Cons:** Less elegant, may miss some syntax errors

```rust
// Parse simplified syntax first
#[derive(Debug)]
enum SimpleExpression {
    Regular(Expression),
    CaseStub { expr: Expression, body: String },
    TernaryStub { condition: Expression, parts: String },
}

// Transform in second pass
fn expand_expressions(simple: SimpleExpression) -> Result<Expression, String> {
    match simple {
        SimpleExpression::Regular(expr) => Ok(expr),
        SimpleExpression::CaseStub { expr, body } => {
            let case_tokens = tokenize_string(body)?;
            let mut parser = ManualParser::new(case_tokens);
            let arms = parser.parse_case_arms()?;
            Ok(Expression::Case {
                expr: Box::new(expr),
                arms,
            })
        },
        // ... handle other cases
    }
}
```

## Implementation Recommendation

For your specific case, I recommend **Approach #1 (Manual Recursive Descent)** for the following reasons:

1. **Incremental**: You can keep your existing Chumsky parser for the parts that work well
2. **Control**: Full control over error messages and parsing behavior
3. **Performance**: Often faster than combinator parsers for complex constructs
4. **Flexibility**: Easy to handle language-specific quirks and edge cases

## Step-by-Step Implementation Plan

### Phase 1: Set up Manual Parser Infrastructure
1. Create `src/manual_parser.rs`
2. Implement basic token management (current, peek, advance, expect)
3. Add error handling and position tracking

### Phase 2: Implement Simple Case Expressions
1. Parse `case expr { pattern -> expr ... }`
2. Handle basic patterns (literals, identifiers, wildcards)
3. Integrate with main parser as fallback

### Phase 3: Add Ternary Operators
1. Implement `condition ? then : else`
2. Handle precedence correctly
3. Test with complex nested expressions

### Phase 4: Complex Patterns
1. Add constructor patterns with arguments
2. Implement nested patterns
3. Add pattern exhaustiveness checking

### Phase 5: When Expressions
1. Parse `when expr { condition -> expr ... }`
2. Handle comparison operators in conditions
3. Add guard expressions

## Testing Strategy

Create comprehensive tests for each feature:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_case() {
        let input = r#"case x { 1 -> "one" _ -> "other" }"#;
        let tokens = tokenize(input).unwrap();
        let mut parser = ManualParser::new(tokens);
        let expr = parser.parse_case_expression().unwrap();
        
        match expr {
            Expression::Case { expr, arms } => {
                assert_eq!(arms.len(), 2);
                // ... more assertions
            }
            _ => panic!("Expected case expression"),
        }
    }

    #[test]
    fn test_nested_ternary() {
        let input = "a ? b ? c : d : e";
        // Test that it parses as a ? (b ? c : d) : e
    }

    #[test]
    fn test_complex_patterns() {
        let input = r#"case list { Some(User(name, age)) -> name _ -> "unknown" }"#;
        // Test nested constructor patterns
    }
}
```

## Migration Path

1. **Start Small**: Implement just literal patterns in case expressions
2. **Gradual Enhancement**: Add features incrementally (wildcards, then identifiers, then constructors)
3. **Keep Compatibility**: Ensure existing parser tests still pass
4. **Performance Testing**: Compare parsing speed between approaches
5. **Error Messages**: Ensure good error reporting for invalid syntax

This approach will give you a robust parser that can handle all the missing features while maintaining the benefits of your existing Chumsky-based implementation for simpler constructs.