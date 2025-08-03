use logos::Logos;
use std::hash::{Hash, Hasher};

// =============================================================================
// LEXER (Logos)
// =============================================================================

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_owned())]
    Identifier(String),

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().unwrap())]
    Integer(i64),

    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().unwrap())]
    Float(f64),

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_owned() // Remove quotes
    })]
    String(String),

    // Keywords
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("case")]
    Case,
    #[token("when")]
    When,
    #[token("distinct")]
    Distinct,
    #[token("type")]
    Type,
    #[token("live")]
    Live,
    #[token("keep")]
    Keep,

    // Operators and punctuation
    #[token("=")]
    Assign,
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("->")]
    Arrow,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("?")]
    Question,
    #[token("_")]
    Underscore,
    #[token("|")]
    Pipe,
    #[token("|>")]
    Pipeline,

    // Brackets
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,

    // Whitespace and comments (ignored)
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[regex(r"//[^\n]*", logos::skip)]
    #[regex(r"/\*([^*]|\*[^/])*\*/", logos::skip)]
    Whitespace,
}

impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Token::Identifier(s) => {
                0u8.hash(state);
                s.hash(state);
            }
            Token::Integer(i) => {
                1u8.hash(state);
                i.hash(state);
            }
            Token::Float(f) => {
                2u8.hash(state);
                // For f64, we'll use the bit representation
                f.to_bits().hash(state);
            }
            Token::String(s) => {
                3u8.hash(state);
                s.hash(state);
            }
            _ => {
                // For all other variants, just hash the discriminant
                std::mem::discriminant(self).hash(state);
            }
        }
    }
}

impl Eq for Token {}

/// Check if a string contains interpolation syntax
pub fn has_interpolation(s: &str) -> bool {
    s.contains('{') && s.contains('}')
}
