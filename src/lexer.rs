use std::{fmt::Display, hash::Hash};

use ordered_float::OrderedFloat;

use crate::error::{LexerError, Result};

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum TokenKind {
    // Single-character tokens
    Assign,
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Comma,

    // Logical operators
    And,
    Or,
    Not,
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    If,
    Else,
    While,
    For,
    Return,
    Function,
    Let,
    Const,
    Class,
    Nil,
    True,
    False,

    Eof,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub literal: Option<Literal>,
    pub location: Location,
}

#[derive(Debug, Clone, Eq)]
pub enum Literal {
    String(String),
    Number(OrderedFloat<f64>),
    Boolean(bool),
    Nil,
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

impl Hash for Literal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Literal::String(s) => s.hash(state),
            Literal::Number(n) => n.hash(state),
            Literal::Boolean(b) => b.hash(state),
            Literal::Nil => 0.hash(state),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(s) => write!(f, "{}", s),
            Literal::Number(n) => write!(f, "{}", n),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    line: usize,
    column: usize,
}

impl Lexer<'_> {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            input,
            pos: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn scan(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();

        while self.pos < self.input.len() {
            if let Some(token) = self.next_token()? {
                let at_end = token.kind == TokenKind::Eof;
                if at_end {
                    break;
                }
                tokens.push(token);
            }
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            lexeme: String::new(),
            literal: None,
            location: Location {
                line: self.line,
                column: self.column,
            },
        });

        Ok(tokens)
    }

    pub fn next_token(&mut self) -> Result<Option<Token>> {
        // Skip whitespace
        while self.current_char().is_ascii_whitespace() {
            if self.current_char() == '\n' {
                self.line += 1;
                self.column = 1;
            }
            self.advance();

            if self.at_end() {
                return Ok(Some(Token {
                    kind: TokenKind::Eof,
                    lexeme: String::new(),
                    literal: None,
                    location: Location {
                        line: self.line,
                        column: self.column,
                    },
                }));
            }
        }

        let current_char = self.current_char();
        let start = self.pos;
        self.advance();

        let mut literal: Option<Literal> = None;
        let kind = match current_char {
            '+' => Ok(TokenKind::Plus),
            '-' => Ok(TokenKind::Minus),
            '*' => Ok(TokenKind::Multiply),
            '(' => Ok(TokenKind::LParen),
            ')' => Ok(TokenKind::RParen),
            '{' => Ok(TokenKind::LBrace),
            '}' => Ok(TokenKind::RBrace),
            ';' => Ok(TokenKind::Semicolon),
            ',' => Ok(TokenKind::Comma),
            '/' => {
                if self.current_char() == '/' {
                    while self.pos < self.input.len() && self.current_char() != '\n' {
                        self.advance();
                    }
                    self.advance();
                    return Ok(None);
                }
                Ok(TokenKind::Divide)
            }
            '=' => {
                if self.current_char() == '=' {
                    self.advance();
                    Ok(TokenKind::Equal)
                } else {
                    Ok(TokenKind::Assign)
                }
            }
            '>' => {
                if self.current_char() == '=' {
                    self.advance();
                    Ok(TokenKind::GreaterEqual)
                } else {
                    Ok(TokenKind::Greater)
                }
            }
            '<' => {
                if self.current_char() == '=' {
                    self.advance();
                    Ok(TokenKind::LessEqual)
                } else {
                    Ok(TokenKind::Less)
                }
            }
            '!' => {
                if self.current_char() == '=' {
                    self.advance();
                    Ok(TokenKind::NotEqual)
                } else {
                    Ok(TokenKind::Not)
                }
            }
            '&' => {
                if self.current_char() == '&' {
                    self.advance();
                    Ok(TokenKind::And)
                } else {
                    Err(LexerError::UnexpectedCharacter(
                        self.current_char(),
                        self.location(),
                    ))
                }
            }
            '|' => {
                if self.current_char() == '|' {
                    self.advance();
                    Ok(TokenKind::Or)
                } else {
                    Err(LexerError::UnexpectedCharacter(
                        self.current_char(),
                        self.location(),
                    ))
                }
            }
            '0'..='9' => {
                self.back();
                literal = Some(self.number()?);
                Ok(TokenKind::Number)
            }
            '"' => {
                self.back();
                literal = Some(self.string()?);
                Ok(TokenKind::String)
            }
            _ => {
                if current_char.is_alphanumeric() {
                    self.back();
                    let (kind, lit) = self.identifier();
                    literal = lit;
                    Ok(kind)
                } else {
                    Err(LexerError::UnexpectedCharacter(
                        current_char,
                        self.location(),
                    ))
                }
            }
        }?;

        Ok(Some(Token {
            kind,
            lexeme: self.input[start..self.pos].to_string(),
            literal,
            location: self.location(),
        }))
    }

    fn current_char(&self) -> char {
        self.input[self.pos..].chars().next().unwrap()
    }

    fn identifier(&mut self) -> (TokenKind, Option<Literal>) {
        let start_pos = self.pos;
        while self.pos < self.input.len() {
            if self.current_char().is_alphanumeric() || self.current_char() == '_' {
                self.pos += 1;
            } else {
                break;
            }
        }

        let identifier = &self.input[start_pos..self.pos];
        let mut literal: Option<Literal> = None;
        let kind = match identifier {
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "return" => TokenKind::Return,
            "fn" => TokenKind::Function,
            "let" => TokenKind::Let,
            "const" => TokenKind::Const,
            "class" => TokenKind::Class,
            "true" => {
                literal = Some(Literal::Boolean(true));
                TokenKind::True
            }
            "false" => {
                literal = Some(Literal::Boolean(false));
                TokenKind::False
            }
            "nil" => TokenKind::Nil,
            _ => TokenKind::Identifier,
        };

        (kind, literal)
    }

    fn string(&mut self) -> Result<Literal> {
        self.pos += 1;
        let start_pos = self.pos;
        while self.pos < self.input.len() && self.current_char() != '"' {
            self.pos += 1;
        }

        if self.at_end() {
            return Err(LexerError::UnterminatedString(self.location()).into());
        }

        let string = &self.input[start_pos..self.pos];
        self.pos += 1; // Skip the closing quote
        Ok(Literal::String(string.to_string()))
    }

    fn number(&mut self) -> Result<Literal> {
        let start_pos = self.pos;
        while self.pos < self.input.len() {
            let current_char = self.current_char();
            match current_char {
                '0'..='9' => self.pos += 1,
                '.' => {
                    if self.peek_next().is_some_and(|c| c.is_ascii_digit()) {
                        self.pos += 1; // Skip the dot
                    } else {
                        break; // End of number
                    }
                }
                _ => break, // End of number
            }
        }

        let number_str = &self.input[start_pos..self.pos];
        number_str
            .parse::<f64>()
            .map(|v| Literal::Number(OrderedFloat(v)))
            .map_err(|_| LexerError::InvalidNumber(number_str.into(), self.location()).into())
    }

    fn peek_next(&self) -> Option<char> {
        if self.pos + 1 < self.input.len() {
            self.input[self.pos + 1..].chars().next()
        } else {
            None
        }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn advance(&mut self) {
        if self.at_end() {
            return;
        }

        self.pos += 1;
        self.column += 1;
    }

    fn back(&mut self) {
        if self.pos > 0 {
            self.pos -= 1;
            self.column -= 1;
        }
    }

    fn location(&self) -> Location {
        Location {
            line: self.line,
            column: self.column,
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Assign => write!(f, "="),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Multiply => write!(f, "*"),
            TokenKind::Divide => write!(f, "/"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::And => write!(f, "&&"),
            TokenKind::Or => write!(f, "||"),
            TokenKind::Not => write!(f, "!"),
            TokenKind::Equal => write!(f, "=="),
            TokenKind::NotEqual => write!(f, "!="),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::Less => write!(f, "<"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::Identifier
            | TokenKind::String
            | TokenKind::Number
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::While
            | TokenKind::For
            | TokenKind::Return
            | TokenKind::Function
            | TokenKind::Let
            | TokenKind::Const
            | TokenKind::Class
            | TokenKind::Nil
            | TokenKind::True
            | TokenKind::False
            | TokenKind::Eof => write!(f, "{:?}", self),
        }
    }
}
