use color_eyre::eyre::{eyre, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Single-character tokens
    Assign,
    Semicolon,
    Plus,
    Minus,
    Multiply,
    Divide,
    LParen,
    RParen,

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
    Identifier(String),
    StringLiteral(String),
    Number(f64),
    Boolean(bool),

    // Keywords
    If,
    Else,
    While,
    For,
    Return,
    Function,
    Var,
    Let,
    Const,
    Class,

    Eof,
}

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
}

impl Lexer<'_> {
    pub fn new(input: &str) -> Lexer {
        Lexer { input, pos: 0 }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        while self.pos < self.input.len() {
            let current_char = self.current_char();

            if current_char.is_whitespace() {
                self.skip_whitespace();
                continue;
            }

            if current_char.is_ascii_digit() {
                return Ok(Token::Number(self.number()));
            }

            if current_char == '"' {
                return Ok(Token::StringLiteral(self.string_literal()));
            }

            match current_char {
                '+' => {
                    self.pos += 1;
                    return Ok(Token::Plus);
                }
                '-' => {
                    self.pos += 1;
                    return Ok(Token::Minus);
                }
                '*' => {
                    self.pos += 1;
                    return Ok(Token::Multiply);
                }
                '/' => {
                    self.pos += 1;
                    return Ok(Token::Divide);
                }
                '(' => {
                    self.pos += 1;
                    return Ok(Token::LParen);
                }
                ')' => {
                    self.pos += 1;
                    return Ok(Token::RParen);
                }
                _ => {
                    return Err(eyre!("Invalid character: {}", current_char));
                }
            }
        }

        Ok(Token::Eof)
    }

    fn current_char(&self) -> char {
        self.input[self.pos..].chars().next().unwrap()
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len() && self.current_char().is_whitespace() {
            self.pos += 1;
        }
    }

    fn string_literal(&mut self) -> String {
        let start_pos = self.pos + 1; // Skip the opening quote
        while self.pos < self.input.len() && self.current_char() != '"' {
            self.pos += 1;
        }
        let string = &self.input[start_pos..self.pos];
        self.pos += 1; // Skip the closing quote
        string.to_string()
    }

    fn number(&mut self) -> f64 {
        let start_pos = self.pos;

        while self.pos < self.input.len() {
            let current_char = self.current_char();
            if current_char.is_ascii_digit() {
                self.pos += 1;
            }

            if current_char == '.' {
                if self.peek_next().is_some_and(|c| c.is_ascii_digit()) {
                    self.pos += 1; // Skip the dot
                } else {
                    break; // End of number
                }
            }
        }

        let number_str = &self.input[start_pos..self.pos];
        number_str.parse::<f64>().unwrap()
    }

    fn peek_next(&self) -> Option<char> {
        if self.pos + 1 < self.input.len() {
            Some(self.input[self.pos + 1..].chars().next().unwrap())
        } else {
            None
        }
    }
}
