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
    Null,

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

    pub fn scan(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();

        while self.pos < self.input.len() {
            let token = self.next_token()?;
            if token == Token::Eof {
                break;
            }
            tokens.push(token);
        }

        Ok(tokens)
    }

    pub fn next_token(&mut self) -> Result<Token> {
        while self.pos < self.input.len() {
            let current_char = self.current_char();

            if current_char.is_whitespace() {
                self.skip_whitespace();
                continue;
            }

            if current_char.is_ascii_digit() {
                return Ok(Token::Number(self.number()?));
            }

            if current_char == '"' {
                return Ok(Token::StringLiteral(self.string_literal()?));
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
                    if self.peek_next() == Some('/') {
                        while self.pos < self.input.len() && self.current_char() != '\n' {
                            self.pos += 1;
                        }
                        continue; // Skip the rest of the line
                    }
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
                '=' => {
                    self.pos += 1;
                    if self.current_char() == '=' {
                        self.pos += 1;
                        return Ok(Token::Equal);
                    }
                    return Ok(Token::Assign);
                }
                ';' => {
                    self.pos += 1;
                    return Ok(Token::Semicolon);
                }
                '>' => {
                    self.pos += 1;
                    if self.current_char() == '=' {
                        self.pos += 1;
                        return Ok(Token::GreaterEqual);
                    }
                    return Ok(Token::Greater);
                }
                '<' => {
                    self.pos += 1;
                    if self.current_char() == '=' {
                        self.pos += 1;
                        return Ok(Token::LessEqual);
                    }
                    return Ok(Token::Less);
                }
                '!' => {
                    self.pos += 1;
                    if self.current_char() == '=' {
                        self.pos += 1;
                        return Ok(Token::NotEqual);
                    }
                    return Ok(Token::Not);
                }
                '&' => {
                    self.pos += 1;
                    if self.current_char() == '&' {
                        self.pos += 1;
                        return Ok(Token::And);
                    }
                }
                '|' => {
                    self.pos += 1;
                    if self.current_char() == '|' {
                        self.pos += 1;
                        return Ok(Token::Or);
                    }
                }
                _ => {}
            }

            if current_char.is_alphanumeric() {
                return Ok(self.identifier());
            }

            return Err(eyre!("Invalid character: {}", current_char));
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

    fn identifier(&mut self) -> Token {
        let start_pos = self.pos;
        while self.pos < self.input.len() && self.current_char().is_alphanumeric() {
            self.pos += 1;
        }

        let identifier = &self.input[start_pos..self.pos];
        match identifier {
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "for" => Token::For,
            "return" => Token::Return,
            "function" => Token::Function,
            "var" => Token::Var,
            "let" => Token::Let,
            "const" => Token::Const,
            "class" => Token::Class,
            "true" => Token::Boolean(true),
            "false" => Token::Boolean(false),
            "null" => Token::Null,
            _ => Token::Identifier(identifier.to_string()),
        }
    }

    fn string_literal(&mut self) -> Result<String> {
        self.pos += 1;
        let start_pos = self.pos;
        while self.pos < self.input.len() && self.current_char() != '"' {
            self.pos += 1;
        }

        if self.at_end() {
            return Err(eyre!("Unterminated string literal"));
        }

        let string = &self.input[start_pos..self.pos];
        self.pos += 1; // Skip the closing quote
        Ok(string.to_string())
    }

    fn number(&mut self) -> Result<f64> {
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
            .map_err(|_| eyre!("Invalid number: {}", number_str))
    }

    fn peek_next(&self) -> Option<char> {
        if self.pos + 1 < self.input.len() {
            Some(self.input[self.pos + 1..].chars().next().unwrap())
        } else {
            None
        }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.input.len()
    }
}
