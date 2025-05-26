use thiserror::Error;

use crate::{
    environment::Value,
    lexer::{Location, TokenKind},
};

pub type Result<T> = color_eyre::Result<T, NovaError>;

#[derive(Debug, Error)]
pub enum NovaError {
    #[error(transparent)]
    Interpreter(#[from] InterpreterError),

    #[error(transparent)]
    Lexer(#[from] LexerError),

    #[error(transparent)]
    Parser(#[from] ParseError),

    #[error(transparent)]
    Report(#[from] color_eyre::Report),
}

#[derive(Debug, Error)]
pub enum InterpreterError {
    #[error("Undefined variable '{1}' at {0}")]
    UndefinedVariable(Location, String),

    #[error("Invalid operands for operator '{1}' at {0}")]
    OperatorNotSupported(Location, TokenKind),

    #[error("Return called outside of a function")]
    ReturnValue(Value),
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Expected token '{0}' at {1}, found '{2}'")]
    ExpectedToken(TokenKind, Location, TokenKind),

    #[error("Invalid assignment at {0}")]
    InvalidAssignment(Location),

    #[error("Expected expression at {0}")]
    ExpectedExpression(Location),

    #[error("Too many parameters at {0} (max 255, found {1})")]
    TooManyParameters(Location, usize, usize),
}

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("Unexpected character '{0}' at {1}")]
    UnexpectedCharacter(char, Location),

    #[error("Unterminated string literal at {0}")]
    UnterminatedString(Location),

    #[error("Invalid number '{0}' at {1}")]
    InvalidNumber(String, Location),
}
