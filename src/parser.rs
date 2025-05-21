use color_eyre::eyre::{eyre, Context, Result};

use crate::lexer::{Token, TokenKind};

#[derive(Debug, Clone)]
pub enum Expression {
    Assign(Token, Box<Expression>),
    Binary(Box<Expression>, Token, Box<Expression>),
    Logical(Box<Expression>, Token, Box<Expression>),
    Unary(Token, Box<Expression>),
    Literal(Token),
    Grouping(Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Print(Expression),
    Var(Token, Option<Expression>),
    Block(Vec<Statement>),
    Function(Token, Vec<Token>, Box<Statement>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
}

pub trait Visitor<T> {
    fn visit_expression(&mut self, expr: &Expression) -> T;
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Statement> {
        if self.match_token(TokenKind::Let) {
            return self.var_declaration();
        } else if self.match_token(TokenKind::Function) {
            return self.function_declaration();
        } else if self.match_token(TokenKind::If) {
            return self.if_statement();
        } else if self.match_token(TokenKind::For) {
            return self.for_statement();
        } else if self.match_token(TokenKind::While) {
            return self.while_statement();
        } else if self.match_token(TokenKind::Return) {
            return self.return_statement();
        } else if self.match_token(TokenKind::Print) {
            return self.print_statement();
        }

        self.expression_statement()
    }

    fn statement(&mut self) -> Result<Statement> {
        if self.match_token(TokenKind::If) {
            return self.if_statement();
        } else if self.match_token(TokenKind::LBrace) {
            return self.block_statement();
        }

        self.expression_statement()
    }

    fn var_declaration(&mut self) -> Result<Statement> {
        let name = self
            .consume(TokenKind::Identifier)
            .wrap_err("Expected variable name.")?
            .clone();

        let initializer = if self.match_token(TokenKind::Assign) {
            Some(self.expression()?)
        } else {
            None
        };

        if !self.match_token(TokenKind::Semicolon) {
            return Err(eyre!("Expected ';'"));
        }

        Ok(Statement::Var(name, initializer))
    }

    fn function_declaration(&mut self) -> Result<Statement> {
        let name = self
            .consume(TokenKind::Identifier)
            .wrap_err("Expected function name.")?
            .clone();

        self.consume(TokenKind::LParen)
            .wrap_err("Expected '(' after function name.")?;

        let mut params = Vec::new();
        while !self.check(TokenKind::RParen) {
            if params.len() >= 255 {
                return Err(eyre!("Cannot have more than 255 parameters"));
            }

            let param = self.consume(TokenKind::Identifier)?.clone();
            params.push(param);
            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.consume(TokenKind::RParen)
            .wrap_err("Expected ')' after parameters.")?;

        self.consume(TokenKind::LBrace)
            .wrap_err("Expected '{' before function body.")?;

        let body = self.block_statement()?;
        Ok(Statement::Function(name, params, Box::new(body)))
    }

    fn if_statement(&mut self) -> Result<Statement> {
        self.consume(TokenKind::LParen)
            .wrap_err("Expected '(' after 'if'.")?;

        let condition = self
            .expression()
            .wrap_err("Expected condition after 'if'.")?;

        self.consume(TokenKind::RParen)
            .wrap_err("Expected ')' after condition.")?;

        let then_branch = self.statement()?;
        let else_branch = if self.match_token(TokenKind::Else) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        Ok(Statement::If(condition, Box::new(then_branch), else_branch))
    }

    fn for_statement(&mut self) -> Result<Statement> {
        self.consume(TokenKind::LParen)
            .wrap_err("Expected '(' after 'for'.")?;

        let initializer = if self.match_token(TokenKind::Semicolon) {
            None
        } else if self.match_token(TokenKind::Let) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if !self.check(TokenKind::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenKind::Semicolon)
            .wrap_err("Expected ';' after loop condition.")?;

        let increment = if !self.check(TokenKind::RParen) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenKind::RParen)
            .wrap_err("Expected ')' after for clauses.")?;

        let body = self.statement()?;

        Ok(Statement::While(
            condition.unwrap_or(Expression::Literal(self.peek().clone())),
            Box::new(body),
        ))
    }

    fn while_statement(&mut self) -> Result<Statement> {
        self.consume(TokenKind::LParen)
            .wrap_err("Expected '(' after 'while'.")?;

        let condition = self
            .expression()
            .wrap_err("Expected condition after 'while'.")?;

        self.consume(TokenKind::RParen)
            .wrap_err("Expected ')' after condition.")?;

        let body = self.statement()?;
        Ok(Statement::While(condition, Box::new(body)))
    }

    fn return_statement(&mut self) -> Result<Statement> {
        let value = if !self.check(TokenKind::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenKind::Semicolon)
            .wrap_err("Expected ';' after return value.")?;

        Ok(Statement::Print(
            value.unwrap_or(Expression::Literal(self.peek().clone())),
        ))
    }

    fn print_statement(&mut self) -> Result<Statement> {
        let value = self.expression()?;
        self.consume(TokenKind::Semicolon)
            .wrap_err("Expected ';' after print statement.")?;
        Ok(Statement::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Statement> {
        let expr = self.expression()?;
        if self.match_token(TokenKind::Semicolon) {
            return Ok(Statement::Print(expr));
        }
        Err(eyre!("Expected ';'"))
    }

    fn block_statement(&mut self) -> Result<Statement> {
        let mut statements = Vec::new();

        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TokenKind::RBrace)
            .wrap_err("Expected '}' after block.")?;

        Ok(Statement::Block(statements))
    }

    fn expression(&mut self) -> Result<Expression> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression> {
        let expr = self.or()?;

        if self.match_token(TokenKind::Assign) {
            let value = self.assignment()?;
            if let Expression::Literal(token) = expr {
                return Ok(Expression::Assign(token, Box::new(value)));
            }
            return Err(eyre!("Invalid assignment target"));
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expression> {
        let mut expr = self.and()?;

        while self.match_token(TokenKind::Or) {
            let operator = self.previous().clone();
            let right = self.and()?;
            expr = Expression::Logical(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expression> {
        let mut expr = self.comparison()?;

        while self.match_token(TokenKind::And) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expression::Logical(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression> {
        let mut expr = self.binary()?;

        while self.match_token(TokenKind::Less)
            || self.match_token(TokenKind::Greater)
            || self.match_token(TokenKind::LessEqual)
            || self.match_token(TokenKind::GreaterEqual)
        {
            let operator = self.previous().clone();
            let right = self.binary()?;
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn binary(&mut self) -> Result<Expression> {
        let mut expr = self.unary()?;

        while self.match_token(TokenKind::Plus) || self.match_token(TokenKind::Minus) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression> {
        if self.match_token(TokenKind::Minus) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expression::Unary(operator, Box::new(right)));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expression> {
        let expr = match self.peek().kind {
            TokenKind::Number
            | TokenKind::String
            | TokenKind::Identifier
            | TokenKind::True
            | TokenKind::False
            | TokenKind::Nil => {
                let token = self.advance().clone();
                Expression::Literal(token)
            }
            TokenKind::LParen => {
                self.advance();
                let expr = self.expression()?;
                if !self.match_token(TokenKind::RParen) {
                    return Err(eyre!("Expected ')'"));
                }
                Expression::Grouping(Box::new(expr))
            }
            _ => return Err(eyre!("Expected expression")),
        };

        Ok(expr)
    }

    fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&self, kind: TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().kind == kind
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn consume(&mut self, kind: TokenKind) -> Result<&Token> {
        if self.check(kind.clone()) {
            return Ok(self.advance());
        }
        Err(eyre!("Expected token: {:?}", kind))
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}
