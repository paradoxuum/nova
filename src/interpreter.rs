use std::collections::HashMap;

use color_eyre::eyre::{eyre, Result};

use crate::{
    lexer::Literal,
    parser::{Expression, Visitor},
};

pub struct Environment {
    pub enclosing: Option<Box<Environment>>,
    variables: HashMap<String, Literal>,
}

pub struct Interpreter {
    pub environment: Environment,
}

impl Visitor<Result<Option<Literal>>> for Interpreter {
    fn visit_expression(&mut self, expr: &Expression) -> Result<Option<Literal>> {
        let output = match expr {
            Expression::Unary(op, right) => {
                let value = self.visit_expression(right)?;
                match value {
                    Some(Literal::Number(num)) => {
                        let result = match op.kind {
                            crate::lexer::TokenKind::Minus => -num,
                            crate::lexer::TokenKind::Plus => num,
                            _ => unreachable!(),
                        };
                        Some(Literal::Number(result))
                    }
                    None => None,
                    _ => {
                        return Err(eyre!(
                            "Unary operator {:?} not supported for type {:?}",
                            op.kind,
                            value
                        ))
                    }
                }
            }
            Expression::Logical(left, op, right) => {
                let left_value = self.visit_expression(left)?;
                let right_value = self.visit_expression(right)?;
                match (left_value.clone(), right_value) {
                    (Some(Literal::Number(left_num)), Some(Literal::Number(right_num))) => {
                        let result = match op.kind {
                            crate::lexer::TokenKind::And => left_num != 0.0 && right_num != 0.0,
                            crate::lexer::TokenKind::Or => left_num != 0.0 || right_num != 0.0,
                            _ => unreachable!(),
                        };
                        Some(Literal::Boolean(result))
                    }
                    _ => {
                        return Err(eyre!(
                            "Logical operator {:?} not supported for type {:?}",
                            op.kind,
                            left_value
                        ))
                    }
                }
            }
            Expression::Binary(left, op, right) => {
                let left_value = self.visit_expression(left)?;
                let right_value = self.visit_expression(right)?;

                // Ensure both sides are numbers
                if left_value.is_none() || right_value.is_none() {
                    return Err(eyre!(
                        "Binary operator {:?} not supported for type {:?}",
                        op.kind,
                        left_value
                    ));
                }

                match (left_value.clone(), right_value) {
                    (Some(Literal::Number(left_num)), Some(Literal::Number(right_num))) => {
                        let result = match op.kind {
                            crate::lexer::TokenKind::Plus => left_num + right_num,
                            crate::lexer::TokenKind::Minus => left_num - right_num,
                            crate::lexer::TokenKind::Multiply => left_num * right_num,
                            crate::lexer::TokenKind::Divide => left_num / right_num,
                            _ => unreachable!(),
                        };
                        Some(Literal::Number(result))
                    }
                    _ => return Err(eyre!("Operands must be numbers for operator {:?}", op.kind)),
                }
            }
            Expression::Grouping(expr) => self.visit_expression(expr)?,
            Expression::Literal(token) => match token.literal {
                Some(Literal::Number(num)) => Some(Literal::Number(num)),
                Some(Literal::String(ref s)) => Some(Literal::String(s.clone())),
                Some(Literal::Boolean(b)) => Some(Literal::Boolean(b)),
                Some(Literal::Nil) => Some(Literal::Nil),
                None => None,
            },
            _ => todo!(),
        };

        Ok(output)
    }

    fn visit_statement(&mut self, stmt: &crate::parser::Statement) -> Result<Option<Literal>> {
        todo!()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Environment {
                enclosing: None,
                variables: HashMap::new(),
            },
        }
    }

    pub fn interpret(&mut self, expr: &Expression) -> Result<Option<Literal>> {
        self.visit_expression(expr)
    }
}
