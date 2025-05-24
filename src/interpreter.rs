use std::{cell::RefCell, collections::HashMap, rc::Rc};

use color_eyre::eyre::{eyre, Result};

use crate::{
    builtin::define_builtin_functions,
    environment::{Environment, Value},
    lexer::{Literal, TokenKind},
    parser::{Expression, Statement, Visitor},
};

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    locals: HashMap<Box<Expression>, usize>,
}

#[derive(Debug, thiserror::Error)]
pub enum InterpreterError {
    #[error("Undefined variable '{0}'")]
    UndefinedVariable(String),

    #[error("Invalid assignment to '{0}'")]
    InvalidOperation(String),

    #[error("Invalid operands for operator '{0:?}'")]
    OperatorNotSupported(TokenKind),

    #[error("Return called outside of a function")]
    ReturnValue(Value),

    #[error("Invalid function call")]
    InvalidFunctionCall,

    #[error(transparent)]
    Report(#[from] color_eyre::Report),
}

pub trait LiteralOperation {
    fn add(&self, other: &Self) -> Result<Self>
    where
        Self: Sized;
    fn subtract(&self, other: &Self) -> Result<Self>
    where
        Self: Sized;
    fn multiply(&self, other: &Self) -> Result<Self>
    where
        Self: Sized;
    fn divide(&self, other: &Self) -> Result<Self>
    where
        Self: Sized;
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Literal::String(s1), Literal::String(s2)) => s1 == s2,
            (Literal::Number(n1), Literal::Number(n2)) => n1 == n2,
            (Literal::Boolean(b1), Literal::Boolean(b2)) => b1 == b2,
            (Literal::Nil, Literal::Nil) => true,
            _ => false,
        }
    }
}

impl PartialOrd for Literal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Literal::Number(n1), Literal::Number(n2)) => n1.partial_cmp(n2),
            _ => None,
        }
    }
}

impl LiteralOperation for Literal {
    fn add(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Literal::Number(a), Literal::Number(b)) => Ok(Literal::Number(a + b)),
            (Literal::String(a), Literal::String(b)) => Ok(Literal::String(format!("{}{}", a, b))),
            _ => Err(eyre!("Invalid operands for addition")),
        }
    }

    fn subtract(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Literal::Number(a), Literal::Number(b)) => Ok(Literal::Number(a - b)),
            _ => Err(eyre!("Invalid operands for subtraction")),
        }
    }

    fn multiply(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Literal::Number(a), Literal::Number(b)) => Ok(Literal::Number(a * b)),
            _ => Err(eyre!("Invalid operands for multiplication")),
        }
    }

    fn divide(&self, other: &Self) -> Result<Self> {
        match (self, other) {
            (Literal::Number(a), Literal::Number(b)) => Ok(Literal::Number(a / b)),
            _ => Err(eyre!("Invalid operands for division")),
        }
    }
}

impl Visitor<Result<Option<Value>, InterpreterError>> for Interpreter {
    fn visit_expression(&mut self, expr: &Expression) -> Result<Option<Value>, InterpreterError> {
        let output = match expr {
            Expression::Unary(op, right) => {
                let value = self.visit_expression(right)?;
                if let Some(value) = &value {
                    if op.kind == TokenKind::Not {
                        return Ok(Some(Literal::Boolean(!is_truthy(value)).into()));
                    }
                }

                match value {
                    Some(Value::Literal(Literal::Number(num))) => {
                        let result = match op.kind {
                            TokenKind::Minus => -num,
                            TokenKind::Plus => num,
                            _ => unreachable!(),
                        };
                        Some(Literal::Number(result).into())
                    }
                    None => None,
                    _ => return Err(InterpreterError::OperatorNotSupported(op.kind.clone())),
                }
            }
            Expression::Logical(left, op, right) => {
                let left_value = self.visit_expression(left)?;
                if left_value.is_none() {
                    return Ok(None);
                }

                match op.kind {
                    TokenKind::And => {
                        if !is_truthy(left_value.as_ref().unwrap()) {
                            return Ok(Some(Literal::Boolean(false).into()));
                        }
                    }
                    TokenKind::Or => {
                        if is_truthy(left_value.as_ref().unwrap()) {
                            return Ok(Some(Literal::Boolean(true).into()));
                        }
                    }
                    _ => unreachable!(),
                }

                // If we didn't short-circuit, evaluate the right side and return its value
                return self.visit_expression(right);
            }
            Expression::Binary(left, op, right) => {
                let left_value = self.visit_expression(left)?;
                let right_value = self.visit_expression(right)?;

                if left_value.is_none() || right_value.is_none() {
                    return Ok(None);
                }

                let left_value = match left_value {
                    Some(Value::Literal(literal)) => literal,
                    _ => {
                        return Err(InterpreterError::Report(eyre!(
                            "Left operand not a literal: {:?}",
                            left_value
                        )))
                    }
                };
                let right_value = match right_value {
                    Some(Value::Literal(literal)) => literal,
                    _ => {
                        return Err(InterpreterError::Report(eyre!(
                            "Right operand not a literal: {:?}",
                            right_value
                        )))
                    }
                };

                match op.kind {
                    TokenKind::Equal => {
                        return Ok(Some(Literal::Boolean(left_value.eq(&right_value)).into()))
                    }
                    TokenKind::NotEqual => {
                        return Ok(Some(Literal::Boolean(left_value.ne(&right_value)).into()))
                    }
                    TokenKind::Greater => {
                        return Ok(Some(Literal::Boolean(left_value.gt(&right_value)).into()))
                    }
                    TokenKind::GreaterEqual => {
                        return Ok(Some(Literal::Boolean(left_value.ge(&right_value)).into()))
                    }
                    TokenKind::Less => {
                        return Ok(Some(Literal::Boolean(left_value.lt(&right_value)).into()))
                    }
                    TokenKind::LessEqual => {
                        return Ok(Some(Literal::Boolean(left_value.le(&right_value)).into()))
                    }
                    TokenKind::Plus => left_value.add(&right_value).map(|val| Some(val.into()))?,
                    TokenKind::Minus => left_value
                        .subtract(&right_value)
                        .map(|val| Some(val.into()))?,
                    TokenKind::Multiply => left_value
                        .multiply(&right_value)
                        .map(|val| Some(val.into()))?,
                    TokenKind::Divide => left_value
                        .divide(&right_value)
                        .map(|val| Some(val.into()))?,
                    _ => unreachable!(),
                }
            }
            Expression::Grouping(expr) => self.visit_expression(expr)?,
            Expression::Literal(token) => {
                let literal = match token.literal {
                    Some(Literal::Number(num)) => Some(Literal::Number(num)),
                    Some(Literal::String(ref s)) => Some(Literal::String(s.clone())),
                    Some(Literal::Boolean(b)) => Some(Literal::Boolean(b)),
                    Some(Literal::Nil) => Some(Literal::Nil),
                    None => None,
                };
                literal.map(Value::Literal)
            }
            Expression::Variable(name) => {
                if let Some(value) = self.environment.borrow().get(&name.lexeme) {
                    Some(value.clone())
                } else {
                    return Err(InterpreterError::UndefinedVariable(name.lexeme.clone()));
                }
            }
            Expression::Assign(name, expr) => {
                let value = self
                    .visit_expression(expr)?
                    .unwrap_or(Value::Literal(Literal::Nil));
                let distance = self.locals.get(expr);
                if let Some(distance) = distance {
                    Environment::assign_at(
                        &self.environment,
                        *distance,
                        &name.lexeme,
                        value.clone(),
                    )?;
                } else {
                    self.environment
                        .borrow_mut()
                        .assign(&name.lexeme, value.clone())?;
                }
                Some(value)
            }
            Expression::Call(callee, args) => {
                let callee_value = self.visit_expression(callee)?;
                let mut arg_values = Vec::new();
                for arg in args {
                    if let Some(arg_value) = self.visit_expression(arg)? {
                        arg_values.push(arg_value);
                    } else {
                        return Err(InterpreterError::Report(eyre!("Argument is not a value")));
                    }
                }

                if let Some(Value::Function(_, params, body)) = callee_value {
                    // Replace missing args with Nil
                    let mut arg_values = arg_values;
                    while arg_values.len() < params.len() {
                        arg_values.push(Value::Literal(Literal::Nil));
                    }

                    let prev = self.environment.clone();
                    self.environment = Environment::with_parent(&self.environment);
                    for (param, arg) in params.iter().zip(arg_values) {
                        self.environment.borrow_mut().define(&param.lexeme, arg);
                    }
                    let result = self.visit_statement(&body);
                    self.environment = prev;
                    match result {
                        Err(InterpreterError::ReturnValue(value)) => Some(value),
                        Ok(_) => Some(Literal::Nil.into()),
                        Err(e) => return Err(e),
                    }
                } else if let Some(Value::BuiltInFunction(_, params, body)) = callee_value {
                    let arg_count = arg_values.len();

                    // Replace missing args with Nil
                    let mut arg_values = arg_values;
                    while arg_values.len() < params.len() {
                        arg_values.push(Value::Literal(Literal::Nil));
                    }

                    let result = body(arg_count, &arg_values);
                    match result {
                        Ok(value) => Some(value),
                        Err(e) => return Err(InterpreterError::Report(e)),
                    }
                } else {
                    return Err(InterpreterError::Report(eyre!("Callee is not a function")));
                }
            }
        };

        Ok(output)
    }

    fn visit_statement(&mut self, stmt: &Statement) -> Result<Option<Value>, InterpreterError> {
        match stmt {
            Statement::Expression(expr) => self.visit_expression(expr),
            Statement::Var(name, expr) => {
                if let Some(expr) = expr {
                    let value = self.visit_expression(expr)?;
                    if let Some(ref literal) = value {
                        self.environment
                            .borrow_mut()
                            .define(&name.lexeme, literal.clone());
                    }
                    Ok(value)
                } else {
                    self.environment
                        .borrow_mut()
                        .define(&name.lexeme, Value::Literal(Literal::Nil));
                    Ok(None)
                }
            }
            Statement::Block(stmts) => {
                let prev = self.environment.clone();
                self.environment = Environment::with_parent(&self.environment);
                for stmt in stmts {
                    let result = self.visit_statement(stmt);
                    if result.is_err() {
                        self.environment = prev;
                        return result;
                    }
                }
                self.environment = prev;
                Ok(None)
            }
            Statement::If(condition, then_branch, else_branch) => {
                let condition_value = self
                    .visit_expression(condition)?
                    .unwrap_or(Value::Literal(Literal::Nil));
                if is_truthy(&condition_value) {
                    self.visit_statement(then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.visit_statement(else_branch)?;
                }
                Ok(None)
            }
            Statement::While(condition, body) => {
                while is_truthy(
                    &self
                        .visit_expression(condition)?
                        .unwrap_or(Value::Literal(Literal::Nil)),
                ) {
                    self.visit_statement(body)?;
                }
                Ok(None)
            }
            Statement::Function(name, params, body) => {
                self.environment.borrow_mut().define(
                    &name.lexeme,
                    Value::Function(name.clone(), params.clone(), body.clone()),
                );
                Ok(None)
            }
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    let value = self.visit_expression(expr)?;
                    if let Some(value) = value {
                        return Err(InterpreterError::ReturnValue(value));
                    }
                }
                Err(InterpreterError::ReturnValue(Value::Literal(Literal::Nil)))
            }
        }
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Environment::new();
        define_builtin_functions(&mut env.borrow_mut());

        Interpreter {
            environment: env,
            locals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, statements: &[Statement]) -> Result<()> {
        for stmt in statements {
            self.visit_statement(stmt)?;
        }
        Ok(())
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Literal(literal) => match literal {
            Literal::Number(n) => *n != 0.0,
            Literal::String(s) => !s.is_empty(),
            Literal::Boolean(b) => *b,
            Literal::Nil => false,
        },
        _ => true,
    }
}
