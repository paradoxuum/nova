use std::io::Write;

use color_eyre::eyre::{eyre, Result};

use crate::{
    environment::{Environment, Value},
    lexer::Literal,
};

pub fn define_builtin_functions(env: &mut Environment) {
    env.define(
        "print",
        Value::BuiltInFunction("print".to_string(), vec!["value".to_string()], print_fn),
    );
    env.define(
        "input",
        Value::BuiltInFunction("input".to_string(), vec!["prompt".to_string()], input_fn),
    )
}

fn print_fn(arg_count: usize, args: &[Value]) -> Result<Value> {
    if arg_count < 1 {
        println!();
        return Ok(Literal::Nil.into());
    }

    for arg in args {
        print!("{}", arg);
        print!(" ");
    }
    println!();
    std::io::stdout()
        .flush()
        .map_err(|e| eyre!("Failed to flush stdout: {}", e))?;
    Ok(Literal::Nil.into())
}

fn input_fn(_: usize, args: &[Value]) -> Result<Value> {
    let prompt = match &args[0] {
        Value::Literal(Literal::String(s)) => Some(s),
        Value::Literal(Literal::Nil) => None,
        _ => {
            return Err(eyre!(
                "Expected a string or nil as the first argument, got: {}",
                args[0]
            ))
        }
    };

    if let Some(prompt) = prompt {
        print!("{}", prompt);
        std::io::stdout()
            .flush()
            .map_err(|e| eyre!("Failed to flush stdout: {}", e))?;
    }
    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .map_err(|e| eyre!("Failed to read line: {}", e))?;

    Ok(Literal::String(input.trim().to_string()).into())
}
