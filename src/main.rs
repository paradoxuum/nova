use std::io::Write;
use std::path::PathBuf;

use clap::Parser;
use color_eyre::eyre::{Context, Result};

mod builtin;
mod environment;
mod interpreter;
mod lexer;
mod parser;

#[derive(Debug, Parser)]
struct Args {
    #[arg()]
    file: Option<PathBuf>,
}

fn repl() -> Result<()> {
    let mut interpreter = interpreter::Interpreter::new();

    loop {
        let mut input = String::new();
        print!("> ");
        std::io::stdout()
            .flush()
            .wrap_err("Failed to flush stdout")?;

        std::io::stdin()
            .read_line(&mut input)
            .wrap_err("Failed to read line")?;

        if input.trim().is_empty() {
            continue;
        }

        let mut lexer = lexer::Lexer::new(&input);
        let tokens = lexer.scan()?;

        let mut parser = parser::Parser::new(tokens.clone());
        let stmts = parser.parse().wrap_err("Failed to parse expression")?;
        interpreter.interpret(&stmts)?;
    }
}

fn run_file(file: PathBuf) -> Result<()> {
    let mut interpreter = interpreter::Interpreter::new();

    let input = std::fs::read_to_string(&file)
        .wrap_err_with(|| format!("Failed to read file: {}", file.display()))?;

    let mut lexer = lexer::Lexer::new(&input);
    let tokens = lexer.scan()?;

    let mut parser = parser::Parser::new(tokens.clone());
    let stmts = parser.parse().wrap_err("Failed to parse expression")?;
    interpreter.interpret(&stmts)?;

    Ok(())
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let args = Args::parse();

    if let Some(file) = args.file {
        return run_file(file);
    }

    repl()
}
