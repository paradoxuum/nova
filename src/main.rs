use std::io::Write;
use std::path::PathBuf;

use clap::Parser;
use color_eyre::eyre::{Context, Result};

mod lexer;
mod parser;

#[derive(Debug, Parser)]
struct Args {
    #[arg()]
    file: Option<PathBuf>,
}

fn repl() -> Result<()> {
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
            return Ok(());
        }

        let mut lexer = lexer::Lexer::new(&input);
        let tokens = lexer.scan()?;

        println!("Tokens: {:?}", tokens);

        let mut parser = parser::Parser::new(tokens.clone());
        let stmts = parser.parse().wrap_err("Failed to parse expression")?;
        println!("Output: {:?}", stmts);
    }
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let args = Args::parse();

    if args.file.is_some() {
        todo!()
    }

    repl()
}
