use std::path::PathBuf;
use std::{fs, io::Write};

use clap::Parser;
use color_eyre::eyre::{Context, Result};

mod lexer;

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
