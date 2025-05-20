use std::fs;
use std::path::PathBuf;

use clap::Parser;
use color_eyre::eyre::{Context, Result};

mod lexer;

#[derive(Debug, Parser)]
struct Args {
    #[arg()]
    file: Option<PathBuf>,
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let args = Args::parse();

    if let Some(file) = &args.file {
        let file_content = fs::read_to_string(file)
            .wrap_err_with(|| format!("Failed to read file: {}", file.display()))?;
        todo!()
    } else {
        loop {
            let mut input = String::new();
            println!("> ");
            std::io::stdin()
                .read_line(&mut input)
                .wrap_err("Failed to read line")?;

            if input.trim().is_empty() {
                break;
            }

            let mut lexer = lexer::Lexer::new(&input);
            match lexer.next_token() {
                Ok(token) => println!("{:?}", token),
                Err(e) => eprintln!("Error: {}", e),
            }
        }
    }

    Ok(())
}
