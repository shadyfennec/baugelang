use clap::Parser;

use std::path::PathBuf;

use bauge::frontend::lexer::CharacterMap;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Enables the output of log files in the ./logs directory.
    #[arg(long)]
    logging: bool,

    /// The Bauge source file to compile.
    file: PathBuf,
}

fn main() {
    let args = Args::parse();

    if args.logging {
        bauge::logging::enable();
    }

    let map: CharacterMap = std::fs::read_to_string(args.file)
        .unwrap()
        .lines()
        .collect();

    let tokens = match map.tokenize() {
        Ok(t) => t,
        Err(e) => {
            eprintln!("{e}");
            return;
        }
    };

    println!("Tokenized to {} tokens.", tokens.len());
}
