use std::{
    fs::File,
    io::{BufReader, Read},
    process::exit,
};

use clap::{ArgGroup, Parser};

mod code_generator;
mod interpreter;
mod lexer;
mod parser;

#[derive(Parser, Debug)]
#[command(
    name = "RustyCC",
    version = "1.0",
    author = "John Band",
    about = "A Rust C Compiler"
)]
#[command(group(ArgGroup::new("mode").required(true).args(&["compiler_selected", "interpreter_selected"])))]
struct Cli {
    /// Use the RustyCC Compiler
    #[arg(short = 'c', long = "compiler")]
    compiler_selected: bool,

    /// Use the RustyCC Interpreter
    #[arg(short = 'i', long = "interpreter")]
    interpreter_selected: bool,

    /// File to be used
    filename: String,
}

fn main() {
    let cli = Cli::parse();

    let file_handler = match File::open(&cli.filename) {
        Ok(file_handle) => file_handle,
        Err(error) => {
            println!(
                "Failed to open file {}, with error {:?}",
                cli.filename, error
            );
            exit(1)
        }
    };

    let file_iterator = BufReader::new(file_handler).bytes();

    let lexer_tokens = match lexer::lex(file_iterator) {
        Ok(lexer_tokens) => lexer_tokens,
        Err(error) => {
            println!("{:?}", error);
            exit(1)
        }
    };

    let ast = match parser::parse(lexer_tokens) {
        Ok(lexer_tokens) => lexer_tokens,
        Err(error) => {
            println!("{:?}", error);
            exit(1)
        }
    };

    if cli.compiler_selected {
        match code_generator::generate_code(ast) {
            Ok(()) => (),
            Err(error) => {
                println!("{:?}", error);
                exit(1);
            }
        }
    } else if cli.interpreter_selected {
        match interpreter::interpret(ast) {
            Ok(exit_code) => exit(exit_code),
            Err(error) => {
                println!("{:?}", error);
                exit(1);
            }
        }
    }
}
