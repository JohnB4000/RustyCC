use std::{
    env::args,
    fs::File,
    io::{BufReader, Read},
    process::exit,
};

#[allow(unused)]
mod code_generator;
//mod interpreter;
mod lexer;
mod parser;

fn main() {
    let arguements: Vec<String> = args().collect();
    let filename = match arguements.get(1) {
        Some(filename) => filename,
        None => {
            println!("Arguement not provided: filename.");
            exit(1)
        }
    };

    let file_handler = match File::open(filename) {
        Ok(file_handle) => file_handle,
        Err(error) => {
            println!("Failed to open file {}, with error {:?}", filename, error);
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

    // match interpreter::interpret(ast) {
    //     Ok(exit_code) => exit(exit_code),
    //     Err(error) => {
    //         println!("{:?}", error);
    //         exit(1);
    //     }
    // }

    match code_generator::generate_code(ast) {
        Ok(()) => (),
        Err(error) => {
            println!("{:?}", error);
            exit(1);
        }
    }
}
