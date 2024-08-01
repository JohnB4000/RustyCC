use std::{
    env::args,
    fs::File,
    io::{BufReader, Read},
    process::exit,
};

mod lexer;
#[allow(unused)]
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

    let ast = parser::parse(lexer_tokens);

    println!("{:?}", ast);
}
