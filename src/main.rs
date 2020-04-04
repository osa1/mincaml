mod lexer;
mod parser;

use lexer::{LexErr, Lexer};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let contents = std::fs::read_to_string(&args[1]).unwrap();

    let mut lexer = Lexer::new(contents.as_bytes());
    loop {
        match lexer.next() {
            Err(LexErr::EndOfInput) => {
                break;
            }
            Err(err) => {
                println!("{:#?}", err);
                std::process::exit(1);
            }
            Ok(token) => {
                println!("{:?}", token);
            }
        }
    }
}
