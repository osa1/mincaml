#![feature(or_patterns)]

mod lexer;
mod parser;

use lexer::{LexErr, Lexer};
use parser::Parser;

use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.as_slice() {
        [_] => {
            repl();
        }
        [_, file] => {
            let contents = std::fs::read_to_string(file).unwrap();
            let mut lexer = Lexer::new(contents.as_bytes());
            let mut tokens = vec![];
            loop {
                match lexer.next() {
                    Err(LexErr::EndOfInput) => {
                        break;
                    }
                    Err(err) => {
                        println!("Lexer error: {:#?}", err);
                        break;
                    }
                    Ok(tok) => {
                        tokens.push(tok);
                    }
                }
            }
            println!("Tokens: {:?}", tokens);
            let mut parser = Parser::new(&tokens);
            println!("{:#?}", parser.expr());
        }
        _ => {}
    }

    // let contents = std::fs::read_to_string(&args[1]).unwrap();

    // let mut lexer = Lexer::new(contents.as_bytes());
    // loop {
    //     match lexer.next() {
    //         Err(LexErr::EndOfInput) => {
    //             break;
    //         }
    //         Err(err) => {
    //             println!("{:#?}", err);
    //             std::process::exit(1);
    //         }
    //         Ok(token) => {
    //             println!("{:?}", token);
    //         }
    //     }
    // }
}

fn repl() {
    let mut rl = Editor::<()>::new();

    loop {
        match rl.readline(">>> ") {
            Ok(line) => {
                let mut lexer = Lexer::new(line.as_bytes());
                let mut tokens = vec![];
                loop {
                    match lexer.next() {
                        Err(LexErr::EndOfInput) => {
                            break;
                        }
                        Err(err) => {
                            println!("Lexer error: {:#?}", err);
                            break;
                        }
                        Ok(tok) => {
                            tokens.push(tok);
                        }
                    }
                }
                println!("Tokens: {:?}", tokens);
                let mut parser = Parser::new(&tokens);
                println!("{:#?}", parser.expr());

                // Add it to the history after using to avoid cloning
                rl.history_mut().add(line);
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => {
                break;
            }
            err
            @ Err(ReadlineError::Io(_) | ReadlineError::Utf8Error | ReadlineError::Errno(_)) => {
                println!("Error while reading line: {:?}", err);
                println!("Aborting.");
                break;
            }
        }
    }
}
