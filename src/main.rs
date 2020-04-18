#![feature(or_patterns)]

mod lexer;
mod parser;
mod type_check;
mod type_check_env;

use lexer::{tokenize, Token};
use parser::parse;
use type_check::type_check_pgm;

use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::process::exit;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.as_slice() {
        [_] => {
            repl();
        }
        [_, ref file] => {
            exit(do_file(file));
        }
        _ => {
            println!("What do you mean?");
            exit(1);
        }
    }
}

fn repl() {
    let mut rl = Editor::<()>::new();

    loop {
        match rl.readline(">>> ") {
            Ok(line) => {
                do_expr(&line);

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

fn do_expr(expr_str: &str) -> i32 {
    let tokens: Vec<Token> = match tokenize(expr_str) {
        Err(err) => {
            println!("Lexer error: {:#?}", err);
            return 1;
        }
        Ok(tokens) => tokens,
    };

    println!("{:#?}", tokens);

    let expr = match parse(&tokens) {
        Err(err) => {
            println!("Parser error: {:#?}", err);
            return 1;
        }
        Ok(expr) => expr,
    };

    println!("{:#?}", expr);

    match type_check_pgm(&expr) {
        Err(err) => {
            println!("Type error: {:#?}", err);
            1
        }
        Ok(()) => 0,
    }
}

fn do_file(file: &str) -> i32 {
    let contents = std::fs::read_to_string(file).unwrap();
    do_expr(&contents)
}
