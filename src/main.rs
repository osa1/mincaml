#![feature(or_patterns)]

mod lexer;
mod parser;
mod type_check;

use lexer::{tokenize, Token};
use parser::parse;
use type_check::type_check;

use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.as_slice() {
        [_] => {
            repl();
        }
        [_, ref file] => {
            do_file(file);
        }
        _ => {
            println!("What do you mean?");
            ::std::process::exit(1);
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

fn do_expr(expr_str: &str) {
    let tokens: Vec<Token> = match tokenize(expr_str) {
        Err(err) => {
            println!("Lexer error: {:#?}", err);
            return;
        }
        Ok(tokens) => tokens,
    };

    println!("Tokens: {:?}", tokens);
    let expr = parse(&tokens);
    println!("Expr: {:#?}", expr);
    if let Ok(ref expr) = expr {
        println!("Type: {:?}", type_check(expr));
    }
}

fn do_file(file: &str) {
    let contents = std::fs::read_to_string(file).unwrap();
    do_expr(&contents)
}
