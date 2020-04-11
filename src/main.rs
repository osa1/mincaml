#![feature(or_patterns)]

mod lexer;
mod parser;
mod type_check;

use lexer::{LexErr, Lexer};
use parser::Parser;
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
    let mut lexer = Lexer::new(expr_str.as_bytes());
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
    let expr = parser.expr();
    println!("Expr: {:#?}", expr);
    if let Ok(ref expr) = expr {
        println!("Type: {:#?}", type_check(expr));
    }
}

fn do_file(file: &str) {
    let contents = std::fs::read_to_string(file).unwrap();
    do_expr(&contents)
}
