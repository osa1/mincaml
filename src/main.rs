#![feature(or_patterns)]

// mod anormal;
// mod closure_convert;
// mod instr_sel;
mod interner;
//mod knormal;
mod ctx;
mod lexer;
mod locals;
mod parser;
mod type_check;
mod var;

// use anormal::anormal;
// use closure_convert::closure_convert;
// use knormal::knormal;
use lexer::{tokenize, Token};
use parser::parse;
use type_check::type_check_pgm;

use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::process::exit;

fn main() {
    // println!("size_of::<u32>() == {}", std::mem::size_of::<u32>());
    // println!("size_of::<Rc<str>>() == {}", std::mem::size_of::<std::rc::Rc<str>>());
    // println!("size_of::<Rc<String>>() == {}", std::mem::size_of::<std::rc::Rc<str>>());
    // println!("size_of::<Var>() == {}", std::mem::size_of::<var::Var>());

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

    // println!("{:#?}", tokens);

    let mut ctx = Default::default();

    let mut expr = match parse(&mut ctx, &tokens) {
        Err(err) => {
            println!("Parser error: {:#?}", err);
            return 1;
        }
        Ok(expr) => expr,
    };

    println!("Expr: {:#?}", expr);

    let ty_env = match type_check_pgm(&mut ctx, &mut expr) {
        Err(err) => {
            println!("Type error: {:#?}", err);
            return 1;
        }
        Ok(ty_env) => ty_env,
    };

    println!("Type env: {:?}", ty_env);
    println!("Type-checked expr: {:#?}", expr);

    /*
    let mut expr = knormal(expr, &bndr_tys);

    // println!("K normalized:");
    // println!("{:?}", expr);

    anormal(&mut expr);

    // println!("A normalized:");
    // println!("{:#?}", expr);

    let (funs, expr) = closure_convert(expr);

    // println!("Functions:");
    // println!("{:#?}", funs);

    // println!("Expr:");

    for fun in funs {
        println!("{}", fun.pprint().pretty(80));
    }

    println!("{}", expr.pprint().pretty(80));
    */

    0
}

fn do_file(file: &str) -> i32 {
    let contents = std::fs::read_to_string(file).unwrap();
    do_expr(&contents)
}
