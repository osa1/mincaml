#![feature(or_patterns, box_patterns)]

mod cg_types;
mod closure_convert;
mod codegen;
mod common;
mod ctx;
mod interner;
mod knormal;
mod lexer;
mod liveness;
mod locals;
mod parser;
mod perf;
mod type_check;
mod utils;
mod var;

use closure_convert::closure_convert;
use codegen::codegen;
use knormal::knormal;
use lexer::{tokenize, Token};
use parser::parse;
use type_check::type_check_pgm;

use std::process::exit;
use std::time::Instant;

#[cfg(debug_assertions)]
#[global_allocator]
static A: perf::AllocCounter = perf::AllocCounter;

fn main() {
    // println!("size_of::<u32>() == {}", std::mem::size_of::<u32>());
    // println!("size_of::<Rc<str>>() == {}", std::mem::size_of::<std::rc::Rc<str>>());
    // println!("size_of::<Rc<String>>() == {}", std::mem::size_of::<std::rc::Rc<str>>());
    // println!("size_of::<Var>() == {}", std::mem::size_of::<var::Var>());

    let args: Vec<String> = std::env::args().collect();
    match args.as_slice() {
        [_, ref file] => {
            exit(do_file(file));
        }
        _ => {
            println!("What do you mean?");
            exit(1);
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

    // println!("Expr: {:#?}", expr);

    match type_check_pgm(&mut ctx, &mut expr) {
        Err(err) => {
            println!("Type error: {:#?}", err);
            return 1;
        }
        Ok(()) => {}
    };

    // println!("Type-checked expr: {:#?}", expr);

    let expr = knormal(&mut ctx, expr);

    // println!("K normalized:");
    // println!("{:?}", expr);

    let funs = closure_convert(&mut ctx, expr);

    println!("### Closure conversion:\n");

    let mut s = String::new();
    for fun in &funs {
        fun.pp(&ctx, &mut s).unwrap();
    }

    println!("{}", s);

    println!("### Code generation:\n");

    for fun in &funs {
        let _ = codegen(&mut ctx, fun);
    }

    0
}

fn do_file(file: &str) -> i32 {
    let start_time = Instant::now();
    perf::reset_allocated();

    let contents = std::fs::read_to_string(file).unwrap();
    let ret = do_expr(&contents);

    let allocated = perf::get_allocated();
    let elapsed = start_time.elapsed();

    println!(
        "File took {} ms to build.",
        utils::comma_sep(&format!("{}", elapsed.as_millis()))
    );
    if allocated != 0 {
        println!(
            "{} bytes allocated.",
            utils::comma_sep(&format!("{}", allocated))
        );
    }

    ret
}
