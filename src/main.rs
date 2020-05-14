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
use std::time::{Duration, Instant};

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

#[derive(Debug)]
struct PassStats {
    name: &'static str,
    time: Duration,
    allocs: usize,
}

fn record_pass_stats<A, F: FnOnce() -> A>(
    stats: &mut Vec<PassStats>, pass_name: &'static str, pass: F,
) -> A {
    let allocs_before = perf::get_allocated();
    let start_time = Instant::now();
    let ret = pass();
    let elapsed = start_time.elapsed();
    let allocs_after = perf::get_allocated();
    stats.push(PassStats {
        name: pass_name,
        time: elapsed,
        allocs: allocs_after - allocs_before,
    });
    ret
}

fn do_expr(expr_str: &str) -> i32 {
    let mut pass_stats: Vec<PassStats> = Vec::with_capacity(10);

    let tokens: Vec<Token> =
        match record_pass_stats(&mut pass_stats, "tokenize", || tokenize(expr_str)) {
            Err(err) => {
                println!("Lexer error: {:#?}", err);
                return 1;
            }
            Ok(tokens) => tokens,
        };

    // println!("{:#?}", tokens);

    let mut ctx = Default::default();

    let mut expr = match record_pass_stats(&mut pass_stats, "parse", || parse(&mut ctx, &tokens)) {
        Err(err) => {
            println!("Parser error: {:#?}", err);
            return 1;
        }
        Ok(expr) => expr,
    };

    // println!("Expr: {:#?}", expr);

    match record_pass_stats(&mut pass_stats, "type check", || {
        type_check_pgm(&mut ctx, &mut expr)
    }) {
        Err(err) => {
            println!("Type error: {:#?}", err);
            return 1;
        }
        Ok(()) => {}
    };

    // println!("Type-checked expr: {:#?}", expr);

    let expr = record_pass_stats(&mut pass_stats, "knormal", || knormal(&mut ctx, expr));

    // println!("K normalized:");
    // println!("{:?}", expr);

    let (funs, main) = record_pass_stats(&mut pass_stats, "closure convert", || {
        closure_convert(&mut ctx, expr)
    });

    println!("### Closure conversion:\n");

    let mut s = String::new();
    for fun in &funs {
        fun.pp(&ctx, &mut s).unwrap();
    }

    println!("{}", s);

    println!("### Code generation:\n");

    record_pass_stats(&mut pass_stats, "codegen", || codegen(&mut ctx, &funs, main));

    report_pass_stats(&pass_stats);

    0
}

fn report_pass_stats(pass_stats: &[PassStats]) {
    // TODO: align columns
    // TODO: show percentage of allocs and times of each pass
    // TODO: maintain a counter for max res?
    println!("--------------------------------------------------------");
    let mut total_elapsed: Duration = Duration::from_micros(0);
    let mut total_allocated: usize = 0;
    for PassStats { name, time, allocs } in pass_stats {
        println!(
            "{}: {} ms, {} bytes",
            name,
            time.as_millis(),
            utils::comma_sep(&format!("{}", allocs))
        );
        total_elapsed += *time;
        total_allocated += allocs;
    }

    println!(
        "TOTAL: {} ms, {} bytes",
        total_elapsed.as_millis(),
        utils::comma_sep(&format!("{}", total_allocated))
    );
    println!("--------------------------------------------------------");
}

fn do_file(file: &str) -> i32 {
    let contents = std::fs::read_to_string(file).unwrap();
    do_expr(&contents)
}
