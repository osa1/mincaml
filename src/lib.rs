#![feature(or_patterns, box_patterns)]

mod anormal;
mod cg_types;
// mod codegen;
mod common;
mod ctx;
mod interner;
mod lexer;
mod locals;
mod lower;
mod parser;
mod perf;
mod type_check;
mod utils;
mod var;

use anormal::anormal;
// use codegen::codegen;
use lexer::{tokenize, Token};
use lower::lower_pgm;
use parser::parse;
use type_check::type_check_pgm;

use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::time::{Duration, Instant};

#[cfg(debug_assertions)]
#[global_allocator]
static A: perf::AllocCounter = perf::AllocCounter;

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

type ObjectCode = Vec<u8>;

fn compile_expr(
    expr_str: &str, dump_cc: bool, dump_cg: bool, show_pass_stats: bool,
) -> Option<ObjectCode> {
    let mut pass_stats: Vec<PassStats> = Vec::with_capacity(10);

    let tokens: Vec<Token> =
        match record_pass_stats(&mut pass_stats, "tokenize", || tokenize(expr_str)) {
            Err(err) => {
                println!("Lexer error: {:#?}", err);
                return None;
            }
            Ok(tokens) => tokens,
        };

    // println!("{:#?}", tokens);

    let mut ctx = Default::default();

    let mut expr = match record_pass_stats(&mut pass_stats, "parse", || parse(&mut ctx, &tokens)) {
        Err(err) => {
            println!("Parser error: {:#?}", err);
            return None;
        }
        Ok(expr) => expr,
    };

    // println!("Expr: {:#?}", expr);

    match record_pass_stats(&mut pass_stats, "type check", || {
        type_check_pgm(&mut ctx, &mut expr)
    }) {
        Err(err) => {
            println!("Type error: {:#?}", err);
            return None;
        }
        Ok(()) => {}
    };

    // println!("Type-checked expr: {:#?}", expr);

    let expr = record_pass_stats(&mut pass_stats, "anormal", || anormal(&mut ctx, expr));

    // println!("K normalized:");
    // println!("{:?}", expr);

    let (funs, main) = record_pass_stats(&mut pass_stats, "closure convert", || {
        lower_pgm(&mut ctx, expr)
    });

    if dump_cc {
        println!("### Closure conversion:\n");

        let mut s = String::new();
        /*
        for fun in &funs {
            fun.pp(&ctx, &mut s).unwrap();
        }
        */

        println!("{}", s);
    }

    if dump_cg {
        println!("### Code generation:\n");
    }


    /*
    let object_code = record_pass_stats(&mut pass_stats, "codegen", || {
        codegen(&mut ctx, &funs, main, dump_cg)
    });
    */

    if show_pass_stats {
        report_pass_stats(&pass_stats);
    }

    // Some(object_code)
    None
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

pub fn compile_file(
    path: &str, out_dir: Option<&str>, dump_cc: bool, dump_cg: bool, show_pass_stats: bool,
) -> i32 {
    let contents = std::fs::read_to_string(path).unwrap();
    match compile_expr(&contents, dump_cc, dump_cg, show_pass_stats) {
        None => 1,
        Some(object_code) => link(path, out_dir, object_code),
    }
}

fn link(path: &str, out_dir: Option<&str>, object_code: ObjectCode) -> i32 {
    let out_dir = out_dir.unwrap_or(".");
    let path = Path::new(path);
    let file_stem = path.file_stem().unwrap().to_str().unwrap();
    let o_file_name = format!("{}.o", file_stem);

    File::create(&format!("{}/{}", out_dir, o_file_name))
        .unwrap()
        .write_all(&object_code)
        .unwrap();

    // Build RTS
    let output = Command::new("gcc")
        .args(&["rts.c", "-c", "-o", &format!("{}/rts.o", out_dir)])
        .spawn()
        .unwrap()
        .wait_with_output()
        .unwrap();

    assert!(output.status.success());

    // Link
    let output = Command::new("gcc")
        .args(&[
            &o_file_name,
            "rts.o",
            "-o",
            file_stem,
            "-lm", // link math library
        ])
        .current_dir(out_dir)
        .spawn()
        .unwrap()
        .wait_with_output()
        .unwrap();

    assert!(output.status.success());

    0
}
