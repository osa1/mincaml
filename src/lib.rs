mod anormal;
mod ast;
mod cg_types;
mod closure_convert;
mod codegen;
mod codegen_llvm;
mod common;
mod ctx;
mod interner;
mod lexer;
mod locals;
mod lower;
#[allow(unused_imports, unused_parens, clippy::all)]
mod parser_lalrpop;
mod perf;
mod type_check;
mod utils;
mod var;

use anormal::anormal;
use closure_convert::closure_convert;
use codegen::codegen;
use lexer::{Token, tokenize};
use lower::lower_fun;
use type_check::type_check_pgm;

use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::time::{Duration, Instant};

#[derive(Debug, Clone)]
pub struct CompileOptions {
    /// Compiled program path.
    pub path: String,

    /// Directory where the compilation outputs will be generated. Defaults to the working
    /// directory.
    pub out_dir: Option<String>,

    pub backend: Backend,

    /// Dump closure converted program.
    pub dump_cc: bool,

    /// Dump lowered program.
    pub dump_lower: bool,

    /// Dump code generated program.
    pub dump_cg: bool,

    /// Report runtime and allocations of passes.
    pub show_pass_stats: bool,
}

#[derive(Debug, Clone)]
pub enum Backend {
    Cranelift,
    LLVM,
}

pub fn compile_file(opts: CompileOptions) -> i32 {
    let contents = std::fs::read_to_string(&opts.path).unwrap();
    match compile_expr(
        &contents,
        opts.backend,
        opts.dump_cc,
        opts.dump_cg,
        opts.dump_lower,
        opts.show_pass_stats,
    ) {
        None => 1,
        Some(object_code) => link(
            &opts.path,
            opts.out_dir.as_ref().map(|s| s.as_str()),
            object_code,
        ),
    }
}

type ObjectCode = Vec<u8>;

fn compile_expr(
    expr_str: &str,
    backend: Backend,
    dump_cc: bool,
    dump_lower: bool,
    dump_cg: bool,
    show_pass_stats: bool,
) -> Option<ObjectCode> {
    let mut pass_stats: Vec<PassStats> = Vec::with_capacity(10);

    let mut ctx = Default::default();

    let (funs, main) = prepare_expr(expr_str, dump_cc, &mut ctx, &mut pass_stats)?;

    if dump_lower {
        println!("### Lowered:\n");
    }

    let funs: Vec<lower::Fun> = funs
        .into_iter()
        .map(|fun| {
            let fun = lower_fun(&mut ctx, fun);
            if dump_lower {
                let mut s = String::new();
                fun.pp(&ctx, &mut s).unwrap();
                println!("{}", s);
            }
            fun
        })
        .collect();

    if dump_cg {
        println!("### Code generation:\n");
    }

    let object_code = record_pass_stats(&mut pass_stats, "codegen", || match backend {
        Backend::Cranelift => codegen(&mut ctx, &funs, main, dump_cg),
        Backend::LLVM => codegen_llvm::codegen(&mut ctx, &funs, main, dump_cg),
    });

    if show_pass_stats {
        report_pass_stats(&pass_stats);
    }

    Some(object_code)
}

/// Prepare an expression for code generation: parse, type check, convert to a-normal form, do
/// closure conversion.
fn prepare_expr(
    expr_str: &str,
    dump_cc: bool,
    ctx: &mut ctx::Ctx,
    pass_stats: &mut Vec<PassStats>,
) -> Option<(Vec<closure_convert::Fun>, ctx::VarId)> {
    let tokens: Vec<Token> = match record_pass_stats(pass_stats, "tokenize", || tokenize(expr_str))
    {
        Err(err) => {
            println!("Lexer error: {:#?}", err);
            return None;
        }
        Ok(tokens) => tokens,
    };

    // println!("{:#?}", tokens);

    let expr = match record_pass_stats(pass_stats, "parse", || {
        let token_iter = tokens
            .into_iter()
            .enumerate()
            .map(|(i, tok)| Ok((i, tok, i + 1)));
        parser_lalrpop::ExprParser::new().parse(token_iter)
    }) {
        Err(err) => {
            println!("Parser error: {:#?}", err);
            return None;
        }
        Ok(expr) => expr,
    };

    // println!("Expr: {:#?}", expr);

    let mut expr = record_pass_stats(pass_stats, "intern", || expr.intern(ctx));

    if let Err(err) = record_pass_stats(pass_stats, "type check", || type_check_pgm(ctx, &mut expr))
    {
        println!("Type error: {:#?}", err);
        return None;
    }

    // println!("Type-checked expr: {:#?}", expr);

    let expr = record_pass_stats(pass_stats, "anormal", || anormal(ctx, expr));

    // println!("K normalized:");
    // println!("{:?}", expr);

    let (funs, main) =
        record_pass_stats(pass_stats, "closure convert", || closure_convert(ctx, expr));

    if dump_cc {
        println!("### Closure conversion:\n");

        for fun in &funs {
            println!("{}\n", fun.pp(ctx));
        }
    }

    Some((funs, main))
}

fn link(path: &str, out_dir: Option<&str>, object_code: ObjectCode) -> i32 {
    let out_dir = out_dir.unwrap_or(".");
    let path = Path::new(path);
    let file_stem = path.file_stem().unwrap().to_str().unwrap();
    let o_file_name = format!("{}.o", file_stem);

    File::create(format!("{}/{}", out_dir, o_file_name))
        .unwrap()
        .write_all(&object_code)
        .unwrap();

    // Build RTS
    let output = Command::new("gcc")
        .args(["rts.c", "-c", "-o", &format!("{}/rts.o", out_dir)])
        .spawn()
        .unwrap()
        .wait_with_output()
        .unwrap();

    assert!(output.status.success());

    // Link
    let output = Command::new("gcc")
        .args([
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
    stats: &mut Vec<PassStats>,
    pass_name: &'static str,
    pass: F,
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
