use std::process::exit;

use clap::{Arg, Command};

use libmc::{Backend, CompileOptions};

fn main() {
    let matches = Command::new("mc")
        .about("MinCaml compiler")
        .arg(
            Arg::new("file")
                .required(true)
                .help("Source file to compile"),
        )
        .arg(
            Arg::new("out-dir")
                .long("out-dir")
                .short('o')
                .value_name("DIR")
                .help("Output directory for compiled artifacts"),
        )
        .arg(
            Arg::new("dump-cc")
                .long("dump-cc")
                .action(clap::ArgAction::SetTrue)
                .help("Dump closure-converted program"),
        )
        .arg(
            Arg::new("dump-lower")
                .long("dump-lower")
                .action(clap::ArgAction::SetTrue)
                .help("Dump lowered program"),
        )
        .arg(
            Arg::new("dump-cg")
                .long("dump-cg")
                .action(clap::ArgAction::SetTrue)
                .help("Dump code-generated program"),
        )
        .arg(
            Arg::new("pass-stats")
                .long("pass-stats")
                .action(clap::ArgAction::SetTrue)
                .help("Report runtime and allocations of passes"),
        )
        .arg(
            Arg::new("backend")
                .long("backend")
                .value_name("BACKEND")
                .default_value("cranelift")
                .value_parser(["cranelift", "llvm"])
                .help("Code generation backend"),
        )
        .get_matches();

    let opts = CompileOptions {
        path: matches.get_one::<String>("file").unwrap().clone(),
        out_dir: matches.get_one::<String>("out-dir").cloned(),
        backend: match matches.get_one::<String>("backend").unwrap().as_str() {
            "llvm" => Backend::LLVM,
            _ => Backend::Cranelift,
        },
        dump_cc: matches.get_flag("dump-cc"),
        dump_lower: matches.get_flag("dump-lower"),
        dump_cg: matches.get_flag("dump-cg"),
        show_pass_stats: matches.get_flag("pass-stats"),
    };

    exit(libmc::compile_file(opts));
}
