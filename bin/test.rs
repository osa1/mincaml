use std::ffi::OsStr;
use std::fs;
use std::io::Write;
use std::path::Path;
use std::process::{Command, ExitStatus, Output, Stdio, exit};

use clap::Arg;

use libmc::Backend;

fn run_ocaml(file_path: &str) -> String {
    let ret: Output = Command::new("ocaml")
        .arg(file_path)
        .stderr(Stdio::null())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap_or_else(|err| {
            eprintln!("Unable to run 'ocaml' executable. Make sure 'ocaml' is in PATH.");
            eprintln!("Error: {:?}", err);
            exit(1)
        })
        .wait_with_output()
        .unwrap();

    String::from_utf8(ret.stdout).unwrap()
}

enum McError {
    CompileError,
    RunError {
        exit_code: ExitStatus,
        stderr: String,
        stdout: String,
    },
}

fn run_mc(file_path_str: &str, backend: Backend) -> Result<String, McError> {
    let file_path = Path::new(file_path_str);
    let file_stem = file_path.file_stem().unwrap();
    let file_stem_str = file_stem.to_str().unwrap();

    let opts = libmc::CompileOptions {
        path: file_path_str.to_string(),
        out_dir: Some("_test".to_string()),
        backend,
        dump_cc: false,
        dump_lower: false,
        dump_cg: false,
        show_pass_stats: false,
    };
    let ret = libmc::compile_file(opts);

    if ret != 0 {
        return Err(McError::CompileError);
    }

    let Output {
        status,
        stdout,
        stderr,
    } = Command::new(format!("_test/{}", file_stem_str))
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap()
        .wait_with_output()
        .unwrap();

    if !status.success() || !stderr.is_empty() {
        let stdout = String::from_utf8(stdout).unwrap();
        let stderr = String::from_utf8(stderr).unwrap();
        return Err(McError::RunError {
            exit_code: status,
            stdout,
            stderr,
        });
    }

    Ok(String::from_utf8(stdout).unwrap())
}

enum TestResult {
    Pass,
    Fail(String),
}

fn run_test(path: &Path, backend: Backend) -> TestResult {
    let path_str = path.to_str().unwrap();
    let ocaml_out = run_ocaml(path_str);
    match run_mc(path_str, backend) {
        Ok(mc_out) => {
            if mc_out == ocaml_out {
                TestResult::Pass
            } else {
                use std::fmt::Write;
                let mut s = String::new();
                writeln!(&mut s, "Expected: {:?}", ocaml_out).unwrap();
                writeln!(&mut s, "Found:    {:?}", mc_out).unwrap();
                TestResult::Fail(s)
            }
        }
        Err(McError::CompileError) => TestResult::Fail("Compile error".to_string()),
        Err(McError::RunError {
            exit_code,
            stderr,
            stdout,
        }) => TestResult::Fail(format!(
            "Generated program returned {}\nstderr: {:?}\nstdout: {:?}",
            exit_code, stderr, stdout
        )),
    }
}

// Returns whether the test failed
fn report(result: TestResult) -> bool {
    match result {
        TestResult::Pass => {
            println!("OK");
            false
        }
        TestResult::Fail(reason) => {
            println!("FAIL");
            println!("{}", reason);
            true
        }
    }
}

// Run all .ml files in a directory as tests
fn run_dir(dir: &Path, backend: Backend) -> bool {
    let mut any_failed = false;
    for entry in fs::read_dir(dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension() == Some(OsStr::new("ml")) {
            // println!("{:?}", path);
            print!("{} ... ", path.to_str().unwrap());
            let _ = ::std::io::stdout().lock().flush();
            any_failed |= report(run_test(&path, backend));
        } else if entry.file_type().unwrap().is_dir() {
            any_failed |= run_dir(&path, backend);
        }
    }
    any_failed
}

fn create_dir(path: &str) {
    match fs::create_dir(path) {
        Ok(()) => (),
        Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => (),
        Err(err) => panic!("Unable to create dir {:?}: {:?}", path, err),
    }
}

fn main() {
    let matches = clap::Command::new("test")
        .about("MinCaml test runner")
        .arg(
            Arg::new("target")
                .help("File or directory to test")
                .default_value("programs"),
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

    let backend = match matches.get_one::<String>("backend").unwrap().as_str() {
        "llvm" => Backend::LLVM,
        _ => Backend::Cranelift,
    };

    create_dir("_test");

    let target = matches.get_one::<String>("target").unwrap();
    let target_path = Path::new(target);

    let fail = if target_path.is_file() {
        report(run_test(target_path, backend))
    } else {
        run_dir(target_path, backend)
    };

    if fail {
        exit(1);
    }
}
