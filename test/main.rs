use std::ffi::OsStr;
use std::fs;
use std::path::Path;
use std::process::{Command, ExitStatus, Output, Stdio};

fn run_ocaml(file_path: &str) -> String {
    let ret: Output = Command::new("ocaml")
        .arg(file_path)
        .stderr(Stdio::null())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap()
        .wait_with_output()
        .unwrap();

    String::from_utf8(ret.stdout).unwrap()
}

enum McError {
    CompileError {
        exit_code: ExitStatus,
        stderr: String,
    },
    RunError {
        exit_code: ExitStatus,
        stderr: String,
        stdout: String,
    },
}

fn run_mc(file_path_str: &str) -> Result<String, McError> {
    let file_path = Path::new(file_path_str);
    let file_stem = file_path.file_stem().unwrap();
    let file_stem_str = file_stem.to_str().unwrap();

    {
        let Output { status, stderr, .. } = Command::new("target/debug/mc")
            .arg(file_path_str)
            .stderr(Stdio::piped())
            .stdout(Stdio::null())
            .spawn()
            .unwrap()
            .wait_with_output()
            .unwrap();

        if !status.success() || !stderr.is_empty() {
            let stderr = String::from_utf8(stderr).unwrap();
            return Err(McError::CompileError {
                exit_code: status,
                stderr,
            });
        }
    }

    let Output {
        status,
        stdout,
        stderr,
    } = Command::new(&format!("./{}", file_stem_str))
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

fn run_test(path: &Path) -> TestResult {
    let path_str = path.to_str().unwrap();
    let ocaml_out = run_ocaml(path_str);
    match run_mc(path_str) {
        Ok(mc_out) => {
            if mc_out == ocaml_out {
                TestResult::Pass
            } else {
                let s = format!("Expected: {:?}\nFound: {:?}\n", ocaml_out, mc_out);
                TestResult::Fail(s)
            }
        }
        Err(McError::CompileError { exit_code, stderr }) => {
            TestResult::Fail(format!("mc returned {}\nstderr:\n{}", exit_code, stderr))
        }
        Err(McError::RunError {
            exit_code,
            stderr,
            stdout,
        }) => TestResult::Fail(format!(
            "Generated program returned {}\nstderr: {:?}\nstdout: {:?})",
            exit_code, stderr, stdout
        )),
    }
}

// Run all .ml files in a directory as tests
fn run_dir(dir: &Path) {
    for entry in fs::read_dir(dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension() == Some(OsStr::new("ml")) {
            // println!("{:?}", path);
            print!("{} ... ", path.to_str().unwrap());
            match run_test(&path) {
                TestResult::Pass => println!("OK"),
                TestResult::Fail(reason) => {
                    println!("FAIL");
                    println!("{}", reason);
                }
            }
        } else if entry.file_type().unwrap().is_dir() {
            run_dir(&path);
        }
    }
}

fn main() {
    run_dir(Path::new("programs"));
}
