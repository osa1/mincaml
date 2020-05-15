use std::env;
use std::path::Path;
use std::process::{Command, Output, Stdio};

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

fn run_mc(file_path_str: &str) -> String {
    let file_path = Path::new(file_path_str);
    let file_name = file_path.file_stem().unwrap();
    let file_name_str = file_name.to_str().unwrap();

    let mc_out = Command::new("target/debug/mc")
        .arg(file_path_str)
        .stdout(Stdio::null())
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    assert!(mc_out.success());

    let gcc_out = Command::new("gcc")
        .args(&["rts.c", "-c"])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    assert!(gcc_out.success());

    let mut mc_output = file_name_str.to_owned();
    mc_output.push_str("_mc");

    let gcc_out = Command::new("gcc")
        .args(&["a.o", "rts.o", "-o", &mc_output])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    assert!(gcc_out.success());

    let run_out = Command::new(&format!("./{}", mc_output))
        .stdout(Stdio::piped())
        .spawn()
        .unwrap()
        .wait_with_output()
        .unwrap();

    assert!(run_out.status.success());

    String::from_utf8(run_out.stdout).unwrap()
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path_str = &args[1];

    let ocaml_out = run_ocaml(file_path_str);
    let mc_out = run_mc(file_path_str);

    println!("ocaml_out: {}", ocaml_out);
    println!("mc_out: {}", ocaml_out);

    assert_eq!(mc_out, ocaml_out);
}
