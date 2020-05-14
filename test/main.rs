use std::env;
use std::ffi::{OsStr, OsString};
use std::path::Path;
use std::process::{Command, Stdio};

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = Path::new(&args[1]);

    let file_name: &OsStr = file_path.file_stem().unwrap();

    let mut ocamlc_output: OsString = file_name.to_owned();
    ocamlc_output.push("_ocamlc");

    let ocamlc_ret = Command::new("ocamlc")
        .args(&[
            file_path.to_str().unwrap(),
            "-o",
            ocamlc_output.to_str().unwrap(),
        ])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    println!("ocamlc: {}", ocamlc_ret);

    let mc_ret = Command::new("target/debug/mc")
        .arg(file_path.to_str().unwrap())
        .stdout(Stdio::null())
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    println!("mc: {}", mc_ret);

    let gcc_rts_ret = Command::new("gcc")
        .args(&["rts.c", "-c"])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    println!("gcc rts.c: {}", gcc_rts_ret);

    let mut mc_output: OsString = file_name.to_owned();
    mc_output.push("_mc");

    let gcc_link_ret = Command::new("gcc")
        .args(&["a.o", "rts.o", "-o", mc_output.to_str().unwrap()])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    println!("gcc link: {}", gcc_link_ret);

    println!("Running ocamlc-generated executable.. {:?}", ocamlc_output);
    let _ = Command::new(&format!("./{}", ocamlc_output.to_str().unwrap()))
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    println!("$\nRunning mc-generated executable.. {:?}", mc_output);
    let _ = Command::new(&format!("./{}", mc_output.to_str().unwrap()))
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    println!("$");
}
