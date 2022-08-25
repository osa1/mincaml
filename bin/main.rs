use std::process::exit;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.as_slice() {
        [_, ref file] => {
            libmc::compile_file_wasm(file, None, false, false, false);
            exit(libmc::compile_file(file, None, true, true, true));
        }
        _ => {
            println!("What do you mean?");
            exit(1);
        }
    }
}
