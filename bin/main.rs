use libmc;

use std::process::exit;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.as_slice() {
        [_, ref file] => {
            exit(libmc::compile_file(file, true, true, true));
        }
        _ => {
            println!("What do you mean?");
            exit(1);
        }
    }
}
