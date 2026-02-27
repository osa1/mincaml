use std::process::exit;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.as_slice() {
        [_, file] => {
            exit(libmc::compile_file(file, None, true, true, false, true));
        }
        _ => {
            println!("What do you mean?");
            exit(1);
        }
    }
}
