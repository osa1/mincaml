mod lexer;

use lexer::Lexer;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let contents = std::fs::read_to_string(&args[1]).unwrap();

    let mut lexer = Lexer::new(contents.as_bytes());
    loop {
        match lexer.next() {
            Err(err) => {
                println!("{:#?}", err);
                break;
            }
            Ok(token) => {
                println!("{:#?}", token);
            }
        }
    }
}
