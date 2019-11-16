mod core;
mod env;
mod error;
mod hashkey;
mod reader;
mod value;

mod step6_file;
use step6_file::eval;
use step6_file::main as mmain;

fn main() {
    let result = mmain();

    if let Err(e) = result {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
