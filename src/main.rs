mod core;
mod env;
mod error;
mod hashmapkey;
mod reader;
mod value;

mod stepA_mal;
use stepA_mal::eval;
use stepA_mal::main as mmain;

fn main() {
    let result = mmain();

    if let Err(e) = result {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
