mod core;
mod env;
mod error;
mod hashkey;
mod reader;
mod value;

mod step8_macros;
use step8_macros::eval;
use step8_macros::main as mmain;

fn main() {
    let result = mmain();

    if let Err(e) = result {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
