mod core;
mod env;
mod error;
mod hashkey;
mod reader;
mod value;

mod step9_try;
use step9_try::eval;
use step9_try::main as mmain;

fn main() {
    let result = mmain();

    if let Err(e) = result {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
