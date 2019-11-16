mod core;
mod env;
mod error;
mod hashkey;
mod reader;
mod value;

mod step5_tco;
use step5_tco::main as mmain;

fn main() {
    let result = mmain();

    if let Err(e) = result {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
