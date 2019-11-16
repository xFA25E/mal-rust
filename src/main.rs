mod core;
mod env;
mod error;
mod hashkey;
mod reader;
mod value;

mod step4_if_fn_do;
use step4_if_fn_do::main as mmain;

fn main() {
    let result = mmain();

    if let Err(e) = result {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
