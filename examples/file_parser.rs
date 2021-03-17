extern crate clap;

use can_dbc;
use clap::{App, Arg};

use std::cmp;
use std::fs::File;
use std::io;
use std::io::prelude::*;

fn main() -> io::Result<()> {
    let matches = App::new("DBC Parser")
        .version("1.0")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("FILE")
                .help("DBC file path")
                .takes_value(true),
        )
        .get_matches();
    let path = matches.value_of("input").unwrap_or("./examples/sample.dbc");

    let mut f = File::open(path)?;
    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer)?;

    match can_dbc::DBC::from_slice(&buffer) {
        Ok(dbc_content) => println!("DBC Content{:#?}", dbc_content),
        Err(e) => {
            match e {
                can_dbc::Error::Nom(nom ) => eprintln!("Nom {:?}", nom),
                can_dbc::Error::Incomplete(dbc, remaining) => eprintln!("Not all data in buffer was read {:#?}, remaining unparsed (length: {}): {}\n...(truncated)", dbc, remaining.len(), String::from_utf8_lossy(&remaining[0..cmp::min(100, remaining.len())]).to_string())
            }
        }
    }

    Ok(())
}
