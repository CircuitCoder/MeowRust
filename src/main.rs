mod grammar;
mod parser;
mod typing;
mod registry;

use failure::Error;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

#[derive(structopt::StructOpt)]
struct Args {
  #[structopt(name = "FILE")]
  file: PathBuf,
}

#[paw::main]
fn main(args: Args) -> Result<(), Error> {
  println!(
    "Processing file {}...",
    args.file.as_path().to_str().unwrap()
  );
  let mut content = String::new();
  File::open(args.file)?.read_to_string(&mut content)?;
  let parsed = parser::parse(&content);
  println!("Success!");
  println!("{:#?}", parsed);
  Ok(())
}
