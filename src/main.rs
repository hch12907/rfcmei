#![feature(let_chains)]

mod document;
mod tree;

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use argh::FromArgs;
use document::phase2::Phase2Document;
use document::Document;
use html5gum::Tokenizer;
use tree::Tree;

#[derive(FromArgs)]
/// A tool to prettify RFC documents.
struct Args {
    /// path to the RFC file (in HTML)
    #[argh(positional)]
    path: PathBuf,
}

fn main() -> Result<(), String> {
    let args = argh::from_env::<Args>();

    let mut file = File::open(&args.path).map_err(|e| e.to_string())?;

    let mut rfc_xhtml = String::with_capacity(65536);
    file.read_to_string(&mut rfc_xhtml)
        .map_err(|e| e.to_string())?;

    let dom = Tree::from_tokens(Tokenizer::new(&rfc_xhtml).infallible()).unwrap();

    let document = Document::from_html(dom)?;
    // println!("{}", document.print());

    let phase2 = Phase2Document::from_phase1(document)?;
    println!("{}", phase2.print());

    Ok(())
}
