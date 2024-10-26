#![feature(let_chains)]

mod document;
mod tree;

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use argh::FromArgs;
use document::phase1::Phase1Document;
use document::phase2::Phase2Document;
use document::phase3::Phase3Document;
use document::phase4::Phase4Document;
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

    let document = Phase1Document::from_html(dom)?;
    // println!("{}", document.print());

    let phase2 = Phase2Document::from_phase1(document)?;
    // println!("{:#?}", phase2);
    // println!("{}", phase2.print());

    let phase3 = Phase3Document::from_phase2(phase2)?;
    // println!("{}", phase3.print());

    let phase4 = Phase4Document::from_phase3(phase3)?;
    // println!("{}", phase4.print());

    let phase5 = Document::from_phase4(phase4);
    println!("{}", phase5.print());

    Ok(())
}
