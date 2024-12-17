#![feature(let_chains)]

mod document;
mod generator;
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
use generator::{Generator, Html};
use html5gum::Tokenizer;
use tree::Tree;

#[derive(FromArgs)]
/// A tool to prettify RFC documents.
struct Args {
    /// print the output of specified phase
    #[argh(option, short = 'p')]
    phase: Option<u32>,

    /// the generator used to produce the final output
    #[argh(option, short = 'g', default = r#"String::from("html")"#)]
    generator: String,

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
    if args.phase == Some(1) {
        println!("{}", document.print());
        return Ok(());
    }

    let phase2 = Phase2Document::from_phase1(document)?;
    if args.phase == Some(2) {
        println!("{}", phase2.print());
        return Ok(());
    }

    let phase3 = Phase3Document::from_phase2(phase2)?;
    if args.phase == Some(3) {
        println!("{}", phase3.print());
        return Ok(());
    }

    let phase4 = Phase4Document::from_phase3(phase3)?;
    if args.phase == Some(4) {
        println!("{}", phase4.print());
        return Ok(());
    }

    let phase5 = Document::from_phase4(phase4);
    if args.phase == Some(5) {
        println!("{}", phase5.print());
        return Ok(());
    }

    if args.phase.is_none() {
        if args.generator == Html.name() {
            println!("{}", Html.generate(&phase5));
            return Ok(());
        } else {
            return Err(String::from("an unknown generator is specified"));
        }
    } else {
        return Err(String::from("an unknown phase is specified"));
    }
}
