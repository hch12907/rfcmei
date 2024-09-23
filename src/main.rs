use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use argh::FromArgs;
use tl::{Node, ParserOptions};

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

    let rfc_dom = tl::parse(&rfc_xhtml, ParserOptions::new()).map_err(|e| e.to_string())?;
    let parser = rfc_dom.parser();

    let mut count = 0;

    for page in rfc_dom.query_selector(r##"pre[class="newpage"]"##).unwrap() {
        let children = page.get(parser).unwrap().as_tag().unwrap().children();

        for child in children.all(parser) {
            match child {
                Node::Raw(bytes) => println!("[RAW] {}", bytes.as_utf8_str()),
                Node::Comment(_) => (),
                Node::Tag(tag)
                    if tag
                        .attributes()
                        .class()
                        .is_some_and(|cl| cl.as_bytes() == b"grey") => {}
                Node::Tag(tag) => println!("[TAG] {}", tag.raw().as_utf8_str()),
            }
        }

        count += 1;
        if count == 3 {
            break;
        }
    }

    Ok(())
}
