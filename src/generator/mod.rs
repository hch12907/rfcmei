use crate::document::Document;

mod html;

pub use html::Html;

pub trait Generator {
    fn name(&self) -> &'static str;
    
    fn generate(&self, document: &Document) -> String;
}
