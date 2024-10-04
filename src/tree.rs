//! TODO: Replace this with rcdom or whatever is mature in Rust's ecosystem.

use std::collections::BTreeMap;

use html5gum::{HtmlString, StartTag, Token};

/// A very rudimentary representation of a HTML document, but it's good enough
/// for our purposes
#[derive(Debug, Clone)]
pub struct Tree {
    root: Node,
}

impl Tree {
    pub fn from_tokens<I>(iter: I) -> Result<Self, String>
    where
        I: Iterator<Item = Token>,
    {
        let mut peek_iter = iter.peekable();
        let mut root = Vec::new();

        while peek_iter.peek().is_some() {
            match Node::from_tokens(&mut peek_iter) {
                NodeResult::Ok(node) => root.push(node),
                NodeResult::Err(e) => Err(e)?,
                NodeResult::EndTag { tag, .. } => panic!("dangling end tag: {}", tag),
                NodeResult::Nothing => (),
            }
        }
        Ok(Self {
            root: Node::Tag {
                name: String::new(),
                attr: BTreeMap::new(),
                inner: root,
            },
        })
    }

    #[expect(dead_code)]
    pub fn children(&self) -> &[Node] {
        match &self.root {
            Node::Tag { inner, .. } => inner,

            _ => unreachable!(),
        }
    }

    pub fn find_by_tag<'a, 'b>(&'a self, tag: &'b str) -> NodeTagIter<'b, 'a> {
        NodeTagIter {
            tag,
            node: &self.root,
            current_index: 0,
        }
    }

    #[expect(dead_code)]
    pub fn find_by_attr<'a, 'b>(&'a self, attr: (&'b str, &'b str)) -> NodeAttrIter<'b, 'a> {
        NodeAttrIter::new(attr, &self.root)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Raw(String),
    Tag {
        name: String,
        attr: BTreeMap<String, String>,
        inner: Vec<Self>,
    },
    // HTML comments are ignored
    // Comment(String),
}

enum NodeResult {
    Ok(Node),
    Err(String),
    EndTag { leftover: Node, tag: String },
    Nothing,
}

#[allow(unused)]
type AttrIter<'a> = std::collections::btree_map::Iter<'a, String, String>;

impl Node {
    pub fn find_by_tag<'a, 'b>(&'a self, tag: &'b str) -> NodeTagIter<'b, 'a> {
        NodeTagIter {
            tag,
            node: self,
            current_index: 0,
        }
    }

    #[expect(dead_code)]
    pub fn find_by_attr<'a, 'b>(&'a self, attr: (&'b str, &'b str)) -> NodeAttrIter<'b, 'a> {
        NodeAttrIter::new(attr, self)
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            Node::Raw(_) => None,
            Node::Tag { ref name, .. } => Some(name),
        }
    }

    pub fn get(&self, attr_key: &str) -> Option<&str> {
        match &self {
            Node::Tag { attr, .. } => attr.get(attr_key).map(|v| v.as_str()),
            _ => None,
        }
    }

    #[allow(unused)]
    pub fn attributes(&self) -> Option<AttrIter> {
        match &self {
            Node::Tag { attr, .. } => Some(attr.iter()),
            _ => None,
        }
    }

    pub fn children(&self) -> &[Node] {
        match &self {
            Node::Tag { inner, .. } => inner,
            _ => &[],
        }
    }

    fn from_tokens<I>(iter: &mut I) -> NodeResult
    where
        I: Iterator<Item = Token>,
    {
        Self::from_tokens_with_start_tag(iter, None)
    }

    fn normalize_start_tag(
        tag: StartTag,
    ) -> Result<(String, BTreeMap<String, String>, bool), String> {
        let start = tag.name.to_string()?;
        let attributes = tag
            .attributes
            .into_iter()
            .map(|(k, v)| -> Result<(String, String), String> {
                let k = k.to_string()?;
                let v = v.to_string()?;
                Ok((k, v))
            })
            .collect::<Result<BTreeMap<String, String>, String>>()?;

        Ok((start, attributes, tag.self_closing))
    }

    fn from_tokens_with_start_tag<I>(iter: &mut I, start: Option<StartTag>) -> NodeResult
    where
        I: Iterator<Item = Token>,
    {
        let this = start.map(Self::normalize_start_tag).transpose();
        let mut this = match this {
            Ok(t) => t,
            Err(e) => return NodeResult::Err(e),
        };
        let mut inner = Vec::new();
        let mut end_tag = None;

        // If we have a self closing tag, act as if we have received a Token::EndTag,
        // that is, we jump to the end. Rust doesn't have a goto though, so let's
        // make do with an if statement...
        let goto_end = this
            .as_ref()
            .map(|t| {
                // RFC consistently uses <br /> because it follows XHTML, but let's be
                // more lenient anyway...
                t.2 || t.0 == "br"
            })
            .unwrap_or(false);

        if !goto_end {
            while let Some(token) = iter.next() {
                match token {
                    Token::StartTag(tag) if this.is_some() => {
                        // println!("started {}", tag.name.as_lossy_str());
                        match Self::from_tokens_with_start_tag(iter, Some(tag)) {
                            NodeResult::Ok(node) => inner.push(node),
                            NodeResult::EndTag { leftover, tag } => {
                                inner.push(leftover);

                                if tag != this.as_ref().unwrap().0 {
                                    end_tag = Some(tag);
                                }

                                break;
                            }
                            NodeResult::Nothing => (),
                            e @ NodeResult::Err(_) => return e,
                        }
                    }
                    Token::StartTag(tag) => {
                        let should_break = tag.self_closing;
                        // println!("started {}", tag.name.as_lossy_str());
                        this = match Self::normalize_start_tag(tag) {
                            Ok(tag) => Some(tag),
                            Err(e) => return NodeResult::Err(e),
                        };

                        if should_break {
                            break;
                        }
                    }
                    Token::String(s) => {
                        let s = match s.to_string() {
                            Ok(s) => s,
                            Err(e) => return NodeResult::Err(e),
                        };
                        inner.push(Self::Raw(s));
                    }
                    Token::EndTag(tag) if this.is_some() => {
                        if this.as_ref().unwrap().0.as_bytes() == *tag.name {
                            // println!("ended {}", this.as_ref().unwrap().0);
                        } else {
                            end_tag = match tag.name.to_string() {
                                Ok(tag) => Some(tag),
                                Err(e) => return NodeResult::Err(e),
                            }
                        }
                        break;
                    }
                    Token::EndTag(tag) => {
                        panic!("dangling end tag {}", tag.name.as_lossy_str())
                    }
                    Token::Error(e) => return NodeResult::Err(e.to_string()),
                    Token::Comment(_) => {}
                    Token::Doctype(_) => {}
                }
            }
        }

        let result = if let Some(this) = this {
            Self::Tag {
                name: this.0,
                attr: this.1,
                inner,
            }
        } else if !inner.is_empty() {
            return NodeResult::Nothing;
        } else {
            return NodeResult::Err(String::from("didn't receive a meaningful HTML document"));
        };

        if let Some(end_tag) = end_tag {
            NodeResult::EndTag {
                leftover: result,
                tag: end_tag,
            }
        } else {
            NodeResult::Ok(result)
        }
    }
}

pub struct NodeTagIter<'a, 'b> {
    tag: &'a str,
    node: &'b Node,
    current_index: usize,
}

impl<'a, 'b> Iterator for NodeTagIter<'a, 'b> {
    type Item = &'b Node;

    fn next(&mut self) -> Option<Self::Item> {
        match self.node {
            Node::Raw(_) => None,
            Node::Tag { inner, .. } => {
                for (i, node) in inner.iter().enumerate().skip(self.current_index) {
                    if matches!(node, Node::Tag { name, .. } if name == self.tag) {
                        self.current_index = i + 1;
                        return Some(node);
                    }
                }

                None
            }
        }
    }
}

pub struct NodeAttrIter<'a, 'b> {
    attr: NodeAttr<'a>,
    node: &'b Node,
    current_index: usize,
}

enum NodeAttr<'a> {
    Class(&'a str),
    Normal(&'a str, &'a str),
}

impl<'a, 'b> NodeAttrIter<'a, 'b> {
    fn new(attr: (&'a str, &'a str), node: &'b Node) -> Self {
        let attr = if attr.0 == "class" {
            NodeAttr::Class(attr.1)
        } else {
            NodeAttr::Normal(attr.0, attr.1)
        };

        Self {
            attr,
            node,
            current_index: 0,
        }
    }
}

impl<'a, 'b> Iterator for NodeAttrIter<'a, 'b> {
    type Item = &'b Node;

    fn next(&mut self) -> Option<Self::Item> {
        match (self.node, &self.attr) {
            (Node::Raw(_), _) => None,
            (Node::Tag { inner, .. }, &NodeAttr::Normal(key, val)) => {
                for (i, node) in inner.iter().enumerate().skip(self.current_index) {
                    match node {
                        Node::Tag { attr, .. }
                            if attr.get(key).map(|v| v == val).unwrap_or(false) =>
                        {
                            self.current_index = i + 1;
                            return Some(node);
                        }
                        _ => continue,
                    }
                }

                None
            }

            (Node::Tag { inner, .. }, &NodeAttr::Class(val)) => {
                for (i, node) in inner.iter().enumerate().skip(self.current_index) {
                    match node {
                        Node::Tag { attr, .. } if attr.contains_key("class") => {
                            if attr["class"].split(' ').any(|cls| cls == val) {
                                self.current_index = i + 1;
                                return Some(node);
                            }
                        }
                        _ => continue,
                    }
                }

                None
            }
        }
    }
}

// Strictly for utility purposes
trait HtmlStringExt {
    fn as_lossy_str(&self) -> std::borrow::Cow<'_, str>;

    fn to_string(self) -> Result<String, String>;
}

impl HtmlStringExt for HtmlString {
    fn as_lossy_str(&self) -> std::borrow::Cow<'_, str> {
        String::from_utf8_lossy(self)
    }

    fn to_string(self) -> Result<String, String> {
        String::from_utf8(self.0).map_err(|e| e.to_string())
    }
}
