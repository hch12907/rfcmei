//! In the first phase, HTML elements of the original RFC document are largely
//! degenerated. Only the most important tags (<meta>, <pre>, <a>, <span>) are
//! retained and in some cases the tags are merged. The contents are not analysed.
//!

use std::sync::LazyLock;

use regex::Regex;

use crate::tree::{Node, Tree};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum MetaElement {
    Author(Box<str>),
    Doi(Box<str>),
    PublicationDate(Box<str>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum Element {
    /// Ordinary text
    Text(Box<str>),

    // Below are the headings. The first field is always the innerHTML of the
    // heading, while the second field is its ID. <h1> doesn't have an ID.
    /// span[class="h1"] and <h1>
    H1(Box<str>),
    /// span[class="h2"] and <h2>; has an anchor
    H2(Box<str>, Box<str>),
    /// span[class="h3"] and <h3>; has an anchor
    H3(Box<str>, Box<str>),
    /// span[class="h4"] and <h4>; has an anchor
    H4(Box<str>, Box<str>),
    /// span[class="h5"] and <h5>; has an anchor
    H5(Box<str>, Box<str>),
    /// span[class="h6"] and <h6>; has an anchor
    H6(Box<str>, Box<str>),

    /// <span> or <a> used as location marker, optionally contains text
    Anchor(Box<str>, Option<Box<str>>),

    // Below are references. The first field is always the location of the
    // reference, while the second field is always the innerHTML of the original
    // <a> tag they originated from.
    /// Reference to another RFC document.
    /// e.g. "[RFC1234]"; saved as (1234, "RFC1234")
    DocReference(u32, Box<str>),
    /// Reference to a section in another RFC document.
    /// e.g. "Section 3 of RFC1234", saved as ((1234, "section-3"), "Section 3 of RFC1234")
    CrossReference((u32, Box<str>), Box<str>),
    /// Reference to a section in this RFC document.
    /// e.g. "Section 3", saved as ("section-3", "Section 3")
    SelfReference(Box<str>, Box<str>),
    /// Reference to another site.
    /// e.g. "[IANA]", saved as ("example.com", "IANA")
    ExtReference(Box<str>, Box<str>),
}

impl Element {
    pub(super) fn is_heading(&self) -> bool {
        matches!(
            self,
            Element::H1(_)
                | Element::H2(_, _)
                | Element::H3(_, _)
                | Element::H4(_, _)
                | Element::H5(_, _)
                | Element::H6(_, _)
        )
    }

    pub(super) fn is_reference(&self) -> bool {
        matches!(
            self,
            Element::DocReference(_, _)
                | Element::CrossReference(_, _)
                | Element::SelfReference(_, _)
                | Element::ExtReference(_, _)
        )
    }
}

pub struct Document {
    meta_info: Vec<MetaElement>,
    elements: Vec<Element>,
}

impl Document {
    pub fn from_html(html: Tree) -> Result<Self, String> {
        let mut this = Self {
            meta_info: Vec::new(),
            elements: Vec::new(),
        };

        let html = html.find_by_tag("html").next().unwrap();

        let head = html.find_by_tag("head").next().unwrap();

        for meta in head.find_by_tag("meta") {
            if let Some(parsed) = this.parse_meta(meta)? {
                this.meta_info.push(parsed);
            }
        }

        let body = html.find_by_tag("body").next().unwrap();

        let first_page = body.find_by_tag("pre").next().unwrap();

        let pages = body
            .find_by_tag("pre")
            .filter(|node| node.get("class") == Some("newpage"));

        let all_pages = Some(first_page).into_iter().chain(pages);

        for page in all_pages {
            for element in page
                .children()
                .iter()
                .filter(|node| node.get("class") != Some("grey"))
            {
                match element {
                    Node::Raw(content) => {
                        if let Some(Element::Text(ref mut last)) = this.elements.last_mut() {
                            let mut last_owned = String::from(std::mem::take(last));
                            last_owned.push_str(content);
                            *last = last_owned.into_boxed_str();
                        } else {
                            this.elements
                                .push(Element::Text(content.clone().into_boxed_str()))
                        }
                    }
                    tag @ Node::Tag { name, .. } if name == "a" => {
                        let a = this.parse_a(tag)?;
                        this.elements.push(a);
                    }
                    tag @ Node::Tag { name, .. } if name == "span" => {
                        let span = this.parse_span(tag)?;
                        this.elements.push(span);
                    }
                    Node::Tag { name, .. } => panic!("encountered unknown tag: {}", name),
                }
            }
        }

        Ok(this)
    }

    /// Print the document out in a Markdown-like syntax
    pub fn print(&self) -> String {
        let mut result = String::new();

        result.push_str("------\n");

        for meta in &self.meta_info {
            result.push_str(&format!("{:?}\n", meta));
        }

        result.push_str("------\n");

        for element in &self.elements {
            match element {
                Element::Text(txt) => result.push_str(txt),
                Element::H1(title) => {
                    result.push_str("# ");
                    result.push_str(title);
                }

                Element::H2(title, id) => result.push_str(&format!("## {} #{{{}}}", title, id)),
                Element::H3(title, id) => result.push_str(&format!("### {} #{{{}}}", title, id)),
                Element::H4(title, id) => result.push_str(&format!("#### {} #{{{}}}", title, id)),
                Element::H5(title, id) => result.push_str(&format!("##### {} #{{{}}}", title, id)),
                Element::H6(title, id) => result.push_str(&format!("###### {} #{{{}}}", title, id)),

                Element::Anchor(id, Some(text)) => result.push_str(&format!("${}, {}$", id, text)),
                Element::Anchor(id, None) => result.push_str(&format!("${}$", id)),

                Element::DocReference(rfc, title) => {
                    result.push_str(&format!("[{}](^doc rfc{}^)", title, rfc))
                }
                Element::CrossReference((rfc, sect), title) => {
                    result.push_str(&format!("[{}](^cross rfc{}#{}^)", title, rfc, sect))
                }
                Element::SelfReference(sect, title) => {
                    result.push_str(&format!("[{}](^self #{}^)", title, sect))
                }
                Element::ExtReference(href, t) => result.push_str(&format!("[{}]({})", href, t)),
                // _ => result.push_str("@"),
            }
        }
        result
    }

    pub(super) fn meta_info(&self) -> &[MetaElement] {
        &self.meta_info
    }

    pub(super) fn elements(&self) -> &[Element] {
        &self.elements
    }

    pub(super) fn from_raw_parts(meta_info: Vec<MetaElement>, elements: Vec<Element>) -> Self {
        Self {
            meta_info,
            elements,
        }
    }

    pub(super) fn to_raw_parts(self) -> (Vec<MetaElement>, Vec<Element>) {
        (self.meta_info, self.elements)
    }

    /// This function traverses the document upwards, optionally skipping one section of
    /// Element::Text, if the Text contains only whitespace. If the first encountered element
    /// is not Text and does not match predicate, None is returned
    fn find_upwards_mut<F>(&mut self, remove_skipped: bool, predicate: F) -> Option<&mut Element>
    where
        F: Fn(&Element) -> bool,
    {
        // XXX: Massive hack!! The problem is, the first last_mut() call causes rustc
        // to think we are borrowing self.elements for the entire function, but we
        // are not...
        let first_borrow: &mut Vec<Element> = unsafe { &mut *(&mut self.elements as *mut _) };

        if let Some(x) = first_borrow.last_mut() {
            if predicate(x) {
                return Some(x);
            }
        }

        if self.elements.len() < 2 {
            return None;
        }

        let should_pop = 'outer: {
            if let Some(Element::Text(text)) = self.elements.last()
                && text.chars().all(|c| c.is_ascii_whitespace())
            {
                if let Some(x) = self.elements.get(self.elements.len() - 2) {
                    if predicate(x) {
                        break 'outer true;
                    }
                }
            }

            false
        };

        if should_pop && remove_skipped {
            self.elements.pop();
            self.elements.last_mut()
        } else if should_pop {
            let idx = self.elements.len() - 2;
            self.elements.get_mut(idx)
        } else {
            None
        }
    }

    fn parse_a(&mut self, node: &Node) -> Result<Element, String> {
        static DOC_REFERENCE_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"^./rfc(\d+)$").unwrap());

        static CROSS_REFERENCE_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"^./rfc(\d+)#(.+)$").unwrap());

        static SELF_REFERENCE_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"^#(.+)$").unwrap());

        match node {
            Node::Tag { name, attr, inner } if name == "a" => {
                let Some(href) = attr.get("href") else {
                    // In the reference section of RFC documents, <a> tags without
                    // a href are found. They serve as anchors.
                    if attr.contains_key("id") && inner.len() <= 1 {
                        let id = attr["id"].clone().into_boxed_str();

                        return if let Some(Node::Raw(inner)) = inner.get(0) {
                            let inner = inner.clone().into_boxed_str();
                            Ok(Element::Anchor(id, Some(inner)))
                        } else if let None = inner.get(0) {
                            Ok(Element::Anchor(id, None))
                        } else {
                            Err("unexpected nested tag in anchor <a>".to_string())
                        };
                    }
                    return Err("encountered <a> tag without href".to_string());
                };
                let [Node::Raw(inner), ..] = inner.as_slice() else {
                    Err("encountered <a> tag without innerHTML".to_string())?
                };
                let inner = inner.clone().into_boxed_str();

                if let Some(captured) = CROSS_REFERENCE_REGEX.captures(href) {
                    let rfc = captured[1].parse::<u32>().unwrap();
                    let section = captured[2].to_owned().into_boxed_str();
                    Ok(Element::CrossReference((rfc, section), inner))
                } else if let Some(captured) = DOC_REFERENCE_REGEX.captures(href) {
                    let rfc = captured[1].parse::<u32>().unwrap();
                    Ok(Element::DocReference(rfc, inner))
                } else if let Some(captured) = SELF_REFERENCE_REGEX.captures(href) {
                    let section = captured[1].to_owned().into_boxed_str();
                    Ok(Element::SelfReference(section, inner))
                } else {
                    Ok(Element::ExtReference(href.clone().into_boxed_str(), inner))
                }
            }

            _ => Err("parse_a() encountered unexpected html element".into()),
        }
    }

    fn parse_meta(&mut self, node: &Node) -> Result<Option<MetaElement>, String> {
        match node {
            Node::Tag { name, attr, .. } if name == "meta" => {
                let get_content = || {
                    attr.get("content")
                        .map(|x| x.to_string().into_boxed_str())
                        .ok_or_else(|| String::from("meta attr has `name` but not `content`"))
                };

                Ok(match attr.get("name").map(String::as_str) {
                    Some("citation_author") => Some(MetaElement::Author(get_content()?)),
                    Some("citation_doi") => Some(MetaElement::Doi(get_content()?)),
                    Some("citation_pub(super)lication_date") => {
                        Some(MetaElement::PublicationDate(get_content()?))
                    }
                    _ => None,
                })
            }

            _ => Err("parse_meta() encountered unexpected html element".into()),
        }
    }

    fn parse_span(&mut self, node: &Node) -> Result<Element, String> {
        match node {
            Node::Tag { name, attr, inner } if name == "span" => {
                // Some spans are empty and serve a single purpose: anchor, aka fragment
                // identifier.
                if inner.len() == 0 && attr.contains_key("id") && !attr.contains_key("class") {
                    let id = attr["id"].clone().into_boxed_str();
                    return Ok(Element::Anchor(id, None));
                }

                // The section titles in RFC documents have an index number that
                // causes them to have an <a> tag.
                // <span><a>content-first</a>content-rest</span>
                let (id, inner) = if let [first @ Node::Tag { .. }, rest @ ..] = inner.as_slice() {
                    let id = if first.name() == Some("a") {
                        let Element::SelfReference(section, _) = self.parse_a(first)? else {
                            eprintln!("{:?}", node);
                            return Err("unexpected hyperlink in <span><a>".to_string());
                        };
                        Some(section)
                    } else {
                        None
                    };

                    let mut new_inner = String::new();
                    for child in first.children().iter().chain(rest.iter()) {
                        match child {
                            Node::Raw(content) => new_inner.push_str(content),
                            _ => return Err("unexpected nested tag in <span><a>".to_string()),
                        }
                    }

                    (id, new_inner)
                }
                // <span>content-inner</span>
                else if let [Node::Raw(inner), ..] = inner.as_slice() {
                    (None, inner.clone())
                }
                // <span></span>, without id attribtue
                else {
                    eprintln!("{:?}", node);
                    return Err("encountered <span> tag without innerHTML".to_string());
                };

                let inner = inner.clone().into_boxed_str();

                match (attr.get("class").map(|x| x.as_str()), id) {
                    (Some("h1"), None) => {
                        if let Some(Element::H1(ref mut last)) =
                            self.find_upwards_mut(true, |el| el.is_heading())
                        {
                            // We find a title that is split across multiple lines:
                            //              <span class="h1">A very long</span>
                            //         <span class="h1">title which is split across lines</span>
                            
                            let mut last_owned = String::from(std::mem::take(last));
                            last_owned.push(' ');
                            last_owned.push_str(&inner);

                            *last = last_owned.into_boxed_str();

                            // Give the function something to return...
                            Ok(self.elements.pop().unwrap())
                        } else {
                            Ok(Element::H1(inner))
                        }
                    }
                    (Some("h2"), Some(id)) => Ok(Element::H2(inner, id)),
                    (Some("h3"), Some(id)) => Ok(Element::H3(inner, id)),
                    (Some("h4"), Some(id)) => Ok(Element::H4(inner, id)),
                    (Some("h5"), Some(id)) => Ok(Element::H5(inner, id)),
                    (Some("h6"), Some(id)) => Ok(Element::H6(inner, id)),

                    (Some("h1"), Some(_)) => {
                        Err("encountered <span class=\"h1\"> with hyperlink".to_string())
                    }
                    (Some("h2" | "h3" | "h4" | "h5" | "h6"), None) => {
                        // In this case, we find a section title that is split across two
                        // lines:
                        //
                        // <span class="h2"><a href="...">12.34.56</a> Some massively long title</span>
                        // <span class="h2">         that causes it to be line-wrapped</span>
                        //
                        // We want to merge them back.
                        if let Some(
                            Element::H2(ref mut last, id)
                            | Element::H3(ref mut last, id)
                            | Element::H4(ref mut last, id)
                            | Element::H5(ref mut last, id)
                            | Element::H6(ref mut last, id),
                        ) = self.find_upwards_mut(true, |el| el.is_heading())
                        {
                            // ASSUME:
                            // (1) id is section-x.x.x.x ; we take the "x.x.x.x" part
                            // (2) the first line starts with "x.x.x.x."
                            // (3) the first line, after "x.x.x.x.", contains some spaces before
                            //     the actual title
                            // (4) the next line has spaces for text alignment
                            //
                            // Below, "fl" means first line, "nl" means second line.
                            let inner = if let Some(section) = id.split('-').skip(1).next()
                                && last.starts_with(section)
                            {
                                let fl_without_section = last
                                    .strip_prefix(section)
                                    .unwrap()
                                    .strip_prefix(". ")
                                    .unwrap();

                                let fl_cleared =
                                    fl_without_section.trim_start_matches(|c| c == ' ');

                                let cleared_len = last.len() - fl_cleared.len();

                                let nl_initial_spaces =
                                    inner.chars().take_while(|c| *c == ' ').count();

                                if nl_initial_spaces == cleared_len {
                                    inner.trim_start_matches(' ')
                                } else {
                                    eprintln!("Failed headings-merger assumption at {}", id);
                                    &inner
                                }
                            } else {
                                &inner
                            };

                            // We finally merge the two together.
                            // Insert a space to accomodate for the newline we removed
                            let mut last_inner = String::from(std::mem::take(last));
                            last_inner.push(' ');
                            last_inner.push_str(&inner);
                            *last = last_inner.into_boxed_str();

                            // Give the function something to return...
                            Ok(self.elements.pop().unwrap())
                        } else {
                            Err("encountered <span class=\"h2...h6\"> without hyperlink"
                                .to_string())
                        }
                    }
                    (Some(unknown), _) => {
                        Err(String::from("encountered <span> tag with unknown class ") + unknown)
                    }
                    (None, _) => Err("encountered <span> tag without class".to_string()),
                }
            }

            _ => Err("parse_span() encountered unexpected html element".into()),
        }
    }
}
