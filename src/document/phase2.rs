//! In this phase, analysis of text content begin. The sections are identified, the
//! textual contents are determined to whether be paragraphs, lists (ordered or not),
//! or preformatted sections (referred to as Code in this codebase).

use std::collections::BTreeMap;
use std::sync::LazyLock;

use regex::Regex;

use super::phase1::{Document as Phase1Document, Element as Phase1Element, MetaElement};

/// This struct represents the information found in the topmost section of all RFC
/// contents
#[derive(Debug, Clone, Default)]
pub struct StartInfo {
    // Left side
    stream: Box<str>,
    rfc: u32,
    obsoletes: Vec<(u32, Box<str>)>,
    date: Box<str>,
    category: Box<str>,
    others: BTreeMap<String, String>,

    // Right side
    authors: Vec<(Box<str>, Box<str>)>, // (author, organization)
}

#[derive(Debug, Clone, Default)]
pub struct Section {
    id: Option<Box<str>>, // An optional fragment-identifier for the section.
    level: usize,         // Level of the section, starts from 0 as the outermost level
    title: Box<str>,
    elements: Vec<Element>,
}

impl Section {
    // fn add_line_to_paragraph(&mut self, line: &str) -> Result<(), String> {
    //     if self.elements.is_empty() && !line.is_empty() {
    //         if !line.starts_with(' ') {
    //             self.title = line.trim().to_owned().into_boxed_str();
    //         }
    //     }
    // }

    fn add_element(&mut self, element: ParagraphElement) -> Result<(), String> {
        match self.elements.last_mut() {
            Some(el) => {
                if let Some(new_element) = el.add_paragraph_element(element)? {
                    self.elements.push(new_element);
                }
                Ok(())
            }
            _ => {
                eprintln!("{:?}", self);
                eprintln!("{:?}", element);
                Err("attempted to add non-text element to empty paragraph".to_string())
            }
        }
    }

    // fn add_to_unordered_list(&mut self, element: ParagraphElement) -> Result<(), String> {
    //     match self.elements.last_mut() {
    //         Some(Element::UnorderedList { ref mut items, .. }) => Ok(items.push(element)),
    //         _ => {
    //             eprintln!("{:?}", self);
    //             eprintln!("{:?}", element);
    //             Err("attempted to add non-text element to empty paragraph".to_string())
    //         }
    //     }
    // }

    /// Criteria to start a new paragraph:
    ///
    /// True when the section is empty, OR the very last element of the
    /// section is empty (i.e. there was an empty line before)
    fn can_start_new_paragraph(&self) -> bool {
        self.elements
            .last()
            .map(|el| el.has_encountered_blank_line())
            .unwrap_or(true)
    }
}

#[derive(Debug, Clone)]
pub(super) enum Element {
    /// Ordinary paragraph. Note that a paragraph MUST NOT contain empty lines.
    /// Any empty lines will create a new paragraph.
    Paragraph {
        depth: u32,
        elements: Vec<ParagraphElement>,
    },
    /// List with a starting number. Each list item may hold multiple paragraphs, hence
    /// they are made a Vec of Element.
    ///
    /// Depth is defined as number of characters before the actual content (spaces and
    /// the item marker itself), so `1. An item` will have a depth of 2.
    OrderedList {
        depth: u32,
        starting: u32,
        style: OrderedListStyle,
        items: Vec<Self>,
    },
    /// List without any numbering, it may start with a `- ` or a `o `. Each list item
    /// may hold multiple paragraphs, hence they are made a Vec of Element.
    ///
    /// Depth is defined as number of characters before the actual content (spaces and
    /// the item marker itself), so `- An item` will have a depth of 1.
    UnorderedList { depth: u32, items: Vec<Self> },
    /// A preformatted block of text. The newlines are all preserved in Code, but
    /// the common indentation (`min(number_of_space(line) for line in lines)`) is not.
    Code {
        depth: u32,
        elements: Vec<ParagraphElement>,
    },
}

impl Element {
    fn depth(&self) -> u32 {
        match self {
            Element::Paragraph { depth, .. } => *depth,
            Element::OrderedList { depth, .. } => *depth,
            Element::UnorderedList { depth, .. } => *depth,
            Element::Code { depth, .. } => *depth,
        }
    }

    fn from_line(line: &str, is_partial: bool) -> Self {
        // Use encountered_newline() upon meeting a newline!
        debug_assert!(!line.contains('\n'));

        let trimmed_line = line.trim_start_matches(' ');
        let line_depth = (line.len() - trimmed_line.len()) as u32;

        if line_depth > 0 && Self::is_likely_code(line, is_partial) {
            Self::Code {
                depth: 0,
                elements: vec![ParagraphElement::Text(line.into())],
            }
        } else if let Some(depth) = Self::unordered_list_item_depth(line)
        {
            Self::UnorderedList {
                depth,
                items: vec![Self::from_line(&line[depth as usize..], is_partial)],
            }
        } else if let Some((style, starting, _, content)) =
            OrderedListStyle::extract_from_line(trimmed_line)
        {
            Self::OrderedList {
                depth: line_depth,
                style,
                starting,
                items: vec![Self::from_line(content, is_partial)],
            }
        } else {
            Self::Paragraph {
                depth: line_depth,
                elements: vec![ParagraphElement::Text(trimmed_line.into())],
            }
        }
    }

    fn is_likely_code(line: &str, is_partial: bool) -> bool {
        const GRAPHICAL_CANDIDATES: [u8; 8] = [b'-', b'+', b'/', b'_', b':', b'*', b'\\', b'|'];
        const CODE_SCORE_THRESHOLD: usize = 650;

        let trimmed_line = line.trim_start_matches(' ');
        let line_depth = line.len() - trimmed_line.len();

        // A normal paragraph starts with 3 spaces.
        let mut excessive_spaces = line_depth.saturating_sub(3);
        let mut graphical_chars = 0;
        let nonalphabet_count = trimmed_line
            .bytes()
            .map(|b| !(b.is_ascii_alphabetic() || b > 0x7F) as usize)
            .fold(0, |acc, x| acc + x);

        let properly_ended = trimmed_line
            .rfind(&['.', ':'])
            .map(|idx| !trimmed_line[idx + 1..].bytes().any(|b| b != b' '))
            .unwrap_or(false);

        if !properly_ended && !is_partial {
            excessive_spaces += 72usize.saturating_sub(line.len());
        }

        // Counting excessive spaces - spaces which are not inbetween two alphabetic chars
        // Use bytes because spaces are 1-byte in UTF8
        for i in 1..trimmed_line.as_bytes().len().saturating_sub(1) {
            let trimmed_line = trimmed_line.as_bytes();
            let char1 = trimmed_line[i - 1];
            let char2 = trimmed_line[i];
            let char3 = trimmed_line[i + 1];

            if char2 == b' ' && char1 == char2 && char1 == char3 {
                excessive_spaces += 1
            }
        }

        for b in trimmed_line.bytes() {
            graphical_chars += GRAPHICAL_CANDIDATES.contains(&b) as usize;
        }

        // Cheats! For known patterns, we artificially inflate the score
        if trimmed_line.contains("0                   1                   2                   3") {
            graphical_chars += 100;
        } else if trimmed_line
            .contains("0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1")
        {
            graphical_chars += 100;
        }

        let score = nonalphabet_count * 10 + excessive_spaces * 30 + graphical_chars * 70;

        score > CODE_SCORE_THRESHOLD
    }

    fn add_paragraph_element(&mut self, element: ParagraphElement) -> Result<Option<Self>, String> {
        match element {
            ParagraphElement::Text(_) => panic!("use add_line()"),
            para @ (ParagraphElement::DocReference(_, _)
            | ParagraphElement::CrossReference(_, _)
            | ParagraphElement::SelfReference(_, _)
            | ParagraphElement::ExtReference(_, _)
            | ParagraphElement::Anchor(_, _)) => {
                match self {
                    Element::Paragraph {
                        ref mut elements, ..
                    } => elements.push(para),
                    Element::OrderedList { items, .. } => {
                        return items.last_mut().unwrap().add_paragraph_element(para)
                    }
                    Element::UnorderedList { items, .. } => {
                        return items.last_mut().unwrap().add_paragraph_element(para)
                    }
                    Element::Code { elements, .. } => elements.push(para),
                }
                Ok(None)
            }
        }
    }

    /// Add a new line to the [Element]. If the additions succeeds, the current [Element]
    /// is modified in place and a new [Element] may be returned. The new element needs
    /// to be inserted after the current one.
    fn add_line(&mut self, line: &str, is_partial: bool) -> Result<Option<Self>, String> {
        // Use encountered_newline() upon meeting a newline!
        debug_assert!(!line.contains('\n'));

        let line_depth = line.chars().take_while(|c| *c == ' ').count() as u32;

        let can_start_paragraph = self.has_encountered_blank_line();

        match self {
            Element::Paragraph {
                depth,
                ref mut elements,
            } => {
                if let Some(para @ ParagraphElement::Text(_)) = elements.last_mut() {
                    if line_depth < *depth {
                        return Ok(Some(Element::from_line(line, is_partial)));
                    }

                    para.add_line(line)?;
                    Ok(None)
                } else {
                    if !can_start_paragraph {
                        elements.push(ParagraphElement::Text(line.trim_start().to_owned()));
                        Ok(None)
                    } else {
                        Ok(Some(Element::from_line(line, is_partial)))
                    }
                }
            }

            Element::OrderedList {
                depth,
                style,
                ref mut items,
                ..
            } => {
                let inner_depth = items.last().map(|item| item.depth()).unwrap();

                if let Some((style_line, _, _, content)) = OrderedListStyle::extract_from_line(line)
                {
                    if *style != style_line {
                        return Ok(Some(Element::from_line(line, is_partial)));
                    }

                    let item_depth = (line.len() - content.len()) as u32;

                    // Allow some tolerance for cmp(depth, item_depth)...
                    let depth_difference = (item_depth as i32) - (*depth as i32);

                    if depth_difference.abs() <= 1 {
                        // item_depth == depth
                        // Add an item to the paragraph
                        items.push(Element::from_line(content, is_partial));
                        Ok(None)
                    } else if depth_difference < -1 {
                        // item_depth < depth
                        // Close the ordered list, start a new one
                        Ok(Some(Element::from_line(line, is_partial)))
                    } else {
                        // item_depth > depth
                        eprintln!("warning: got a nested list");
                        items.push(Element::from_line(content, is_partial));
                        Ok(None)
                    }
                } else {
                    if inner_depth <= line_depth {
                        // Continue the paragraph (or create a new one if blank line)
                        if inner_depth < line_depth {
                            eprintln!("warning: got a deep paragraph in list, merging with the shallow one")
                        }

                        if can_start_paragraph {
                            items.push(Element::from_line(line.trim_start(), is_partial))
                        } else {
                            let new_element = items
                                .last_mut()
                                .map(|el| el.add_line(line.trim_start(), is_partial))
                                .unwrap()?;

                            if let Some(el) = new_element {
                                items.push(el);
                            }
                        }

                        Ok(None)
                    } else {
                        // if inner_depth > line_depth
                        // Close the ordered list, start a new paragraph
                        Ok(Some(Element::from_line(line, is_partial)))
                    }
                }
            }

            Element::UnorderedList {
                depth,
                ref mut items,
            } => {
                if let Some(item_depth) = Self::unordered_list_item_depth(line) {
                    if *depth == item_depth {
                        // Add an item to the paragraph
                        items.push(Element::from_line(&line[line_depth as usize..], is_partial));
                        Ok(None)
                    } else if item_depth < *depth {
                        // Close the unordered list, start a new one
                        Ok(Some(Element::from_line(line, is_partial)))
                    } else {
                        // if inner_depth > line_depth
                        eprintln!("warning: got a nested list");
                        if let Some(el @ Element::UnorderedList { .. }) = items.last_mut() {
                            if let Some(new_el) = el.add_line(&line[*depth as usize..], is_partial)? {
                                items.push(new_el);
                            }
                        } else {
                            items.push(Element::from_line(&line[*depth as usize..], is_partial));
                        }
                        Ok(None)
                    }
                } else {
                    if *depth == line_depth {
                        // Continue the paragraph (or create a new one if blank line)

                        if can_start_paragraph {
                            // +1 to count in the marker ('*' / '-' / 'o') itself,
                            // ditto in the else branch
                            items.push(Element::from_line(
                                &line[*depth as usize..],
                                is_partial,
                            ))
                        } else {
                            let new_element = items
                                .last_mut()
                                .map(|el| el.add_line(&line[*depth as usize..], is_partial))
                                .unwrap()?;

                            if let Some(el) = new_element {
                                items.push(el);
                            }
                        }

                        Ok(None)
                    } else if *depth < line_depth {
                        if let Some(ref mut el @ Element::UnorderedList { .. }) = items.last_mut()
                            && el.depth() == line_depth
                        {
                            if let Some(new_el) = el.add_line(&line[*depth as usize..], is_partial)? {
                                items.push(new_el);
                            }

                            Ok(None)
                        } else {
                            eprintln!("warning: got a deep paragraph in list, merging with the shallow one");
                            dbg!(*depth, line_depth, line);

                            let new_element = items
                                .last_mut()
                                .map(|el| el.add_line(&line[line_depth as usize..], is_partial))
                                .unwrap()?;

                            if let Some(el) = new_element {
                                items.push(el);
                            }

                            Ok(None)
                        }
                    } else {
                        // if depth > line_depth
                        // Close the unordered list, start a new paragraph
                        Ok(Some(Element::from_line(line, is_partial)))
                    }
                }
            }

            Element::Code {
                depth,
                ref mut elements,
            } => {
                if !can_start_paragraph
                    || line_depth == *depth
                    || Self::is_likely_code(line, is_partial)
                {
                    if let Some(ParagraphElement::Text(ref mut text)) = elements.last_mut() {
                        text.push_str(line);
                        text.push('\n');
                    } else {
                        elements.push(ParagraphElement::Text(line.into()))
                    }

                    Ok(None)
                } else {
                    Ok(Some(Self::from_line(line, is_partial)))
                }
            } // _ => Ok(Some(Element::Paragraph {
              //     depth: line_depth,
              //     elements: vec![ParagraphElement::Text(line.trim_start().to_owned())],
              // })),
        }
    }

    /// Calculate at which column the list item starts
    fn unordered_list_item_depth(line: &str) -> Option<u32> {
        let trimmed_line = line.trim_start_matches(' ');

        if trimmed_line.starts_with("- ")
            || trimmed_line.starts_with("o ")
            || trimmed_line.starts_with("* ")
        {
            let actual_line = trimmed_line
                .strip_prefix(&['-', 'o', '*'])
                .unwrap()
                .trim_start();
            let line_depth = (line.len() - actual_line.len()) as u32;

            Some(line_depth)
        } else {
            None
        }
    }

    /// Tell the element that a blank line is encountered. The element is modified
    /// and a new element may be returned. The new element is inserted after the
    /// current element.
    fn encountered_blank_line(&mut self) -> Option<Self> {
        match self {
            // For Paragraph, we create a new empty Paragraph
            Element::Paragraph {
                depth,
                ref elements,
            } if !elements.is_empty() => Some(Element::Paragraph {
                depth: *depth,
                elements: Vec::new(),
            }),

            // For the Lists, we create a new empty Paragraph in the last item
            Element::OrderedList { ref mut items, .. }
            | Element::UnorderedList { ref mut items, .. } => {
                let last_item = items.last_mut().unwrap();

                if let Some(new_el) = last_item.encountered_blank_line() {
                    items.push(new_el);
                }

                None
            }

            // For Code, the newline is kept
            Element::Code {
                ref mut elements, ..
            } => {
                if let Some(ParagraphElement::Text(ref mut text)) = elements.last_mut() {
                    text.push('\n');
                } else {
                    elements.push(ParagraphElement::Text("\n".into()));
                }

                None
            }

            _ => None,
        }
    }

    fn has_encountered_blank_line(&self) -> bool {
        match self {
            Element::Paragraph { elements, .. } => elements.is_empty(),
            Element::OrderedList { items, .. } => {
                items.last().unwrap().has_encountered_blank_line()
            }
            Element::UnorderedList { items, .. } => {
                items.last().unwrap().has_encountered_blank_line()
            }
            Element::Code { elements, .. } => elements
                .last()
                .map(|el| {
                    matches!(
                    el,
                    ParagraphElement::Text(text)
                        if text.ends_with("\n\n")
                    )
                })
                .unwrap_or(false),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum OrderedListStyle {
    /// `a. Hello world!`
    DottedLetter,
    /// `1. Hello world!`
    DottedNumber,
    /// `(a) Hello world!`
    BracketedLetter,
    /// `(1) Hello world!`
    BracketedNumber,
    /// `1 Hello world!`
    UndottedNumber,
}

impl OrderedListStyle {
    /// From the line, extract the item number/letter and return the style used.
    ///
    /// Returns (Style, item number (number), item number (original), item)
    /// e.g. "(a) Some example here" returns `(BracketedLetter, 1, "a", "a.")`
    /// while "10. Some example here" returns `(DottedNumber, 10, "10", " Some example here")`
    fn extract_from_line(line: &str) -> Option<(Self, u32, &str, &str)> {
        let line = line.trim_start();

        static ORDERED_LIST_REGEX: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new(
                r#"(?x)   # this regex is whitespace-insenstive unless escaped
                ^(?<dot> [a-zA-Z0-9])\.\  # match for dotted lists: "1. Text" or "A. Text"
                |
                ^\((?<bracket> [a-zA-Z0-9])\)\  # match for bracketed lists: "(a) Text" or "(1) Text"
                |
                ^(?<none> [0-9]+?)\ \w # match for numbered lists without a dot: "1 Text"
            "#,
            )
            .unwrap()
        });

        let is_number = |s: &str| (b'0'..=b'9').contains(s.as_bytes().first().unwrap_or(&b' '));

        if let Some(captured) = ORDERED_LIST_REGEX.captures(line) {
            let extracted = if let Some(dot) = captured.name("dot")
                && !dot.is_empty()
            {
                let dot = dot.as_str();
                if is_number(dot) {
                    (
                        Self::DottedNumber,
                        dot.parse::<u32>().unwrap(),
                        dot,
                        &line[dot.len() + 1..],
                    )
                } else {
                    if dot.len() > 1 {
                        unimplemented!("proper number extraction for lettered lists")
                    }
                    (
                        Self::DottedLetter,
                        (dot.as_bytes()[0].to_ascii_lowercase() - b'a') as u32,
                        dot,
                        &line[dot.len() + 1..],
                    )
                }
            } else if let Some(bracket) = captured.name("bracket")
                && !bracket.is_empty()
            {
                let bracket = bracket.as_str();
                if is_number(bracket) {
                    (
                        Self::BracketedNumber,
                        bracket.parse::<u32>().unwrap(),
                        bracket,
                        &line[bracket.len() + 2..],
                    )
                } else {
                    (
                        Self::BracketedLetter,
                        (bracket.as_bytes()[0].to_ascii_lowercase() - b'a') as u32,
                        bracket,
                        &line[bracket.len() + 2..],
                    )
                }
            } else if let Some(none) = captured.name("none")
                && !none.is_empty()
            {
                let none = none.as_str();
                (
                    Self::UndottedNumber,
                    none.parse::<u32>().unwrap(),
                    none,
                    &line[none.len()..],
                )
            } else {
                unreachable!()
            };

            Some(extracted)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParagraphElement {
    /// Text MUST NOT contain newline characters. A newline will create a new Text.
    /// (Unless you are Code.)
    Text(String),
    /// See [Phase1Element::DocReference].
    DocReference(u32, Box<str>),
    /// See [Phase1Element::CrossReference].
    CrossReference((u32, Box<str>), Box<str>),
    /// See [Phase1Element::SelfReference].
    SelfReference(Box<str>, Box<str>),
    /// See [Phase1Element::ExtReference].
    ExtReference(Box<str>, Box<str>),
    /// See [Phase1Element::Anchor].
    Anchor(Box<str>, Option<Box<str>>),
}

impl ParagraphElement {
    fn add_line(&mut self, line: &str) -> Result<(), String> {
        match self {
            ParagraphElement::Text(ref mut text) => {
                let first_word_len = line
                    .trim_start()
                    .split(&[' ', ',', '.'])
                    .next()
                    .filter(|word| word.chars().all(|c| c.is_ascii_alphanumeric()))
                    .map(|word| word.len())
                    .unwrap_or(0);

                let has_hyphened_word = text.len() + first_word_len >= 72
                    && text.ends_with('-')
                    && (b'a'..=b'z').contains(&text.as_bytes()[text.as_bytes().len() - 2]);

                if !has_hyphened_word {
                    text.push(' ');
                    text.push_str(line.trim_start());
                } else {
                    text.push_str(line.trim_start());
                }

                Ok(())
            }

            _ => Err("unable to add text to non-text paragraph element".into()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Phase2Document {
    start_info: StartInfo,
    title: Box<str>,
    sections: Vec<Section>,
}

impl Phase2Document {
    pub fn from_phase1(document: Phase1Document) -> Result<Self, String> {
        let mut this = Self {
            start_info: Self::parse_start_info(document.meta_info(), document.elements())?,
            title: Box::<str>::default(),
            sections: Vec::new(),
        };

        if let Some(Phase1Element::H1(h1)) = document.elements().iter().find(|el| el.is_heading()) {
            this.title = h1.clone();
        }

        let document = Self::preprocess_phase1(document)?;

        eprintln!(
            "Parsed RFC {}, titled {:?}",
            this.start_info.rfc, this.title
        );

        this.sections = this.parse_sections(document.elements())?;

        Ok(this)
    }

    pub fn print(&self) -> String {
        let mut result = String::new();

        for section in &self.sections {
            result.push_str("========SECTION START========\n");
            result.push_str(&format!("id: {}\n", section.id.as_deref().unwrap_or("")));
            result.push_str(&format!("title: {}\n", section.title));
            result.push_str(&format!("level: {}\n", section.level + 1));
            result.push_str("-----------------------------\n");

            for element in &section.elements {
                match element {
                    Element::Paragraph {
                        depth,
                        elements: para_elements,
                    } => {
                        let depth = " ".repeat(*depth as usize);
                        let mut was_text = true;
                        for pel in para_elements {
                            let mut is_text = false;
                            match pel {
                                ParagraphElement::Text(text) => {
                                    if was_text {
                                        result.push_str(&depth);
                                    }
                                    result.push_str(text);
                                    is_text = true;
                                }
                                ParagraphElement::DocReference(rfc, title) => {
                                    result.push_str(&format!("[{}](^doc rfc{}^)", title, rfc))
                                }
                                ParagraphElement::CrossReference((rfc, sect), title) => result
                                    .push_str(&format!("[{}](^cross rfc{}#{}^)", title, rfc, sect)),
                                ParagraphElement::SelfReference(sect, title) => {
                                    result.push_str(&format!("[{}](^self #{}^)", title, sect))
                                }
                                ParagraphElement::ExtReference(href, t) => {
                                    result.push_str(&format!("[{}]({})", href, t))
                                }
                                ParagraphElement::Anchor(id, Some(text)) => {
                                    result.push_str(&format!("${}, {}$", id, text))
                                }
                                ParagraphElement::Anchor(id, None) => {
                                    result.push_str(&format!("${}$", id))
                                }
                            }
                            was_text = is_text;
                        }
                        result.push('\n');
                    }
                    _ => todo!(),
                }
            }

            result.push_str("=========SECTION END=========\n\n");
        }

        result
    }

    // The preprocessing here removes page boundaries (<span id="page-N">) and excessive
    // newlines created by those boundaries.
    fn preprocess_phase1(document: Phase1Document) -> Result<Phase1Document, String> {
        let (meta_info, mut elements) = document.to_raw_parts();

        // Remove all elements before <h1> and also itself
        if let Some(pos) = elements.iter().position(|el| el.is_heading()) {
            elements = elements.split_off(pos + 1);
        } else {
            return Err("couldn't find the document title during preprocessing".into());
        }

        for i in 1..elements.len() {
            let is_page_boundary = matches!(
                &elements[i],
                Phase1Element::Anchor(id, text) if id.starts_with("page") && text.is_none()
            );

            if is_page_boundary {
                if let Phase1Element::Text(ref mut text) = elements[i - 1] {
                    let mut text_owned = String::from(std::mem::take(text));

                    loop {
                        if text_owned.ends_with('\n') {
                            text_owned.pop().unwrap();
                        } else {
                            break;
                        }
                    }

                    // All page boundaries will be removed later, let's add a newline to
                    // compensate for its removal
                    text_owned.push('\n');

                    let text_owned = text_owned.into_boxed_str();
                    *text = text_owned;
                }

                if let Some(Phase1Element::Text(ref mut text)) = elements.get_mut(i + 1) {
                    // We know a page will always start at the same position, so there is no need
                    // for unlimited trimming like we do for elements[i-1] (where a page break
                    // can result in multiple newlines). Limit to 3 newlines, to take into account
                    // the excessive newlines caused by removal of <span class="grey"> in Phase 1
                    let cut_off = text.chars().take_while(|c| *c == '\n').count().min(3);

                    *text = text[cut_off..].to_string().into_boxed_str();
                } else {
                    eprintln!("{:?}", elements.get(i + 1))
                }
            }
        }

        elements.retain(|el| {
            !matches!(
                el,
                Phase1Element::Anchor(id, text) if id.starts_with("page") && text.is_none()
            )
        });

        Ok(Phase1Document::from_raw_parts(meta_info, elements))
    }

    fn parse_start_info(
        metas: &[MetaElement],
        p1_elements: &[Phase1Element],
    ) -> Result<StartInfo, String> {
        let heading = p1_elements
            .iter()
            .position(|el| el.is_heading())
            .ok_or_else(|| String::from("couldn't find heading in RFC document"))?;
        let p1_elements = &p1_elements[0..heading];

        let mut this = StartInfo::default();

        let mut parsing_obselete = false;
        let mut left_column = Vec::new();
        let mut right_column = Vec::new();

        // All RFC documents begins like so:
        // `Some Research Organization (SRO)                          J. Fabulousity`
        // This constant represents the minimum amount of space between the left
        // and right side. 10 is arbitrarily chosen.
        const SPACE_THRESHOLD: usize = 10;

        for p1_element in p1_elements {
            match p1_element {
                Phase1Element::Text(text) => {
                    let spaces = " ".repeat(SPACE_THRESHOLD);

                    for line in text.lines() {
                        // Some lines towards the end are completely whitespace, just skip them...
                        if line.trim_ascii().is_empty() {
                            continue;
                        }

                        let split_at = line.find(&spaces);

                        if let Some(at) = split_at {
                            let (left, right) = line.split_at(at);
                            let right = right[SPACE_THRESHOLD..].trim_start_matches(' ');

                            if !left.is_empty() {
                                left_column
                                    .push(Phase1Element::Text(left.to_owned().into_boxed_str()));
                            }

                            if !right.is_empty() {
                                right_column.push(right);
                            }
                        } else {
                            left_column.push(Phase1Element::Text(line.to_owned().into_boxed_str()))
                        }
                    }
                }
                doc @ Phase1Element::DocReference(_, _) => {
                    left_column.push(doc.clone());
                }
                _ => return Err("invalid start info - found unexpected tags".into()),
            }
        }

        // This regex attempts to find:
        // - One or more word
        // - (optionally) an abbreviation consisting of uppercase letters, enclosed by "()"
        // - SPACE_THRESHOLD amount of spaces following the above
        let stream_regex = Regex::new(r"^((?:\w+ )+(?:\w+|\([A-Z]+\)))").unwrap();

        // RFC streams, some kinda namespace
        if let Some(Phase1Element::Text(text)) = left_column.get(0)
            && let Some(captured) = stream_regex.captures(text)
        {
            this.stream = captured[1].to_owned().into_boxed_str();
            text.strip_prefix(&captured[0]).unwrap()
        } else {
            return Err("suspected invalid start info - invalid stream?".into());
        };

        for p1_element in &left_column[1..] {
            match p1_element {
                Phase1Element::Text(text) if text.starts_with("Request for Comments:") => {
                    parsing_obselete = false;

                    let regex = Regex::new(r"^Request for Comments:[ ]+(\d+)").unwrap();

                    if let Some(captured) = regex.captures(text) {
                        let rfc = captured[1].parse::<u32>().unwrap();
                        this.rfc = rfc;
                    } else {
                        return Err("invalid start info - in Request for Comments line".into());
                    };
                }

                Phase1Element::Text(text) if text.starts_with("Obsoletes:") => {
                    parsing_obselete = true;
                }

                Phase1Element::Text(text) if text.starts_with("Category:") => {
                    parsing_obselete = false;
                    this.category = text
                        .strip_prefix("Category:")
                        .unwrap()
                        .trim()
                        .to_owned()
                        .into_boxed_str();
                }

                Phase1Element::Text(text) if text.contains(':') && !parsing_obselete => {
                    parsing_obselete = false;
                    let mut split = text.split(':');
                    let first = split.next().unwrap().trim_start();
                    let second = split.next().unwrap().trim();

                    this.others.insert(first.to_string(), second.to_string());
                }

                Phase1Element::Text(text) if text.starts_with(",") && parsing_obselete => continue,

                Phase1Element::DocReference(doc, title) if parsing_obselete => {
                    this.obsoletes.push((*doc, title.clone()))
                }

                Phase1Element::DocReference(_, _) if !parsing_obselete => {
                    return Err("unexpected RFC doc reference in start info".into())
                }

                _ => return Err("unexpected element in start info".into()),
            }
        }

        // Regex matching "MONTH_NAME, YEAR_IN_YYYY"
        let date_regex = Regex::new(
            r"(January|February|March|April|May|June|July|August|September|October|November|December),? \d{4}$"
        ).unwrap();
        for &right in &right_column {
            if date_regex.is_match(right) {
                if !this.date.is_empty() {
                    return Err("apparent duplicated date in start info".into());
                }
                this.date = right.to_owned().into_boxed_str();
            } else if metas
                .iter()
                .any(|meta| matches!(meta, MetaElement::Author(a) if right.contains(&**a)))
            {
                this.authors
                    .push((right.to_owned().into_boxed_str(), Default::default()));
            } else {
                for author in this.authors.iter_mut().rev() {
                    if author.1.is_empty() {
                        author.1 = right.to_owned().into_boxed_str();
                    } else {
                        break;
                    }
                }
            }
        }

        if this.date.is_empty() {
            return Err("missed date in start info".into());
        }

        println!("{:#?}", this);
        Ok(this)
    }

    fn parse_sections(&mut self, p1_elements: &[Phase1Element]) -> Result<Vec<Section>, String> {
        let mut sections = Vec::new();

        let mut current_section = Section {
            id: None,
            level: 0,
            title: Box::<str>::default(),
            elements: Vec::new(),
        };

        // A partial line - the line wasn't processed from beginning to the end (newline)
        // let mut line = Vec::new();
        // let mut is_partial = false;

        // for p1_element in p1_elements {
        //     match p1_element {
        //         el @ Phase1Element::Text(text) => {
        //             if text.contains('\n') {
        //                 // is_partial = text.ends_with('\n');
        //                 let mut lines = text.lines().peekable();

        //                 while let Some(text_line) = lines.next() {
        //                     let is_partial =
        //                         dbg!(lines.peek().is_none()) && dbg!(!text.ends_with('\n'));

        //                     if !is_partial {
        //                         line.push(Phase1Element::Text(
        //                             text_line.to_owned().into_boxed_str(),
        //                         ));
        //                         println!("{:?}", line);
        //                         line.clear();
        //                         println!();
        //                     } else {
        //                         line.push(Phase1Element::Text(
        //                             text_line.to_owned().into_boxed_str(),
        //                         ));
        //                     }
        //                 }
        //             } else {
        //                 line.push(el.clone());
        //             }
        //         }

        //         x => line.push(x.clone()),
        //     }
        // }

        for p1_element in p1_elements {
            match p1_element {
                Phase1Element::Text(text) => {
                    let mut lines = text.lines().peekable();

                    while let Some(line) = lines.next() {
                        let is_partial = lines.peek().is_none() && !text.ends_with('\n');

                        if line.is_empty() {
                            if let Some(element) = current_section.elements.last_mut()
                                && let Some(new_element) = element.encountered_blank_line()
                            {
                                current_section.elements.push(new_element);
                            }

                            continue;
                        }

                        let can_start_new_para = current_section.can_start_new_paragraph();
                        if !line.starts_with(' ') && can_start_new_para {
                            if !(current_section.title.is_empty()
                                && current_section.elements.len() == 0)
                            {
                                // Flush section
                                sections.push(std::mem::take(&mut current_section));
                            }
                            current_section.title = line.trim_end().to_owned().into_boxed_str();
                        } else {
                            if let Some(element) = current_section.elements.last_mut() {
                                if let Some(new_element) = element.add_line(line, is_partial)? {
                                    current_section.elements.push(new_element);
                                };
                            } else {
                                current_section
                                    .elements
                                    .push(Element::from_line(line, is_partial))
                            }
                        }
                    }
                }

                Phase1Element::H1(_) => {
                    return Err("unexpected <h1> in the middle of document".into())
                }

                Phase1Element::H2(title, id)
                | Phase1Element::H3(title, id)
                | Phase1Element::H4(title, id)
                | Phase1Element::H5(title, id)
                | Phase1Element::H6(title, id) => {
                    if !(current_section.title.is_empty() && current_section.elements.is_empty()) {
                        // Flush section
                        sections.push(std::mem::take(&mut current_section));
                    }

                    current_section.id = Some(id.clone());
                    current_section.title = title.clone();
                    current_section.level = id.chars().filter(|c| *c == '.').count();
                }

                Phase1Element::Anchor(id, text) => current_section
                    .add_element(ParagraphElement::Anchor(id.clone(), text.clone()))?,

                Phase1Element::CrossReference(href, title) => current_section.add_element(
                    ParagraphElement::CrossReference(href.clone(), title.clone()),
                )?,
                Phase1Element::DocReference(href, title) => current_section
                    .add_element(ParagraphElement::DocReference(href.clone(), title.clone()))?,
                Phase1Element::SelfReference(href, title) => current_section
                    .add_element(ParagraphElement::SelfReference(href.clone(), title.clone()))?,
                Phase1Element::ExtReference(href, title) => current_section
                    .add_element(ParagraphElement::ExtReference(href.clone(), title.clone()))?,
            }
        }

        Ok(sections)
    }
}

// figuring out whether next section of the text is part of current paragraph
// - if len(first word in next sect) + len(last line) + 1 > 74
// - if !ends_with(last line, '.') and first_word(next sect) is in lowercase
//
//
// figuring out whether the following section is graphical:
// - if extensive amount of ASCII graphical character (-+/_:*) is involved
// - if the whitespace in between characters is excessive (>5 between chars)
// - if there is a stark difference in indentation
// - in the case of packet format, look for:
//   ```
//    0                   1                   2                   3
//    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
//   ```
//
//
// figuring out whether the following section is a table:
// - it is established that the section is graphical (see above)
// - consistent lining of text (same column)
//
