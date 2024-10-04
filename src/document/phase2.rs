//! In this phase, analysis of text content begin. The sections are identified,
//! the textual contents are determined to whether be paragraphs, lists (ordered
//! or unordered), or preformatted sections. Phase 2 will produce a tree that
//! Phase 3 consumes.

use std::collections::BTreeMap;
use std::sync::LazyLock;

use regex::{Regex, RegexSet};

use super::phase1::{Document as Phase1Document, Element as Phase1Element, Line, MetaElement};

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
    pub fn into_raw_parts(self) -> (Option<Box<str>>, usize, Box<str>, Vec<Element>) {
        (self.id, self.level, self.title, self.elements)
    }

    fn add_line(&mut self, line: Line) -> Result<Option<Self>, String> {
        match line.as_slice() {
            [Phase1Element::H2(title, id)
            | Phase1Element::H3(title, id)
            | Phase1Element::H4(title, id)
            | Phase1Element::H5(title, id)
            | Phase1Element::H6(title, id), Phase1Element::Text { text, ending: true }] => {
                if !text.is_empty() {
                    return Err("<h2>..<h6> is followed by text in the same line".into());
                }

                if self.title.is_empty() {
                    self.title = title.clone();
                    Ok(None)
                } else {
                    Ok(Some(Section {
                        id: Some(id.clone()),
                        level: title.chars().filter(|c| *c == '.').count(),
                        title: title.clone(),
                        elements: Vec::new(),
                    }))
                }
            }

            [Phase1Element::Line(line)] if line.is_empty() => {
                if let Some(el) = self.elements.last_mut() {
                    if let Some(new_el) = el.encountered_blank_line() {
                        self.elements.push(new_el);
                    }
                }

                Ok(None)
            }

            [Phase1Element::Line(line)] if !line.starts_with(' ') => {
                if self.title.is_empty() {
                    self.title = line.clone();
                    Ok(None)
                } else {
                    Ok(Some(Section {
                        id: None,
                        level: 0,
                        title: line.clone(),
                        elements: Vec::new(),
                    }))
                }
            }

            [Phase1Element::Line(_)]
            | [Phase1Element::Text {
                text: _,
                ending: false,
            }, ..] => {
                if let Some(el) = self.elements.last_mut() {
                    if let Some(new_el) = el.add_line(line, 0)? {
                        self.elements.push(new_el);
                    }
                } else {
                    self.elements.push(Element::from_phase1_line(line, 0));
                }

                Ok(None)
            }

            [Phase1Element::Text {
                text: _,
                ending: true,
            }] => panic!("encountered Text that covers a whole line but was not Line"),

            xs @ [_, ..] if xs.iter().any(|x| x.is_heading()) => panic!("encountered <h1>"),

            xs @ [_, ..] => panic!(
                "encountered lines not starting with text or heading: {:?}",
                xs
            ),

            [] => panic!("encountered a completely empty line"),
        }
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
    /// List with a starting number. Each list item may hold multiple paragraphs,
    /// hence they are made a Vec of Element.
    ///
    /// Depth is defined as number of spaces before the item marker, so
    /// `1. An item` will have a depth of 0.
    ///
    /// Each item carries a 1-indexed numbering. If None, that item follows the
    /// previous item's numbering.
    OrderedList {
        depth: u32,
        style: OrderedListStyle,
        items: Vec<(Option<u32>, Self)>,
    },
    /// List without any numbering, it may start with a `-`, `o`, or '*'. Each list
    /// item may hold multiple paragraphs, hence they are made a Vec of Element.
    ///
    /// Depth is defined as number of characters before the item marker, so
    /// `- An item` will have a depth of 0.
    ///
    /// Each item carries a bool to indicate whether they have a mark.
    UnorderedList {
        depth: u32,
        style: UnorderedListStyle,
        items: Vec<(bool, Self)>,
    },
    /// A preformatted block of text. The newlines are all preserved in Code, but
    /// the common indentation (`min(number_of_space(line) for line in lines)`) is not.
    Preformatted {
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
            Element::Preformatted { depth, .. } => *depth,
        }
    }

    fn from_phase1_line(line: Line, ignore: usize) -> Self {
        match line.as_slice() {
            [Phase1Element::Line(start), xs @ ..]
            | [Phase1Element::Text {
                text: start,
                ending: false,
            }, xs @ ..] => {
                let start = &start[ignore..];
                let trimmed_start = start.trim_start_matches(' ');
                let start_depth = (start.len() - trimmed_start.len()) as u32;

                if let [Phase1Element::Line(_)] = line.as_slice() {
                    debug_assert!(xs.is_empty());
                }

                assert!(start_depth > 0);

                if let Some((style, depth)) = UnorderedListStyle::extract_from_line(start) {
                    let mut elements = Vec::new();
                    elements.push(ParagraphElement::Text(
                        start[depth as usize..].trim_start().to_owned(),
                    ));
                    Self::paragraph_from_line(&mut elements, xs, false);

                    Self::UnorderedList {
                        depth: start_depth,
                        style,
                        items: vec![(
                            true,
                            Element::Paragraph {
                                depth: depth - (start_depth + 1),
                                elements,
                            },
                        )],
                    }
                } else if let Some((style, starting, _, content)) =
                    OrderedListStyle::extract_from_line(trimmed_start)
                {
                    let mut elements = Vec::new();
                    elements.push(ParagraphElement::Text(content.trim_start().to_owned()));
                    Self::paragraph_from_line(&mut elements, xs, false);

                    Self::OrderedList {
                        depth: start_depth,
                        style,
                        items: vec![(
                            Some(starting),
                            Element::Paragraph {
                                depth: content.chars().take_while(|c| *c == ' ').count() as u32,
                                elements,
                            },
                        )],
                    }
                } else if Self::is_likely_preformatted(&line.make_string()) {
                    let mut elements = Vec::new();
                    Self::paragraph_from_line(&mut elements, line.as_slice(), true);
                    Self::Preformatted { depth: 0, elements }
                } else {
                    let mut elements = Vec::new();
                    elements.push(ParagraphElement::Text(trimmed_start.to_owned()));
                    Self::paragraph_from_line(&mut elements, xs, false);

                    Self::Paragraph {
                        depth: start_depth,
                        elements,
                    }
                }
            }

            [_, ..] => panic!("encountered lines not starting with text"),

            [] => panic!("encountered a completely empty line"),
        }
    }

    fn paragraph_from_line(
        elements: &mut Vec<ParagraphElement>,
        line: &[Phase1Element],
        keep_newline: bool,
    ) {
        for p1_element in line {
            match p1_element {
                Phase1Element::Text { text, ending } => {
                    let mut line = String::from(text.to_owned());
                    if *ending && keep_newline {
                        line.push('\n');
                    }
                    elements.push(ParagraphElement::Text(line))
                }
                Phase1Element::Line(line) => {
                    let mut line = String::from(line.to_owned());
                    if keep_newline {
                        line.push('\n');
                    }
                    elements.push(ParagraphElement::Text(line))
                }
                Phase1Element::H1(_)
                | Phase1Element::H2(_, _)
                | Phase1Element::H3(_, _)
                | Phase1Element::H4(_, _)
                | Phase1Element::H5(_, _)
                | Phase1Element::H6(_, _) => {
                    panic!("headings shouldn't be found inside a paragraph")
                }
                Phase1Element::Anchor(id, title) => {
                    elements.push(ParagraphElement::Anchor(id.clone(), title.clone()))
                }
                Phase1Element::DocReference(loc, title) => {
                    elements.push(ParagraphElement::DocReference(*loc, title.clone()))
                }
                Phase1Element::CrossReference(loc, title) => {
                    elements.push(ParagraphElement::CrossReference(loc.clone(), title.clone()))
                }
                Phase1Element::SelfReference(loc, title) => {
                    elements.push(ParagraphElement::SelfReference(loc.clone(), title.clone()))
                }
                Phase1Element::ExtReference(loc, title) => {
                    elements.push(ParagraphElement::ExtReference(loc.clone(), title.clone()))
                }
            }
        }
    }

    fn is_likely_preformatted(line: &str) -> bool {
        const GRAPHICAL_CANDIDATES: [u8; 8] = [b'-', b'+', b'/', b'_', b':', b'*', b'\\', b'|'];
        const CODE_SCORE_THRESHOLD: usize = 700;

        // Code blocks often have a caption towards the end.
        // Example: "Figure 1. This code does something"
        static FIGCAPTION_REGEX: LazyLock<Regex> = LazyLock::new(|| {
            // Ugly regex. A few things here:
            // 1. Match 3 or more spaces. Capture it for use later.
            // 2. Match "Figure N." or "Figure N:" or "Figure N".
            // 3. Match the word after step 2.
            // 4. Match everything else until the end.

            // Step 1 assumes the caption is center-aligned.
            Regex::new(r"^([ ]{3,})Figure \d+[:.]? \w+.*$").unwrap()
        });

        // Match for something that vaguely resembles a line of C.
        static C_CODE_REGEX: LazyLock<RegexSet> = LazyLock::new(|| {
            RegexSet::new([
                r"^[ ]+[a-z_][a-z0-9_]*\(.+\);",                // function call
                r"^[ ]+while ?\(1\)",                           // while(1) loop
                r"^[ ]+if ?\(.+\)(?: \{)?$",                    // if statement
                r"[a-z_][a-z0-9_]* [+\-*/]=? [a-z_][a-z0-9_]*", // binary expression
            ])
            .unwrap()
        });

        // Look for conjunction words and make sure we don't accidentally consider
        // a sentence like "Foo is achieved when A is found, AND\n" to
        // be a non-sentence. But make sure there are a bunch of words preceding them.
        static CONJUCTION_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"(?:\w+? ){3,}\w+?,? (and|or|AND|OR)$").unwrap());

        let trimmed_line = line.trim_start_matches(' ');
        let line_depth = line.len() - trimmed_line.len();

        // A normal paragraph starts with 3 spaces.
        let mut excessive_spaces = line_depth.saturating_sub(3);
        let mut ending_spaces = 0;
        let mut graphical_chars = 0;
        let nonalphabet_count = trimmed_line
            .bytes()
            .map(|b| !(b.is_ascii_alphabetic() || b > 0x7F) as usize)
            .sum::<usize>();

        // See if a line is properly "ended", i.e it forms a sentence, maybe.
        let mut properly_ended = trimmed_line
            .rfind(['.', ':', ',', ']'])
            .map(|idx| !trimmed_line[idx + 1..].bytes().any(|b| b != b' '))
            .unwrap_or(false);

        // Also look for logical conjunctions.
        if !properly_ended {
            properly_ended |= CONJUCTION_REGEX.is_match(trimmed_line);
        }

        // If the line is STILL not properly ended, it is likely to be part of a code block
        if !properly_ended {
            ending_spaces += 72usize.saturating_sub(line.len());
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
        } else if let Some(captured) = FIGCAPTION_REGEX.captures(line) {
            let left_spaces = captured[1].len();
            let whole_line = (left_spaces * 2 + trimmed_line.len()) as i32;

            // We want to know whether the matched figcaption is center aligned.
            // Because if not, it could very well just be a normal paragraph.
            if (whole_line - 72).abs() <= 2 {
                graphical_chars += 100;
            }
        } else if C_CODE_REGEX.is_match(line) {
            graphical_chars += 100;
        }

        // TODO: This thing is completely unscientific. Do some statistical analysis in the
        // future
        let score = nonalphabet_count * 10
            + ending_spaces * 20
            + excessive_spaces * 50
            + graphical_chars * 70;

        score > CODE_SCORE_THRESHOLD
    }

    /// Add a new line to the [Element]. If the addition succeeds, the current [Element]
    /// is modified in place and a new [Element] may be returned. The new element needs
    /// to be inserted after the current one.
    fn add_line(&mut self, full_line: Line, ignore: usize) -> Result<Option<Self>, String> {
        match full_line.as_slice() {
            [Phase1Element::Line(line), xs @ ..]
            | [Phase1Element::Text {
                text: line,
                ending: false,
            }, xs @ ..] => {
                let line = &line[ignore..];
                let trimmed_line = line.trim_start();
                let line_depth = (line.len() - trimmed_line.len()) as u32;

                match self {
                    Element::Paragraph { elements, .. } if elements.is_empty() => {
                        let parsed_line = Self::from_phase1_line(full_line, ignore);

                        match parsed_line {
                            Element::Paragraph {
                                elements: new_elements,
                                ..
                            } => {
                                *elements = new_elements;
                                Ok(None)
                            }

                            x => Ok(Some(x)),
                        }
                    }

                    Element::Paragraph { elements, .. } => {
                        if let Some(text @ ParagraphElement::Text(_)) = elements.last_mut() {
                            text.add_line(line)?;
                            Self::paragraph_from_line(elements, xs, false);
                            Ok(None)
                        } else {
                            Self::paragraph_from_line(elements, full_line.as_slice(), false);
                            Ok(None)
                        }
                    }

                    Element::OrderedList {
                        depth,
                        style,
                        items,
                    } => {
                        if let Some((item_style, num, marker, content)) =
                            OrderedListStyle::extract_from_line(line)
                            && *style == item_style
                        {
                            // Allow some tolerance for cmp(depth, item_depth)...
                            let depth_difference = line_depth as i32 - *depth as i32;

                            let content_depth =
                                content.chars().take_while(|c| *c == ' ').count() as u32;
                            let content = content.trim_start();
                            let content_start =
                                *depth + marker.len() as u32 + item_style.occupied_space();

                            if depth_difference.abs() <= 1 {
                                // depth == line_depth

                                let mut elements = vec![ParagraphElement::Text(content.into())];
                                Self::paragraph_from_line(&mut elements, xs, false);

                                items.push((
                                    Some(num),
                                    Element::Paragraph {
                                        depth: content_depth,
                                        elements,
                                    },
                                ));

                                Ok(None)
                            } else if depth_difference < -1 {
                                // line_depth < depth
                                Ok(Some(Self::from_phase1_line(full_line, ignore)))
                            } else {
                                // line_depth > depth
                                let el = &mut items.last_mut().unwrap().1;
                                if let Some(new_el) =
                                    el.add_line(full_line, ignore + content_start as usize)?
                                {
                                    items.push((None, new_el));
                                }

                                Ok(None)
                            }
                        } else {
                            let inner_depth = items.last().map(|item| item.1.depth()).unwrap();

                            // Assumption: marker numbering takes one char
                            let content_start = *depth + style.occupied_space() + 1;
                            let depth_difference =
                                line_depth as i32 - (content_start + inner_depth) as i32;

                            if depth_difference.abs() <= 1 {
                                // (content_start + inner_depth) == line_depth
                                let el = &mut items.last_mut().unwrap().1;
                                if let Some(new_el) =
                                    el.add_line(full_line, ignore + content_start as usize)?
                                {
                                    items.push((None, new_el));
                                }

                                Ok(None)
                            } else if depth_difference < -1 {
                                // line_depth < (content_start + inner_depth)
                                Ok(Some(Self::from_phase1_line(full_line, ignore)))
                            } else {
                                // line_depth > (content_start + inner_depth)
                                let el = &mut items.last_mut().unwrap().1;
                                if let Some(new_el) =
                                    el.add_line(full_line, ignore + content_start as usize)?
                                {
                                    items.push((None, new_el));
                                }

                                Ok(None)
                            }
                        }
                    }

                    Element::UnorderedList {
                        depth,
                        style,
                        items,
                    } => {
                        if let Some((item_style, item_depth)) =
                            UnorderedListStyle::extract_from_line(line)
                            && *style == item_style
                        {
                            let content_start = *depth + 1;
                            let content = &line[item_depth as usize..];

                            if *depth == line_depth {
                                let mut elements =
                                    vec![ParagraphElement::Text(content.trim_start().into())];
                                Self::paragraph_from_line(&mut elements, xs, false);

                                items.push((
                                    true,
                                    Element::Paragraph {
                                        depth: item_depth - content_start,
                                        elements,
                                    },
                                ));

                                Ok(None)
                            } else if line_depth < *depth {
                                Ok(Some(Self::from_phase1_line(full_line, ignore)))
                            } else {
                                // line_depth > depth
                                let el = &mut items.last_mut().unwrap().1;
                                if let Some(new_el) =
                                    el.add_line(full_line, ignore + content_start as usize)?
                                {
                                    items.push((false, new_el));
                                }

                                Ok(None)
                            }
                        } else {
                            let inner_depth = items.last().map(|item| item.1.depth()).unwrap();
                            let content_start = *depth + 1;

                            if content_start + inner_depth == line_depth {
                                let el = &mut items.last_mut().unwrap().1;
                                if let Some(new_el) =
                                    el.add_line(full_line, ignore + content_start as usize)?
                                {
                                    items.push((false, new_el));
                                }

                                Ok(None)
                            } else if line_depth < content_start + inner_depth {
                                Ok(Some(Self::from_phase1_line(full_line, ignore)))
                            } else {
                                // line_depth > content_start + inner_depth
                                let el = &mut items.last_mut().unwrap().1;
                                if let Some(new_el) =
                                    el.add_line(full_line, ignore + content_start as usize)?
                                {
                                    items.push((false, new_el));
                                }

                                Ok(None)
                            }
                        }
                    }

                    Element::Preformatted { depth: _, elements } => {
                        if let Some(ParagraphElement::Text(text)) = elements.last()
                            && text.ends_with("\n\n")
                        {
                            let is_code =
                                Self::is_likely_preformatted(&full_line.make_string());

                            if is_code {
                                Self::paragraph_from_line(elements, full_line.as_slice(), true);
                                Ok(None)
                            } else {
                                let parsed = Self::from_phase1_line(full_line, ignore);
                                Ok(Some(parsed))
                            }
                        } else {
                            Self::paragraph_from_line(elements, full_line.as_slice(), true);
                            Ok(None)
                        }
                    }
                }
            }

            [_, ..] => panic!("encountered lines not starting with text"),

            [] => panic!("encountered a completely empty line"),
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
            Element::OrderedList { ref mut items, .. } => {
                let last_item = &mut items.last_mut().unwrap().1;

                if let Some(new_el) = last_item.encountered_blank_line() {
                    items.push((None, new_el));
                }

                None
            }

            Element::UnorderedList { ref mut items, .. } => {
                let last_item = &mut items.last_mut().unwrap().1;

                if let Some(new_el) = last_item.encountered_blank_line() {
                    items.push((false, new_el));
                }

                None
            }

            // For preformatted sections, the newline is kept
            Element::Preformatted {
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OrderedListStyle {
    /// `a. Hello world!`
    DottedLetterLower,
    /// `A. Hello world!`
    DottedLetterUpper,
    /// `1. Hello world!`
    DottedNumber,
    /// `(a) Hello world!`
    BracketedLetterLower,
    /// `(A) Hello world!`
    BracketedLetterUpper,
    /// `(1) Hello world!`
    BracketedNumber,
    /// `(i) Hello world!`
    BracketedRoman,
    /// `1 Hello world!`
    UndottedNumber,
}

impl OrderedListStyle {
    /// How many spaces does the marker (not including the numbering/lettering)
    /// require
    fn occupied_space(&self) -> u32 {
        match self {
            OrderedListStyle::DottedLetterUpper => 1,
            OrderedListStyle::DottedLetterLower => 1,
            OrderedListStyle::DottedNumber => 1,
            OrderedListStyle::BracketedLetterUpper => 2,
            OrderedListStyle::BracketedLetterLower => 2,
            OrderedListStyle::BracketedNumber => 2,
            OrderedListStyle::BracketedRoman => 2,
            OrderedListStyle::UndottedNumber => 0,
        }
    }

    /// From the line, extract the item number/letter and return the style used.
    ///
    /// Returns (Style, item number (number), item number (original), item)
    /// e.g. "(a) Some example here" returns `(BracketedLetter, 1, "a", " Some example here")`
    /// while "10. Some example here" returns `(DottedNumber, 10, "10", " Some example here")`
    fn extract_from_line(line: &str) -> Option<(Self, u32, &str, &str)> {
        let line = line.trim_start();

        static ORDERED_LIST_REGEX: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new(
                r#"(?x)   # this regex is whitespace-insenstive unless escaped
                ^(?<dotLetterUpper> [A-Z])\.\  # match for dotted lists: "A. Text"
                |
                ^(?<dotLetterLower> [a-z])\.\  # match for dotted lists: "a. Text"
                |
                ^(?<dotNumber> [0-9]+)\.\    # match for dotted lists: "1. Text"
                |
                ^\((?<bracketRoman> i|ii|iii|iv|v|vi|vii|viii|ix|x)\)\  # match for bracketed lists: "(ii) Text"
                |
                ^\((?<bracketLetterUpper> [A-Z])\)\  # match for bracketed lists: "(A) Text"
                |
                ^\((?<bracketLetterLower> [a-z])\)\  # match for bracketed lists: "(a) Text"
                |
                ^\((?<bracketNumber> [0-9])\)\  # match for bracketed lists: "(1) Text"
                |
                ^(?<none> [0-9]+?)\ {1,3}\w # match for numbered lists without a dot: "1 Text"
            "#,
            )
            .unwrap()
        });

        if let Some(captured) = ORDERED_LIST_REGEX.captures(line) {
            let extracted = if let Some(dot) = captured.name("dotLetterUpper")
                && !dot.is_empty()
            {
                let dot = dot.as_str();
                (
                    Self::DottedLetterUpper,
                    (dot.as_bytes()[0] - b'A' + 1) as u32,
                    dot,
                    &line[dot.len() + 1..],
                )
            } else if let Some(dot) = captured.name("dotLetterLower") {
                let dot = dot.as_str();
                (
                    Self::DottedLetterLower,
                    (dot.as_bytes()[0] - b'a' + 1) as u32,
                    dot,
                    &line[dot.len() + 1..],
                )
            } else if let Some(dot) = captured.name("dotNumber") {
                let dot = dot.as_str();
                (
                    Self::DottedNumber,
                    dot.parse::<u32>().unwrap(),
                    dot,
                    &line[dot.len() + 1..],
                )
            } else if let Some(bracket) = captured.name("bracketLetterUpper")
                && !bracket.is_empty()
            {
                let bracket = bracket.as_str();
                (
                    Self::BracketedLetterUpper,
                    (bracket.as_bytes()[0] - b'A' + 1) as u32,
                    bracket,
                    &line[bracket.len() + 2..],
                )
            } else if let Some(bracket) = captured.name("bracketLetterLower")
                && !bracket.is_empty()
            {
                let bracket = bracket.as_str();
                (
                    Self::BracketedLetterLower,
                    (bracket.as_bytes()[0] - b'a' + 1) as u32,
                    bracket,
                    &line[bracket.len() + 2..],
                )
            } else if let Some(bracket) = captured.name("bracketNumber")
                && !bracket.is_empty()
            {
                let bracket = bracket.as_str();
                (
                    Self::BracketedNumber,
                    bracket.parse::<u32>().unwrap(),
                    bracket,
                    &line[bracket.len() + 2..],
                )
            } else if let Some(bracket) = captured.name("bracketRoman") {
                let bracket = bracket.as_str();
                let num = match bracket {
                    "i" => 1,
                    "ii" => 2,
                    "iii" => 3,
                    "iv" => 4,
                    "v" => 5,
                    "vi" => 6,
                    "vii" => 7,
                    "viii" => 8,
                    "ix" => 9,
                    "x" => 10,
                    _ => unreachable!(), // Do we need more?
                };
                (
                    Self::BracketedRoman,
                    num,
                    bracket,
                    &line[bracket.len() + 2..],
                )
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnorderedListStyle {
    /// `* Hello world!`
    Asterisk,
    /// `- Hello world!`
    Dash,
    /// `o Hello world!`
    Round,
    /// `+ Hello wrold!`
    Plus,
}

impl UnorderedListStyle {
    /// Find the style and calculate at which column the list item starts
    fn extract_from_line(line: &str) -> Option<(Self, u32)> {
        let trimmed_line = line.trim_start_matches(' ');

        if trimmed_line.starts_with("- ")
            || trimmed_line.starts_with("o ")
            || trimmed_line.starts_with("* ")
            || trimmed_line.starts_with("+ ")
        {
            let (style, actual_line) = if let Some(line) = trimmed_line.strip_prefix('*') {
                (Self::Asterisk, line)
            } else if let Some(line) = trimmed_line.strip_prefix('-') {
                (Self::Dash, line)
            } else if let Some(line) = trimmed_line.strip_prefix('o') {
                (Self::Round, line)
            } else if let Some(line) = trimmed_line.strip_prefix('+') {
                (Self::Plus, line)
            } else {
                unreachable!()
            };

            let actual_line = actual_line.trim_start_matches(' ');
            let line_depth = (line.len() - actual_line.len()) as u32;

            Some((style, line_depth))
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
                // let first_word_len = line
                //     .trim_start()
                //     .split(&[' ', ',', '.'])
                //     .next()
                //     .filter(|word| word.chars().all(|c| c.is_ascii_alphanumeric()))
                //     .map(|word| word.len())
                //     .unwrap_or(0);

                let has_hyphened_word = text.ends_with('-')
                    && text.as_bytes()[text.as_bytes().len() - 2].is_ascii_alphabetic();

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
            start_info: Self::parse_start_info(&document)?,
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

        this.sections = this.parse_sections(&document)?;

        Ok(this.cleanup_document())
    }

    pub fn into_raw_parts(self) -> (StartInfo, Box<str>, Vec<Section>) {
        (self.start_info, self.title, self.sections)
    }

    pub fn print(&self) -> String {
        let mut result = String::with_capacity(65536);

        fn print_paragraph_element(element: &ParagraphElement, output: &mut String) {
            match element {
                ParagraphElement::Text(text) => {
                    output.push_str(text);
                }
                ParagraphElement::DocReference(rfc, inner) => {
                    output.push_str(&format!(r#"<a href="./{rfc}">{inner}</a>"#))
                }
                ParagraphElement::CrossReference((rfc, section), inner) => {
                    output.push_str(&format!(r#"<a href="./{rfc}#{section}">{inner}</a>"#))
                }
                ParagraphElement::SelfReference(section, inner) => {
                    output.push_str(&format!(r##"<a href="#{section}">{inner}</a>"##))
                }
                ParagraphElement::ExtReference(href, inner) => {
                    output.push_str(&format!(r#"<a href="{href}">{inner}</a>"#))
                }
                ParagraphElement::Anchor(id, inner) => match inner {
                    Some(text) => output.push_str(&format!(r#"<span id="{id}">{text}</span>"#)),
                    None => output.push_str(&format!(r#"<span id="{id}"></span>"#)),
                },
            }
        }

        fn print_element(element: &Element, outer_depth: u32, output: &mut String) {
            match element {
                Element::Paragraph { depth, elements } => {
                    output.push_str(&" ".repeat((outer_depth + *depth) as usize));
                    output.push_str("<p>");
                    print_paragraph_element(&elements[0], output);
                    for para_el in &elements[1..] {
                        output.push('\n');
                        output.push_str(&" ".repeat((outer_depth + *depth) as usize));
                        print_paragraph_element(para_el, output);
                    }
                    output.push_str(&" ".repeat((outer_depth + *depth) as usize));
                    output.push_str("</p>\n");
                }

                Element::OrderedList {
                    depth,
                    items,
                    style,
                    ..
                } => {
                    output.push_str(&" ".repeat((outer_depth + *depth) as usize));
                    output.push_str(&format!(
                        "<ol type=\"{}\" start=\"{}\">\n",
                        match style {
                            OrderedListStyle::DottedLetterLower
                            | OrderedListStyle::BracketedLetterLower => "a",
                            OrderedListStyle::DottedLetterUpper
                            | OrderedListStyle::BracketedLetterUpper => "A",
                            OrderedListStyle::DottedNumber
                            | OrderedListStyle::BracketedNumber
                            | OrderedListStyle::UndottedNumber => "1",
                            OrderedListStyle::BracketedRoman => "i",
                        },
                        items[0].0.unwrap()
                    ));
                    output.push_str(&" ".repeat((outer_depth + *depth) as usize));
                    output.push_str("<li>\n");
                    print_element(&items[0].1, outer_depth + *depth, output);
                    for (marker, item) in &items[1..] {
                        if marker.is_some() {
                            output.push_str(&" ".repeat((outer_depth + *depth) as usize));
                            output.push_str("</li>\n");
                            output.push_str("<li>\n");
                        }
                        print_element(item, outer_depth + *depth, output);
                        output.push('\n');
                    }
                    output.push_str(&" ".repeat((outer_depth + *depth) as usize));
                    output.push_str("</li>\n");
                    output.push_str(&" ".repeat((outer_depth + *depth) as usize));
                    output.push_str("</ol>\n");
                }

                Element::UnorderedList { depth, items, .. } => {
                    output.push_str(&" ".repeat((outer_depth + *depth) as usize));
                    output.push_str("<ul>\n");
                    output.push_str(&" ".repeat((outer_depth + *depth) as usize));
                    output.push_str("<li>\n");
                    print_element(&items[0].1, outer_depth + *depth, output);
                    for (marker, item) in &items[1..] {
                        if *marker {
                            output.push_str(&" ".repeat((outer_depth + *depth) as usize));
                            output.push_str("</li>\n");
                            output.push_str("<li>\n");
                        }
                        print_element(item, outer_depth + *depth, output);
                        output.push('\n');
                    }
                    output.push_str(&" ".repeat((outer_depth + *depth) as usize));
                    output.push_str("</li>\n");
                    output.push_str(&" ".repeat((outer_depth + *depth) as usize));
                    output.push_str("</ul>\n");
                }
                Element::Preformatted { depth, elements } => {
                    output.push_str(&" ".repeat((outer_depth + *depth) as usize));
                    output.push_str("<pre>");
                    output.push_str(&" ".repeat(*depth as usize));
                    print_paragraph_element(&elements[0], output);
                    for para_el in &elements[1..] {
                        output.push_str(&" ".repeat((outer_depth + *depth) as usize));
                        print_paragraph_element(para_el, output);
                    }
                    output.push_str("</pre>\n");
                }
            }
        }

        for section in &self.sections {
            if section.title.to_ascii_lowercase().contains("table of contents") {
                continue
            }
            result.push_str(&format!(
                "<h{0} id={2}>{1}</h{0}>\n",
                section.level.max(1) + 1,
                section.title,
                section.id.as_deref().unwrap_or(""),
            ));

            for element in &section.elements {
                print_element(element, 0, &mut result);
            }
        }

        result
    }

    // The preprocessing here removes page boundaries (`<span id="page-N">`) and
    // excessive newlines created by those boundaries.
    fn preprocess_phase1(document: Phase1Document) -> Result<Phase1Document, String> {
        let (meta_info, mut elements) = document.into_raw_parts();

        // Remove all elements before <h1> and also itself (and the following newline)
        if let Some(pos) = elements.iter().position(|el| el.is_heading()) {
            elements = elements.split_off(pos + 1 + 1);
        } else {
            return Err("couldn't find the document title during preprocessing".into());
        }

        // Remove excessive blank lines before and after a page boundary.
        let mut i = 0;
        while i < elements.len() {
            if let Phase1Element::Anchor(id, _) = &elements[i]
                && id.starts_with("page")
            {
                while let Some(Phase1Element::Line(line)) = elements.get(i - 1)
                    && line.is_empty()
                {
                    elements.remove(i - 1);
                    i -= 1;
                }

                // The anchor is followed by a newline. Since the anchor will be
                // removed, the Text will cover the whole line.
                if let Some(Phase1Element::Text { text, ending: true }) = elements.get(i + 1)
                    && text.is_empty()
                {
                    elements[i + 1] = Phase1Element::Line("".into());
                }

                // We know a page will always start at the same position, so there is no need
                // for unlimited removal like we do for elements[i-1] (where a page break
                // can result in multiple newlines). Limit to 3 newlines, to take into account
                // the excessive newlines caused by removal of <span class="grey"> in Phase 1
                let mut count = 0;
                while let Some(Phase1Element::Line(text)) = elements.get(i + 2)
                    && text.is_empty()
                    && count < 3
                {
                    elements.remove(i + 1);
                    count += 1;
                }
            }

            i += 1;
        }

        elements.retain(|el| {
            !matches!(
                el,
                Phase1Element::Anchor(id, text) if id.starts_with("page") && text.is_none()
            )
        });

        // Try to merge multi-line headings that were missed during phase 1.
        let mut i = 0;
        while i < elements.len() {
            let merge_with = if let Phase1Element::H2(this, id)
                | Phase1Element::H3(this, id)
                | Phase1Element::H4(this, id)
                | Phase1Element::H5(this, id)
                | Phase1Element::H6(this, id)
             = &elements[i] {
                let reasonable_start = if let Some(appendix) = id.strip_prefix("appendix-") {
                    "Appendix ".len() + appendix.len()
                } else if let Some(section) = id.strip_prefix("section-") {
                    section.len()
                } else {
                    0
                };

                let next_element = if let Some(Phase1Element::Text { text, ending: true }) = elements.get(i + 1)
                    && text.is_empty()
                {
                    i + 2
                } else {
                    i // The current element is guaranteed to fail the test.
                };

                if let Some(title_start) = this.find(". ")
                    && (title_start as i32 - reasonable_start as i32).abs() <= 1
                {
                    let required_spaces = title_start + 1 + this[title_start + 1..]
                        .chars()
                        .take_while(|c| *c == ' ')
                        .count();

                    if let Some(Phase1Element::Line(next)) = elements.get(next_element) {
                        let spaces = next
                            .chars()
                            .take_while(|c| *c == ' ')
                            .count();

                        (required_spaces == spaces).then_some(next_element)
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            };

            if let Some(merge_with) = merge_with {
                let merge = elements.remove(merge_with);

                if let Phase1Element::H2(this, _)
                    | Phase1Element::H3(this, _)
                    | Phase1Element::H4(this, _)
                    | Phase1Element::H5(this, _)
                    | Phase1Element::H6(this, _)
                = &mut elements[i] {
                    let mut this_owned = String::from(std::mem::take(this));
                    this_owned.push(' ');

                    if let Phase1Element::Line(line) = merge {
                        this_owned.push_str(line.trim_start());
                    } else {
                        unreachable!();
                    }

                    *this = this_owned.into_boxed_str();
                } else {
                    unreachable!();
                }
            }

            i += 1
        }

        Ok(Phase1Document::from_raw_parts(meta_info, elements))
    }

    fn parse_start_info(document: &Phase1Document) -> Result<StartInfo, String> {
        document
            .elements()
            .iter()
            .position(|el| el.is_heading())
            .ok_or_else(|| String::from("couldn't find heading in RFC document"))?;

        let mut this = StartInfo::default();

        let mut parsing_obselete = false;
        let mut left_column = Vec::new();
        let mut right_column = Vec::new();

        // All RFC documents begins like so:
        // `Some Research Organization (SRO)                          J. Fabulousity`
        // This constant represents the minimum amount of space between the left
        // and right side. 10 is arbitrarily chosen.
        const SPACE_THRESHOLD: usize = 10;
        let spaces = " ".repeat(SPACE_THRESHOLD);

        let lines = document
            .lines()
            .take_while(|line| !line.as_slice().iter().any(|el| el.is_heading()));

        for line in lines {
            match line.as_slice() {
                [Phase1Element::Line(line)] => {
                    if line.trim_ascii().is_empty() {
                        continue;
                    }

                    let split_at = line.find(&spaces);

                    if let Some(at) = split_at {
                        let (left, right) = line.split_at(at);
                        let right = right[SPACE_THRESHOLD..].trim_start_matches(' ');

                        if !left.is_empty() {
                            left_column.push(Phase1Element::Line(left.to_owned().into_boxed_str()));
                        }

                        if !right.is_empty() {
                            right_column.push(right);
                        }
                    } else {
                        left_column.push(Phase1Element::Line(line.to_owned()))
                    }
                }

                [Phase1Element::Text {
                    text: start_text, ..
                }, xs @ .., Phase1Element::Text { text: end_text, .. }] => {
                    if !start_text.trim().is_empty() {
                        left_column.push(Phase1Element::Line(start_text.clone()));
                    }

                    for x in xs {
                        if x.is_reference() {
                            left_column.push(x.clone());
                        } else if let Phase1Element::Text { text, .. } = x {
                            if text.trim() != "," {
                                return Err("invalid start info - found unexpected text".into());
                            }
                        } else {
                            return Err("invalid start info - found unexpected elements".into());
                        }
                    }

                    let end_text = end_text.strip_prefix(',').unwrap_or(end_text);
                    if end_text.starts_with(&spaces) {
                        right_column.push(end_text.trim_start());
                    }
                }

                _ => return Err("invalid start info - found unexpected elements".into()),
            }
        }

        // This regex attempts to find:
        // - One or more word
        // - (optionally) an abbreviation consisting of uppercase letters, enclosed by "()"
        // - SPACE_THRESHOLD amount of spaces following the above
        let stream_regex = Regex::new(r"^((?:\w+ )+(?:\w+|\([A-Z]+\)))").unwrap();

        // RFC streams, some kinda namespace
        if let Some(Phase1Element::Line(text)) = left_column.first()
            && let Some(captured) = stream_regex.captures(text)
        {
            this.stream = captured[1].to_owned().into_boxed_str();
            text.strip_prefix(&captured[0]).unwrap()
        } else {
            return Err("suspected invalid start info - invalid stream?".into());
        };

        for p1_element in &left_column[1..] {
            match p1_element {
                Phase1Element::Line(text) if text.starts_with("Request for Comments:") => {
                    parsing_obselete = false;

                    let regex = Regex::new(r"^Request for Comments:[ ]+(\d+)").unwrap();

                    if let Some(captured) = regex.captures(text) {
                        let rfc = captured[1].parse::<u32>().unwrap();
                        this.rfc = rfc;
                    } else {
                        return Err("invalid start info - in Request for Comments line".into());
                    };
                }

                Phase1Element::Line(text) if text.starts_with("Obsoletes:") => {
                    parsing_obselete = true;
                }

                Phase1Element::Line(text) if text.starts_with("Category:") => {
                    parsing_obselete = false;
                    this.category = text
                        .strip_prefix("Category:")
                        .unwrap()
                        .trim()
                        .to_owned()
                        .into_boxed_str();
                }

                Phase1Element::Line(text) if text.contains(':') && !parsing_obselete => {
                    parsing_obselete = false;
                    let mut split = text.split(':');
                    let first = split.next().unwrap().trim_start();
                    let second = split.next().unwrap().trim();

                    this.others.insert(first.to_string(), second.to_string());
                }

                Phase1Element::Line(text) if text.starts_with(",") && parsing_obselete => continue,

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
            } else if document
                .meta_info()
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

        Ok(this)
    }

    fn parse_sections(&mut self, document: &Phase1Document) -> Result<Vec<Section>, String> {
        let mut sections = Vec::new();

        let mut current_section = Section {
            id: None,
            level: 0,
            title: Box::<str>::default(),
            elements: Vec::new(),
        };

        for line in document.lines() {
            if let Some(new_section) = current_section.add_line(line)? {
                sections.push(std::mem::replace(&mut current_section, new_section));
            }
        }

        sections.push(current_section);
        Ok(sections)
    }

    fn cleanup_document(mut self) -> Self {
        fn cleanup_element(element: &mut Element) -> bool {
            match element {
                Element::Paragraph { elements, .. } if elements.is_empty() => false,
                Element::OrderedList { items, .. } => {
                    items.retain_mut(|item| cleanup_element(&mut item.1));
                    true
                }
                Element::UnorderedList { items, .. } => {
                    items.retain_mut(|item| cleanup_element(&mut item.1));
                    true
                }
                Element::Preformatted { elements, .. } => {
                    if let Some(ParagraphElement::Text(text)) = elements.last_mut()
                        && text.ends_with("\n\n")
                    {
                        text.pop();
                    }
                    true
                }
                _ => true,
            }
        }

        // Remove empty paragraphs
        for section in &mut self.sections {
            section.elements.retain_mut(cleanup_element);
        }

        for section in &mut self.sections {
            for element in &mut section.elements {
                if let Element::Preformatted {
                    depth,
                    elements: code_elements,
                } = element
                {
                    let mut min_ident = match &code_elements[0] {
                        ParagraphElement::Text(text) => {
                            text.chars().take_while(|c| *c == ' ').count()
                        }
                        _ => continue,
                    };

                    let mut was_end = true;
                    for code_element in &code_elements[1..] {
                        match code_element {
                            ParagraphElement::Text(text) if was_end => {
                                min_ident =
                                    min_ident.min(text.chars().take_while(|c| *c == ' ').count());
                            }
                            ParagraphElement::Text(text) => {
                                if text.ends_with("\n") {
                                    was_end = true
                                }
                            }
                            _ => was_end = false,
                        };
                    }

                    let mut was_end = true;
                    for code_element in code_elements.iter_mut() {
                        match code_element {
                            ParagraphElement::Text(text) if was_end => {
                                *text = text[min_ident..].to_string();
                            }
                            ParagraphElement::Text(text) => {
                                if text.ends_with("\n") {
                                    was_end = true
                                }
                            }
                            _ => was_end = false,
                        }
                    }

                    *depth = min_ident as u32;
                }
            }
        }

        self
    }
}
