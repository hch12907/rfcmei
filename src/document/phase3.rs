//! In this phase, parsing of text content begin. The textual contents are determined
//! to whether be paragraphs, lists (ordered or unordered), or preformatted sections.
//! Phase 3 will produce a tree that Phase 4 consumes.

use std::sync::LazyLock;

use regex::{Regex, RegexSet};

use super::phase2::{Line, Phase2Document, StartInfo, Section as Phase2Section};

#[derive(Debug, Clone)]
pub struct Phase3Document {
    pub start_info: StartInfo,
    pub title: Box<str>,
    pub sections: Vec<Section>,
}

impl Phase3Document {
    pub fn from_phase2(phase2: Phase2Document) -> Result<Self, String> {
        let Phase2Document {
            start_info,
            title,
            sections
        } = phase2;

        let sections = sections
            .into_iter()
            .map(Section::from_phase2)
            .collect::<Result<_, _>>()?;

        Ok(Self {
            start_info,
            title,
            sections,
        })
    }

    pub fn print(&self) -> String {
        let mut result = String::with_capacity(65536);

        fn print_element(element: &Element, output: &mut String) {
            match element {
                Element::Paragraph { depth, preformatted, lines, .. } => {
                    if *preformatted {
                        output.push_str("<pre>");
                    } else {
                        output.push_str("<p>");
                    }

                    let space = if *preformatted {
                        &" ".repeat(*depth as usize)
                    } else {
                        ""
                    };

                    for line in lines {
                        output.push_str(&space);
                        output.push_str(&line.text);
                        if let Some(connector) = line.connector {
                            output.push(connector);
                        }
                    }

                    if *preformatted {
                        output.push_str("</pre>");
                    } else {
                        output.push_str("</p>");
                    }
                },
                Element::OrderedList { depth: _, style, items } => {
                    output.push_str(&format!(
                        "<ol type=\"{}\" start=\"{}\">",
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

                    output.push_str("<li>");
                    print_element(&items[0].1, output);
                    for (marker, item) in &items[1..] {
                        if marker.is_some() {
                            output.push_str("</li>");
                            output.push_str("<li>");
                        }
                        print_element(item, output);
                    }
                    output.push_str("</li>");
                    output.push_str("</ol>");
                },
                Element::UnorderedList { depth: _, style: _, items } => {
                    output.push_str("<ul>");
                    output.push_str("<li>");
                    print_element(&items[0].1, output);
                    for (marker, item) in &items[1..] {
                        if *marker {
                            output.push_str("</li>");
                            output.push_str("<li>");
                        }
                        print_element(item, output);
                    }
                    output.push_str("</li>");
                    output.push_str("</ul>");
                },
            }
        }

        result.push_str(&format!("<h1>{}</h1>", &self.title));

        for section in &self.sections {
            if section
            .title
            .to_ascii_lowercase()
            .contains("table of contents")
            {
                continue;
            }

            result.push_str(&format!(
                "<h{0} id=\"{2}\">{1}</h{0}>\n",
                section.level.max(1) + 1,
                section.title,
                section.id.as_deref().unwrap_or(""),
            ));

            for element in &section.elements {
                print_element(element, &mut result);
            }
        }

        result
    }
}

#[derive(Debug, Clone, Default)]
pub struct Section {
    /// An optional fragment-identifier for the section.
    pub id: Option<Box<str>>,
    /// Level of the section, starts from 0 as the outermost level
    pub level: usize,
    pub title: Box<str>,
    pub elements: Vec<Element>,
}

impl Section {
    fn from_phase2(phase2: Phase2Section) -> Result<Self, String> {
        let Phase2Section { id, level, title, lines } = phase2;

        let empty_lines = lines.iter().take_while(|line| line.text.is_empty()).count();
        let mut lines = lines.into_iter().skip(empty_lines);

        let elements = if let Some(line) = lines.next() {
            let mut elements = Vec::new();
            let mut current_element = Element::from_line(&line, 0);

            for line in lines {
                if let Some(el) = current_element.add_line(&line, 0) {
                    elements.push(current_element);
                    current_element = el;
                };
            }

            elements.push(current_element);
            elements
        } else {
            Vec::new()
        };

        let mut this = Self {
            id,
            level,
            title,
            elements
        };
        this.fixup_paragraphs();

        Ok(this)
    }

    fn fixup_paragraphs(&mut self) {
        // Determine whether a paragraph is preformatted. While we are at it, also
        // find out the depth of the paragraph
        for element in &mut self.elements {
            if let Element::Paragraph { depth, preformatted, lines } = element {
                // If the ratio of code_lines : noncode_lines is >= 1, we consider
                // the paragraph a preformatted block.
                let mut code_lines = 0;
                let mut noncode_lines = 0;

                let mut common_depth = lines
                    .first()
                    .map(|line| line.text.chars().take_while(|c| *c == ' ').count())
                    .unwrap_or(0);

                for line in lines.iter() {
                    if Self::is_likely_preformatted(&line.text) {
                        code_lines += 1
                    } else {
                        noncode_lines += 1
                    }

                    common_depth = common_depth
                        .min(line.text.chars().take_while(|c| *c == ' ').count());
                }

                if code_lines == 0 && noncode_lines == 0 {
                    continue
                }

                if noncode_lines == 0 || code_lines / noncode_lines > 0 {
                    *preformatted = true;
                    *depth = common_depth as u32;
                } else {
                    *preformatted = false;
                    *depth = common_depth as u32;
                }

                for line in lines.iter_mut() {
                    *line = line.cut(*depth);
                }
            }
        }

        // If there is a list in between two preformatted blocks, there is a
        // possibility it is a misanalysis. Undo the list.
        let mut i = 1;
        while i < self.elements.len().saturating_sub(1) {
            let Element::UnorderedList {
                depth: _,
                style: _,
                items: _,
            } = &self.elements[i] else {
                i += 1;
                continue
            };

            let Element::Paragraph {
                depth,
                preformatted: true,
                lines: _,
            } = &self.elements[i - 1] else {
                i += 1;
                continue
            };
            let depth = *depth;

            let Element::Paragraph {
                depth: _,
                preformatted: true,
                lines: _,
            } = &self.elements[i + 1] else {
                i += 1;
                continue
            };

            let Element::UnorderedList { depth: list_depth, style, items, .. } = self.elements.remove(i) else {
                unreachable!();
            };
            let Element::Paragraph { lines, .. } = &mut self.elements[i - 1] else {
                unreachable!()
            };

            for (marker, item) in items {
                let mut new_line = Line::new();

                for _ in 0..list_depth - depth {
                    new_line.text.push(' ');
                }

                if marker {
                    new_line.text.push(style.to_char());
                }

                match item {
                    Element::Paragraph { depth, lines, .. } => {
                        for _ in 0..depth {
                            new_line.text.push(' ');
                        }
                        for line in lines {
                            new_line.text.push_str(&line.text);
                        }
                    },
                    Element::OrderedList { depth: _, style: _, items: _ } => todo!(),
                    Element::UnorderedList { depth: _, style: _, items: _ } => todo!(),
                }

                lines.push(new_line);
            }
        }

        // Merge neighbouring preformatted blocks together.
        let mut i = 0;
        while i < self.elements.len().saturating_sub(1) {
            let Element::Paragraph {
                depth: depth_this,
                preformatted: preformatted_this,
                lines: _,
            } = &self.elements[i] else {
                i += 1;
                continue
            };

            let Element::Paragraph {
                depth: depth_next,
                preformatted: preformatted_next,
                lines: _,
            } = &self.elements[i + 1] else {
                i += 1;
                continue
            };

            if *depth_this != *depth_next
                || !*preformatted_this
                || !*preformatted_next
            {
                i += 1;
                continue
            }

            // We will merge the next element into the current one.
            let Element::Paragraph { lines: lines_next, .. } = self.elements.remove(i + 1) else {
                unreachable!();
            };
            let Element::Paragraph { lines, .. } = &mut self.elements[i] else {
                unreachable!()
            };
            // Neighbouring [Element::Paragraph]s are separated by a blank line.
            lines.push(Line::new());
            lines.extend_from_slice(&lines_next);
        }

        // Remove connectors or replace them with a space in non-preformatted
        // paragraphs.
        for element in &mut self.elements {
            let Element::Paragraph {
                depth: _,
                preformatted: false,
                lines,
            } = element else {
                continue
            };

            for line in lines.iter_mut() {
                let has_hyphened_word = line.text.ends_with('-')
                    && line.text.as_bytes()[line.text.as_bytes().len() - 2]
                        .is_ascii_alphabetic();
                
                if has_hyphened_word {
                    line.connector = None;
                } else {
                    line.connector = Some(' ');
                }
            }
        }
    }

    fn is_likely_preformatted(line: &str) -> bool {
        const GRAPHICAL_CANDIDATES: [u8; 8] = [b'-', b'+', b'/', b'_', b':', b'*', b'\\', b'|'];
        const CODE_SCORE_THRESHOLD: usize = 800;

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

    pub fn to_char(&self) -> char {
        match self {
            UnorderedListStyle::Asterisk => '*',
            UnorderedListStyle::Dash => '-',
            UnorderedListStyle::Round => 'o',
            UnorderedListStyle::Plus => '+',
        }
    }
}

#[derive(Debug, Clone)]
pub enum Element {
    /// Ordinary paragraph. Note that a paragraph MUST NOT contain empty lines.
    /// Any empty lines will create a new paragraph.
    Paragraph {
        depth: u32,
        preformatted: bool,
        lines: Vec<Line>,
    },
    /// List with a starting number. Each list item may hold multiple paragraphs,
    /// hence they are made a Vec of Element.
    ///
    /// Depth is defined as number of spaces before the item marker, so
    /// ` 1. An item` will have a depth of 1.
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
    /// ` - An item` will have a depth of 1.
    ///
    /// Each item carries a bool to indicate whether they have a mark.
    UnorderedList {
        depth: u32,
        style: UnorderedListStyle,
        items: Vec<(bool, Self)>,
    },
}

impl Element {
    fn depth(&self) -> u32 {
        match self {
            Element::Paragraph { depth, .. } => *depth,
            Element::OrderedList { depth, .. } => *depth,
            Element::UnorderedList { depth, .. } => *depth,
        }
    }

    fn from_line(line: &Line, ignore: usize) -> Self {
        let start = &line.text[ignore..];
        let trimmed_start = start.trim_start_matches(' ');
        let start_depth = (start.len() - trimmed_start.len()) as u32;

        assert!(start_depth > 0);

        if let Some((style, depth)) = UnorderedListStyle::extract_from_line(start) {
            Self::UnorderedList {
                depth: start_depth,
                style,
                items: vec![(
                    true,
                    Self::Paragraph {
                        depth: depth - (start_depth + 1),
                        preformatted: false,
                        lines: vec![line.cut(depth + ignore as u32)],
                    },
                )],
            }
        } else if let Some((style, starting, _, content)) =
            OrderedListStyle::extract_from_line(trimmed_start)
        {
            let trimmed_content = content.trim_start();

            Self::OrderedList {
                depth: start_depth,
                style,
                items: vec![(
                    Some(starting),
                    Element::Paragraph {
                        depth: (content.len() - trimmed_content.len()) as u32,
                        preformatted: false,
                        lines: vec![line.cut((start.len() - trimmed_content.len() + ignore) as u32)],
                    },
                )],
            }
        } else {
            Self::Paragraph {
                depth: 0, // We will revisit this later
                preformatted: false,
                lines: vec![line.clone()],
            }
        }
    }

    /// Add a new line to the [Element]. If the addition succeeds, the current [Element]
    /// is modified in place and a new [Element] may be returned. The new element needs
    /// to be inserted after the current one.
    fn add_line(&mut self, full_line: &Line, ignore: usize) -> Option<Self> {
        let line = &full_line.text[ignore..];
        let trimmed_line = line.trim_start();
        let line_depth = (line.len() - trimmed_line.len()) as u32;

        if full_line.text.is_empty() {
            return self.encountered_blank_line()
        }

        match self {
            Element::Paragraph { lines, depth, .. } if lines.is_empty() => {
                let parsed_line = Self::from_line(full_line, ignore);

                match parsed_line {
                    Element::Paragraph {
                        lines: new_lines,
                        depth: new_depth,
                        ..
                    } => {
                        *depth = new_depth;
                        *lines = new_lines;

                        None
                    }

                    x => Some(x),
                }
            }

            Element::Paragraph {
                lines,
                preformatted: _,
                depth,
            } => {
                let apparent_end = lines
                    .last()
                    .map(|line| line.text.ends_with(':'))
                    .unwrap_or(false);

                // If a paragraph ends in ":" we somewhat treat it as if it
                // has already ended - look for list item markers. If there
                // isn't any, continue the paragraph as normal.
                if apparent_end
                    && (UnorderedListStyle::extract_from_line(line).is_some()
                        || OrderedListStyle::extract_from_line(line).is_some())
                {
                    Some(Self::from_line(full_line, ignore))
                } else {
                    if *depth == 0 {
                        lines.push(full_line.clone());
                    } else {
                        lines.push(full_line.cut(*depth + ignore as u32));
                    }
                    None
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
                    let content_start =
                        line_depth + marker.len() as u32 + item_style.occupied_space();

                    if depth_difference.abs() <= 1 {
                        // depth == line_depth

                        items.push((
                            Some(num),
                            Element::Paragraph {
                                depth: content_depth,
                                preformatted: false,
                                lines: vec![full_line.cut(content_start + content_depth + ignore as u32)],
                            },
                        ));

                        None
                    } else if depth_difference < -1 {
                        // line_depth < depth
                        Some(Self::from_line(full_line, ignore))
                    } else {
                        // line_depth > depth
                        let el = &mut items.last_mut().unwrap().1;
                        if let Some(new_el) =
                            el.add_line(full_line, ignore + content_start as usize)
                        {
                            items.push((None, new_el));
                        }

                        None
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
                            el.add_line(full_line, ignore + content_start as usize)
                        {
                            items.push((None, new_el));
                        }

                        None
                    } else if depth_difference < -1 {
                        // line_depth < (content_start + inner_depth)
                        Some(Self::from_line(full_line, ignore))
                    } else {
                        // line_depth > (content_start + inner_depth)
                        let el = &mut items.last_mut().unwrap().1;
                        if let Some(new_el) =
                            el.add_line(full_line, ignore + content_start as usize)
                        {
                            items.push((None, new_el));
                        }

                        None
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

                    if *depth == line_depth {
                        items.push((
                            true,
                            Element::Paragraph {
                                depth: item_depth - content_start,
                                preformatted: false,
                                lines: vec![full_line.cut(item_depth + ignore as u32)],
                            },
                        ));

                        None
                    } else if line_depth < *depth {
                        Some(Self::from_line(full_line, ignore))
                    } else {
                        // line_depth > depth
                        let el = &mut items.last_mut().unwrap().1;
                        if let Some(new_el) =
                            el.add_line(full_line, ignore + content_start as usize)
                        {
                            items.push((false, new_el));
                        }

                        None
                    }
                } else {
                    let inner_depth = items.last().map(|item| item.1.depth()).unwrap();
                    let content_start = *depth + 1;

                    if content_start + inner_depth == line_depth {
                        let el = &mut items.last_mut().unwrap().1;
                        if let Some(new_el) =
                            el.add_line(full_line, ignore + content_start as usize)
                        {
                            items.push((false, new_el));
                        }

                        None
                    } else if line_depth < content_start + inner_depth {
                        Some(Self::from_line(full_line, ignore))
                    } else {
                        // line_depth > content_start + inner_depth
                        let el = &mut items.last_mut().unwrap().1;
                        if let Some(new_el) =
                            el.add_line(full_line, ignore + content_start as usize)
                        {
                            items.push((false, new_el));
                        }

                        None
                    }
                }
            }
        }
    }

    fn encountered_blank_line(&mut self) -> Option<Self> {
        match self {
            Self::Paragraph {
                depth,
                ref lines,
                ..
            } if !lines.is_empty() => Some(Element::Paragraph {
                depth: *depth,
                preformatted: false,
                lines: Vec::new(),
            }),

            Self::Paragraph { .. } => None,

            // For the Lists, we create a new empty Paragraph in the last item
            Self::OrderedList { ref mut items, .. } => {
                let last_item = &mut items.last_mut().unwrap().1;

                if let Some(new_el) = last_item.encountered_blank_line() {
                    items.push((None, new_el));
                }

                None
            }

            Self::UnorderedList { ref mut items, .. } => {
                let last_item = &mut items.last_mut().unwrap().1;

                if let Some(new_el) = last_item.encountered_blank_line() {
                    items.push((false, new_el));
                }

                None
            }
        }
    }
}
