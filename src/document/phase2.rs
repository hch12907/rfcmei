//! In this Phase, the beginning document headers are parsed. The document body is
//! split into multiple sections, but there isn't a tree structure yet. Lines are
//! transformed from a (text | metadata) sumtype into a (text, [metadata]) struct
//! for easier analysis in later Phases.

use std::collections::BTreeMap;
use std::ops::RangeBounds;

use regex::Regex;

use crate::document::phase1::MetaElement;

use super::phase1::{Phase1Document as Phase1Document, Element as Phase1Element};

/// This struct represents the information found in the topmost section of all RFC
/// contents
#[derive(Debug, Clone, Default)]
pub struct StartInfo {
    // Left side
    pub stream: Box<str>,
    pub rfc: u32,
    pub obsoletes: Vec<(u32, Box<str>)>,
    pub updates: Vec<(u32, Box<str>)>,
    pub date: Box<str>,
    pub category: Box<str>,
    pub others: BTreeMap<String, String>,

    // Right side
    pub authors: Vec<(Box<str>, Box<str>)>, // (author, organization)
}

#[derive(Debug, Clone, Default)]
pub struct Section {
    /// An optional fragment-identifier for the section.
    pub id: Option<Box<str>>,
    /// Level of the section, starts from 0 as the outermost level
    pub level: usize,
    pub title: Box<str>,
    pub lines: Vec<Line>,
}

#[derive(Debug, Clone)]
pub struct Line {
    pub text: String,
    /// How the line will be connected to the next line. The line in the final output
    /// shall become `current_line.remove_suffix('\n') + connector`.
    pub connector: Option<char>,
    pub metadata: Vec<LineMetadata>,
}

impl Line {
    /// Creates an empty line.
    pub fn new() -> Line {
        Line {
            text: String::new(),
            connector: Some('\n'),
            metadata: Vec::new(),
        }
    }

    /// Remove a portion of the line starting from the beginning.
    pub fn cut(&self, len: u32) -> Line {
        Line {
            text: self.text[len as usize..].to_owned(),
            connector: self.connector,
            metadata: self.metadata.iter()
                .cloned()
                .flat_map(|mut meta| {
                    if meta.column >= len {
                        meta.column -= len;
                        Some(meta)
                    } else {
                        None
                    }
                })
                .collect()
        }
    }

    /// Append some spaces to the line in the beginning such that it reaches a
    /// certain depth.
    pub fn pad(&self, depth: u32) -> Line {
        let current_depth = self.text.chars().take_while(|c| *c == ' ').count();
        let to_pad = depth as usize - current_depth;

        Line {
            text: " ".repeat(to_pad) + &self.text,
            connector: self.connector,
            metadata: self.metadata.iter()
                .cloned()
                .map(|mut meta| {
                    meta.column += to_pad as u32;
                    meta
                })
                .collect()
        }
    }

    pub fn carve_out<R: RangeBounds<usize>>(&self, at: R) -> (Line, Line) {
        let start = match at.start_bound() {
            std::ops::Bound::Included(i) => *i,
            std::ops::Bound::Excluded(_i) => unreachable!(),
            std::ops::Bound::Unbounded => 0,
        };

        let end = match at.end_bound() {
            std::ops::Bound::Included(i) => *i + 1,
            std::ops::Bound::Excluded(i) => *i,
            std::ops::Bound::Unbounded => todo!(),
        };

        let result = Line {
            text: self.text[start..end].to_owned(),
            connector: if self.text.len() == end { self.connector } else { None },
            metadata: self.metadata.iter()
                .cloned()
                .flat_map(|mut meta| {
                    if meta.column >= start as u32 
                        && meta.column + meta.length <= end as u32
                    {
                        meta.column -= start as u32;
                        meta.length = meta.length.min((end - start) as u32);
                        Some(meta)
                    } else {
                        None
                    }
                })
                .collect()
        };

        let remain = Line {
            text: self.text[..start].to_owned() + &self.text[end..],
            connector: if self.text.len() == end { None } else { self.connector },
            metadata: self.metadata.iter()
                .cloned()
                .flat_map(|mut meta| {
                    if meta.column < start as u32
                        || meta.column >= end as u32
                    {
                        meta.column -= start as u32;
                        meta.length = meta.length.min((end - start) as u32);
                        Some(meta)
                    } else {
                        None
                    }
                })
                .collect()
        };

        (remain, result)
    }

    pub fn trim_end(&mut self) {
        assert!(self.connector.is_none(), "{:?}", self);
        self.text = self.text.trim_end().to_owned();
    }
}

#[derive(Debug, Clone)]
pub struct LineMetadata {
    pub column: u32,
    pub length: u32,
    pub kind: LineMetadataKind,
}

#[derive(Debug, Clone)]
pub enum LineMetadataKind {
    /// See [Phase1Element::Reference]. Only the href is retained.
    Reference(Box<str>),
    /// See [Phase1Element::Anchor].
    Anchor(Box<str>),
    /// Keywords are bolded in the final output.
    Keyword,
}

#[derive(Debug, Clone)]
pub struct Phase2Document {
    pub start_info: StartInfo,
    pub title: Box<str>,
    pub sections: Vec<Section>,
}

impl Phase2Document {
    pub fn from_phase1(document: Phase1Document) -> Result<Self, String> {
        let mut this = Self {
            start_info: Self::parse_start_info(&document)?,
            title: Box::<str>::default(),
            sections: Vec::new(),
        };

        if let Some(Phase1Element::H1 { title: h1 }) =
            document.elements.iter().find(|el| el.is_heading())
        {
            this.title = h1.clone();
        }

        let document = Self::preprocess_phase1(document)?;

        eprintln!(
            "Parsed RFC {}, titled {:?}",
            this.start_info.rfc, this.title
        );

        this.sections = this.parse_sections(&document)?;

        Ok(this)
    }

    pub fn print(&self) -> String {
        let mut result = String::with_capacity(65536);

        for section in &self.sections {
            result.push_str("************\n");
            result.push_str(&format!("* title: {}\n", section.title));
            result.push_str(&format!(
                "* id: {} ({})\n",
                section.id.as_deref().unwrap_or("(none)"),
                section.level
            ));
            result.push_str("************\n");

            for (i, line) in section.lines.iter().enumerate() {
                result.push_str(&format!(
                    "line {:03} ; {:72} ; {:?}\n",
                    i, line.text, line.metadata
                ));
            }

            result.push('\n');
        }

        result
    }

    // The preprocessing here removes page boundaries (`<span id="page-N">`) and
    // excessive newlines created by those boundaries.
    fn preprocess_phase1(document: Phase1Document) -> Result<Phase1Document, String> {
        let Phase1Document {
            meta_info,
            mut elements,
        } = document;

        // Remove all elements before <h1> and also itself (and the following newline)
        if let Some(pos) = elements.iter().position(|el| el.is_heading()) {
            elements = elements.split_off(pos + 1 + 1);
        } else {
            return Err("couldn't find the document title during preprocessing".into());
        }

        // Remove excessive blank lines before and after a page boundary.
        let mut i = 0;
        while i < elements.len() {
            if let Phase1Element::Anchor { id, .. } = &elements[i]
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
                Phase1Element::Anchor { id, text } if id.starts_with("page") && text.is_none()
            )
        });

        // Try to merge multi-line headings that were missed during phase 1.
        let mut i = 0;
        while i < elements.len() {
            let merge_with = if let Phase1Element::H2 { title: this, id }
            | Phase1Element::H3 { title: this, id }
            | Phase1Element::H4 { title: this, id }
            | Phase1Element::H5 { title: this, id }
            | Phase1Element::H6 { title: this, id } = &elements[i]
            {
                let reasonable_start = if let Some(appendix) = id.strip_prefix("#appendix-") {
                    "Appendix ".len() + appendix.len()
                } else if let Some(section) = id.strip_prefix("#section-") {
                    section.len()
                } else {
                    0
                };

                let next_element = if let Some(Phase1Element::Text { text, ending: true }) =
                    elements.get(i + 1)
                    && text.is_empty()
                {
                    i + 2
                } else {
                    i // The current element is guaranteed to fail the test.
                };

                if let Some(title_start) = this.find(". ")
                    && (title_start as i32 - reasonable_start as i32).abs() <= 1
                {
                    let required_spaces = title_start
                        + 1
                        + this[title_start + 1..]
                            .chars()
                            .take_while(|c| *c == ' ')
                            .count();

                    if let Some(Phase1Element::Line(next)) = elements.get(next_element) {
                        let spaces = next.chars().take_while(|c| *c == ' ').count();

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

                if let Phase1Element::H2 { title: this, .. }
                | Phase1Element::H3 { title: this, .. }
                | Phase1Element::H4 { title: this, .. }
                | Phase1Element::H5 { title: this, .. }
                | Phase1Element::H6 { title: this, .. } = &mut elements[i]
                {
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

        Ok(Phase1Document {
            meta_info,
            elements,
        })
    }

    fn parse_start_info(document: &Phase1Document) -> Result<StartInfo, String> {
        document
            .elements
            .iter()
            .position(|el| el.is_heading())
            .ok_or_else(|| String::from("couldn't find heading in RFC document"))?;

        let mut this = StartInfo::default();

        #[derive(Debug, PartialEq, Eq)]
        enum State {
            Anything,
            Obsolete,
            Update,
        }

        let mut parsing_state = State::Anything;
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

        let doc_regex = Regex::new(r"^./rfc(\d+)$").unwrap();

        for p1_element in &left_column[1..] {
            match p1_element {
                Phase1Element::Line(text) if text.starts_with("Request for Comments:") => {
                    parsing_state = State::Anything;

                    let regex = Regex::new(r"^Request for Comments:[ ]+(\d+)").unwrap();

                    if let Some(captured) = regex.captures(text) {
                        let rfc = captured[1].parse::<u32>().unwrap();
                        this.rfc = rfc;
                    } else {
                        return Err("invalid start info - in Request for Comments line".into());
                    };
                }

                Phase1Element::Line(text) if text.starts_with("Obsoletes:") => {
                    parsing_state = State::Obsolete;
                }

                Phase1Element::Line(text) if text.starts_with("Updates:") => {
                    parsing_state = State::Update;
                }

                Phase1Element::Line(text) if text.starts_with("Category:") => {
                    parsing_state = State::Anything;
                    this.category = text
                        .strip_prefix("Category:")
                        .unwrap()
                        .trim()
                        .to_owned()
                        .into_boxed_str();
                }

                Phase1Element::Line(text) if text.contains(':') && parsing_state == State::Anything => {
                    let mut split = text.split(':');
                    let first = split.next().unwrap().trim_start();
                    let second = split.next().unwrap().trim();

                    this.others.insert(first.to_string(), second.to_string());
                }

                Phase1Element::Line(text) if text.starts_with(",") && parsing_state == State::Obsolete => continue,
                Phase1Element::Line(text) if text.starts_with(",") && parsing_state == State::Anything => continue,

                Phase1Element::Reference(doc, title) if parsing_state == State::Obsolete => {
                    if let Some(captured) = doc_regex.captures(&doc) {
                        let doc = captured[1].parse::<u32>().unwrap();
                        this.obsoletes.push((doc, title.clone()))
                    } else {
                        return Err("unexpected reference in start info".into());
                    }
                }

                Phase1Element::Reference(doc, title) if parsing_state == State::Update => {
                    if let Some(captured) = doc_regex.captures(&doc) {
                        let doc = captured[1].parse::<u32>().unwrap();
                        this.updates.push((doc, title.clone()))
                    } else {
                        return Err("unexpected reference in start info".into());
                    }
                }

                Phase1Element::Reference(_, _) if parsing_state == State::Anything => {
                    return Err("unexpected reference in start info".into())
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
                .meta_info
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
            lines: Vec::new(),
        };

        for line in document.lines() {
            match line.as_slice() {
                [Phase1Element::H1 { .. }] => {
                    panic!("encountered <h1> in parse_sections()")
                }

                [Phase1Element::H2 { title, id }
                | Phase1Element::H3 { title, id }
                | Phase1Element::H4 { title, id }
                | Phase1Element::H5 { title, id }
                | Phase1Element::H6 { title, id }, xs @ ..] => {
                    assert!(
                        xs.is_empty()
                            || matches!(
                                &xs[0],
                                Phase1Element::Text { text, ending: true }
                                    if text.is_empty()
                            ),
                        "headings must occupy the entire line"
                    );

                    if !(current_section.title.is_empty() && current_section.lines.is_empty()) {
                        sections.push(std::mem::take(&mut current_section));
                    }

                    current_section.title = title.clone();
                    current_section.level = id.chars().filter(|c| *c == '.').count();
                    current_section.id = Some(id.clone());
                }

                [Phase1Element::Line(line)] if line.is_empty() => {
                    if current_section.title.is_empty() {
                        continue;
                    } else {
                        current_section.lines.push(Line {
                            text: line.clone().into(),
                            connector: Some('\n'),
                            metadata: Vec::new(),
                        })
                    }
                }

                [Phase1Element::Line(line)] if !line.starts_with(' ') => {
                    if !(current_section.title.is_empty() && current_section.lines.is_empty()) {
                        sections.push(std::mem::take(&mut current_section));
                    }

                    current_section.title = line.clone();
                    current_section.level = 0;
                    current_section.id = None;
                }

                [Phase1Element::Line(line)] => current_section.lines.push(Line {
                    text: line.to_owned().into(),
                    connector: Some('\n'),
                    metadata: Vec::new(),
                }),

                [Phase1Element::Text {
                    text,
                    ending: false,
                }, xs @ .., Phase1Element::Text { ending: true, .. }] => {
                    assert!(text.starts_with("   "));

                    let mut this_line = Line {
                        text: line.make_string(),
                        connector: Some('\n'),
                        metadata: Vec::new(),
                    };

                    let mut column = text.len() as u32;
                    for x in xs {
                        match x {
                            Phase1Element::Line(text) | Phase1Element::Text { text, .. } => {
                                column += text.len() as u32;
                            }

                            Phase1Element::H1 { .. }
                            | Phase1Element::H2 { .. }
                            | Phase1Element::H3 { .. }
                            | Phase1Element::H4 { .. }
                            | Phase1Element::H5 { .. }
                            | Phase1Element::H6 { .. } => panic!("unexpected headings in sections"),

                            Phase1Element::Anchor { id, text } => {
                                let length = text.as_deref().unwrap_or("").len() as u32;

                                this_line.metadata.push(LineMetadata {
                                    column,
                                    length,
                                    kind: LineMetadataKind::Anchor(id.clone()),
                                });

                                column += length;
                            }

                            Phase1Element::Reference(href, text) => {
                                let length = text.len() as u32;

                                this_line.metadata.push(LineMetadata {
                                    column,
                                    length,
                                    kind: LineMetadataKind::Reference(href.clone()),
                                });

                                column += length;
                            }
                        }
                    }

                    current_section.lines.push(this_line);
                }

                [Phase1Element::Text {
                    text: _,
                    ending: true,
                }] => panic!("encountered Text that covers a whole line but was not Line"),

                xs @ [_, ..] => panic!(
                    "encountered lines not starting with text or heading: {:?}",
                    xs
                ),

                [] => panic!("encountered a completely empty line"),
            }
        }

        sections.push(current_section);

        Ok(sections)
    }
}
