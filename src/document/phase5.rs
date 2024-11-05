//! Phase 5 removes the concept of lines from the document.

use std::sync::LazyLock;

use regex::Regex;

use super::phase2::{Line, LineMetadataKind, StartInfo};
use super::phase3::{OrderedListStyle, UnorderedListStyle};
use super::phase4::{Element as Phase4Element, Phase4Document, Section as Phase4Section};

#[derive(Debug, Clone)]
pub struct Phase5Document {
    pub start_info: StartInfo,
    pub title: Box<str>,
    pub sections: Vec<Section>,
}

impl Phase5Document {
    pub fn from_phase4(phase4: Phase4Document) -> Self {
        let Phase4Document {
            start_info,
            title,
            sections,
        } = phase4;

        let mut this = Self {
            start_info,
            title,
            sections: sections.into_iter().map(Section::from_phase4).collect(),
        };

        this.mark_keywords();

        this
    }

    pub fn print(&self) -> String {
        format!("{:#?}", self)
    }

    fn mark_keywords(&mut self) {
        static KEYWORDS_REGEX: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new(
                r"^(MUST NOT|MUST|REQUIRED|SHALL NOT|SHALL|SHOULD NOT|SHOULD|NOT RECOMMENDED|RECOMMENDED|MAY|OPTIONAL)\b"
            ).unwrap()
        });

        fn mark_keywords_in_inner_elements(elements: &mut Vec<InnerElement>) {
            let mut i = 0;
            while i < elements.len() {
                let mut buffer = Vec::new();
                let mut current = String::new();

                match &elements[i] {
                    InnerElement::Text(text) => {
                        let mut chars = text.chars();

                        while !chars.as_str().is_empty() {
                            if let Some(found) = KEYWORDS_REGEX.find(chars.as_str()) {
                                let keyword = InnerElement::Keyword(found.as_str().to_owned());
                                chars.nth(found.len() - 1).unwrap();

                                if !current.is_empty() {
                                    let text = InnerElement::Text(std::mem::take(&mut current));
                                    buffer.push(text);
                                }

                                buffer.push(keyword);
                            } else if let Some(c) = chars.next() {
                                current.push(c);
                            }
                        }

                        if !current.is_empty() {
                            let text = InnerElement::Text(std::mem::take(&mut current));
                            buffer.push(text);
                        }
                    }
                    InnerElement::Anchor(_, _) => (),
                    InnerElement::Reference(_, _) => (),
                    InnerElement::Keyword(_) => (),
                    InnerElement::Break => (),
                }

                if !buffer.is_empty() {
                    let elements_after = elements.split_off(i);
                    elements.extend_from_slice(&buffer);
                    elements.extend_from_slice(&elements_after[1..]);
                }

                i += 1;
            }
        }

        fn mark_keywords_in_element(element: &mut Element) {
            match element {
                Element::Paragraph {
                    preformatted,
                    elements,
                    ..
                } => {
                    if *preformatted {
                        return;
                    }
                    mark_keywords_in_inner_elements(elements);
                }
                Element::DefinitionList { definitions, .. } => {
                    for (_term, def) in definitions {
                        mark_keywords_in_inner_elements(def);
                    }
                }
                Element::OrderedList { items, .. } => {
                    for item in items {
                        mark_keywords_in_element(&mut item.1);
                    }
                }
                Element::UnorderedList { items, .. } => {
                    for item in items {
                        mark_keywords_in_element(&mut item.1);
                    }
                }
                Element::Table {
                    depth: _,
                    headings: _,
                    cells: _,
                } => todo!(),
            }
        }

        for section in &mut self.sections {
            for element in &mut section.elements {
                mark_keywords_in_element(element);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Section {
    pub id: Option<Box<str>>, // An optional fragment-identifier for the section.
    pub level: usize,         // Level of the section, starts from 0 as the outermost level
    pub title: Box<str>,
    pub elements: Vec<Element>,
}

impl Section {
    pub fn from_phase4(phase4: Phase4Section) -> Self {
        let Phase4Section {
            id,
            level,
            title,
            elements,
        } = phase4;

        Self {
            id,
            level,
            title,
            elements: elements.into_iter().map(Element::from_phase4).collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Element {
    Paragraph {
        depth: u32,
        indentation: Indent,
        preformatted: bool,
        elements: Vec<InnerElement>,
    },
    DefinitionList {
        depth: u32,
        definitions: Vec<(Vec<InnerElement>, Vec<InnerElement>)>,
    },
    OrderedList {
        depth: u32,
        style: OrderedListStyle,
        items: Vec<(Option<u32>, Self)>,
    },
    UnorderedList {
        depth: u32,
        style: UnorderedListStyle,
        items: Vec<(bool, Self)>,
    },
    Table {
        depth: u32,
        headings: Vec<Vec<InnerElement>>,
        cells: Vec<Vec<Self>>,
    },
}

impl Element {
    fn from_phase4(phase4: Phase4Element) -> Self {
        match phase4 {
            Phase4Element::Paragraph {
                depth,
                preformatted,
                lines,
            } => {
                let indentation = {
                    let first_depth = lines
                        .first()
                        .map(|line| line.text.chars().take_while(|c| *c == ' ').count())
                        .unwrap_or(0);
                    let next_depth = lines
                        .get(1)
                        .map(|line| line.text.chars().take_while(|c| *c == ' ').count())
                        .unwrap_or(0);

                    let mut same_depth = true;
                    if lines.len() > 2 {
                        for line in &lines[2..] {
                            let depth = line.text.chars().take_while(|c| *c == ' ').count();
                            same_depth &= depth == next_depth;
                        }
                    }

                    if same_depth {
                        if first_depth > next_depth {
                            Indent::Indented
                        } else if first_depth == next_depth {
                            Indent::Normal
                        } else {
                            Indent::Hanging
                        }
                    } else {
                        Indent::Normal
                    }
                };

                let elements = InnerElement::from_lines(preformatted, depth, lines);

                Element::Paragraph {
                    depth,
                    indentation,
                    preformatted,
                    elements,
                }
            }
            Phase4Element::DefinitionList { depth, definitions } => {
                let definitions = definitions
                    .into_iter()
                    .map(|(term, def)| {
                        let term = InnerElement::from_line(&term);
                        let def = InnerElement::from_lines(false, 0, def);
                        (term, def)
                    })
                    .collect();
                Element::DefinitionList { depth, definitions }
            }
            Phase4Element::OrderedList {
                depth,
                style,
                items,
            } => {
                let items = items
                    .into_iter()
                    .map(|item| (item.0, Self::from_phase4(item.1)))
                    .collect();

                Element::OrderedList {
                    depth,
                    style,
                    items,
                }
            }
            Phase4Element::UnorderedList {
                depth,
                style,
                items,
            } => {
                let items = items
                    .into_iter()
                    .map(|item| (item.0, Self::from_phase4(item.1)))
                    .collect();

                Element::UnorderedList {
                    depth,
                    style,
                    items,
                }
            }
            Phase4Element::Table {
                depth,
                headings,
                cells,
            } => {
                let headings = headings.iter().map(InnerElement::from_line).collect();
                let cells = cells
                    .into_iter()
                    .map(|row| row.into_iter().map(Self::from_phase4).collect())
                    .collect();
                Element::Table {
                    depth,
                    headings,
                    cells,
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum InnerElement {
    Break,
    Text(String),
    Anchor(String, Box<Self>),
    Reference(String, Box<Self>),
    Keyword(String),
}

impl InnerElement {
    fn from_lines(preformatted: bool, depth: u32, lines: Vec<Line>) -> Vec<Self> {
        let mut result = Vec::new();

        for (i, line) in lines.iter().enumerate() {
            let mut this_line = Self::from_line(line);

            if preformatted {
                let spaces = " ".repeat(depth as usize);
                if let Some(InnerElement::Text(first)) = this_line.first_mut() {
                    *first = spaces + first;
                } else {
                    this_line.insert(0, InnerElement::Text(spaces))
                }
            }

            if let Some(InnerElement::Text(last)) = result.last_mut()
                && let Some(InnerElement::Text(this)) = this_line.first()
            {
                last.push_str(this);
                result.extend_from_slice(&this_line[1..]);
            } else {
                result.extend_from_slice(&this_line);
            }

            if !preformatted && let Some(next_line) = lines.get(i + 1) {
                let depth_now = line.text.chars().take_while(|c| *c == ' ').count();
                let depth_next = next_line.text.chars().take_while(|c| *c == ' ').count();

                if depth_now != depth_next {
                    result.push(InnerElement::Break);
                }
            }
        }

        result
    }

    fn from_line(line: &Line) -> Vec<Self> {
        Self::from_line_inner(line, 0, line.text.len() as u32, None)
    }

    fn from_line_inner(line: &Line, start: u32, len: u32, ignore: Option<usize>) -> Vec<Self> {
        let mut iter = line
            .text
            .as_bytes()
            .iter()
            .enumerate()
            .skip(start as usize)
            .take(len as usize);

        let mut result = Vec::new();
        let mut current = Vec::<u8>::new();

        while let Some((i, c)) = iter.next() {
            let metadata_idx = line
                .metadata
                .iter()
                .position(|meta| meta.column as usize == i);

            let metadata = if metadata_idx != ignore {
                metadata_idx.map(|i| &line.metadata[i])
            } else {
                None
            };

            if let Some(metadata) = metadata {
                if !current.is_empty() {
                    let text = String::from_utf8(std::mem::take(&mut current)).unwrap();
                    result.push(Self::Text(text));
                }

                match &metadata.kind {
                    LineMetadataKind::Reference(r) => {
                        let mut inner = Self::from_line_inner(
                            line,
                            metadata.column,
                            metadata.length,
                            metadata_idx,
                        );
                        assert!(inner.len() == 1);

                        if metadata.length > 1 {
                            iter.nth((metadata.length - 2) as usize);
                        }

                        result.push(Self::Reference(
                            r.clone().into(),
                            Box::new(inner.pop().unwrap()),
                        ))
                    }
                    LineMetadataKind::Anchor(a) => {
                        let mut inner = Self::from_line_inner(
                            line,
                            metadata.column,
                            metadata.length,
                            metadata_idx,
                        );
                        assert!(inner.len() == 1);

                        if metadata.length > 1 {
                            iter.nth((metadata.length - 2) as usize);
                        }

                        result.push(Self::Anchor(
                            a.clone().into(),
                            Box::new(inner.pop().unwrap()),
                        ))
                    }
                    LineMetadataKind::Keyword => todo!(),
                }
            } else {
                current.push(*c);
            }
        }

        if (start + len) as usize == line.text.len()
            && let Some(connector) = line.connector
        {
            let mut utf8 = [0u8; 4];
            connector.encode_utf8(&mut utf8);
            current.extend_from_slice(&utf8[..connector.len_utf8()]);
        }

        if !current.is_empty() {
            let text = String::from_utf8(std::mem::take(&mut current)).unwrap();
            result.push(Self::Text(text));
        }

        result
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Indent {
    Normal,
    Indented,
    Hanging,
}
