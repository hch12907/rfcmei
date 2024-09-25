//! In this phase, analysis of text content begin.
//!
//!

use std::collections::BTreeMap;

use regex::Regex;

use super::phase1::{Document as Phase1Document, Element as Phase1Element, MetaElement};

/// This struct represents the information found in the topmost section of all RFC
/// contents
#[derive(Debug, Default)]
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

#[derive(Debug, Default)]
pub struct Section {
    id: Option<Box<str>>, // An optional fragment-identifier for the section.
    level: usize,         // Level of the section, starts from 0 as the outermost level
    title: Box<str>,
    elements: Vec<Element>,
}

#[derive(Debug)]
pub enum ParagraphElement {
    /// A paragraph MUST NOT contain newlines. A newline will create a new
    /// paragraph.
    Text(String),
    DocReference(u32, Box<str>),
    CrossReference((u32, Box<str>), Box<str>),
    SelfReference(Box<str>, Box<str>),
    ExtReference(Box<str>, Box<str>),
    Anchor(Box<str>, Option<Box<str>>),
}

#[derive(Debug)]
pub(super) enum Element {
    /// Indentation and elements
    Paragraph(u32, Vec<ParagraphElement>),
    OrderedList(Vec<ParagraphElement>),
    UnorderedList(Vec<ParagraphElement>), // note: may start with 'o' or '-'
    Code(u32, String),                    // the newlines are all preserved in Code, but indent
}

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
                    Element::Paragraph(depth, para_elements) => {
                        let depth = " ".repeat(*depth as usize);
                        for pel in para_elements {
                            result.push_str(&depth);
                            match pel {
                                ParagraphElement::Text(text) => {
                                    result.push_str(text);
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
        elements: &[Phase1Element],
    ) -> Result<StartInfo, String> {
        let heading = elements
            .iter()
            .position(|el| el.is_heading())
            .ok_or_else(|| String::from("couldn't find heading in RFC document"))?;
        let elements = &elements[0..heading];

        let mut this = StartInfo::default();

        let mut parsing_obselete = false;
        let mut left_column = Vec::new();
        let mut right_column = Vec::new();

        // All RFC documents begins like so:
        // `Some Research Organization (SRO)                          J. Fabulousity`
        // This constant represents the minimum amount of space between the left
        // and right side. 10 is arbitrarily chosen.
        const SPACE_THRESHOLD: usize = 10;

        for element in elements {
            match element {
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

        for element in &left_column[1..] {
            match element {
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

    fn parse_sections(&mut self, elements: &[Phase1Element]) -> Result<Vec<Section>, String> {
        let mut sections = Vec::new();

        let mut current_section = Section {
            id: None,
            level: 0,
            title: Box::<str>::default(),
            elements: Vec::new(),
        };

        let add_line_to_paragraph = |section: &mut Section, line: &str| -> bool {
            let line_depth = line.chars().take_while(|c| *c == ' ').count() as u32;

            if let Some(Element::Paragraph(depth, ref mut para)) = section.elements.last_mut() {
                if let Some(ParagraphElement::Text(ref mut text)) = para.last_mut() {
                    if line_depth != *depth {
                        section.elements.push(Element::Paragraph(
                            line_depth,
                            vec![ParagraphElement::Text(line.trim_start().to_owned())],
                        ));
                        return true
                    }

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
                } else {
                    para.push(ParagraphElement::Text(line.trim_start().to_owned()));
                }
            } else {
                section.elements.push(Element::Paragraph(
                    line_depth,
                    vec![ParagraphElement::Text(line.trim_start().to_owned())],
                ));
            }

            true
        };

        let add_to_paragraph = |section: &mut Section, element| {
            if let Some(Element::Paragraph(_, ref mut para)) = section.elements.last_mut() {
                Ok(para.push(element))
            } else {
                eprintln!("{:?}", section);
                eprintln!("{:?}", element);
                Err("attempted to add non-text element to empty paragraph".to_string())
                // section.elements.push(
                //     Element::Paragraph(vec![element])
                // );
            }
        };

        // Criteria to start a new paragraph:
        // The section is empty, OR the very last element of the
        // section is empty (i.e. there was an empty line before)
        let can_start_new_paragraph = |section: &Section| {
            section
                .elements
                .last()
                .map(|el| match el {
                    Element::Paragraph(_, para) => {
                        para.is_empty()
                    }
                    _ => false,
                })
                .unwrap_or(true)
        };

        for element in elements {
            match element {
                Phase1Element::Text(text) => {
                    let mut lines = text.lines();

                    while let Some(line) = lines.next() {
                        if line.is_empty() {
                            if let Some(Element::Paragraph(depth, ref para)) =
                                current_section.elements.last()
                                && !para.is_empty()
                            {
                                current_section
                                    .elements
                                    .push(Element::Paragraph(*depth, Vec::new()));
                            }

                            continue;
                        }

                        if !line.starts_with(' ') && can_start_new_paragraph(&current_section) {
                            if !(current_section.title.is_empty()
                                && current_section.elements.len() == 0)
                            {
                                // Flush section
                                sections.push(std::mem::take(&mut current_section));
                            }
                            current_section.title = line.trim_end().to_owned().into_boxed_str();
                        } else {
                            add_line_to_paragraph(&mut current_section, line);
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

                Phase1Element::Anchor(id, text) => add_to_paragraph(
                    &mut current_section,
                    ParagraphElement::Anchor(id.clone(), text.clone()),
                )?,

                Phase1Element::CrossReference(href, title) => add_to_paragraph(
                    &mut current_section,
                    ParagraphElement::CrossReference(href.clone(), title.clone()),
                )?,
                Phase1Element::DocReference(href, title) => add_to_paragraph(
                    &mut current_section,
                    ParagraphElement::DocReference(href.clone(), title.clone()),
                )?,
                Phase1Element::SelfReference(href, title) => add_to_paragraph(
                    &mut current_section,
                    ParagraphElement::SelfReference(href.clone(), title.clone()),
                )?,
                Phase1Element::ExtReference(href, title) => add_to_paragraph(
                    &mut current_section,
                    ParagraphElement::ExtReference(href.clone(), title.clone()),
                )?,
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
