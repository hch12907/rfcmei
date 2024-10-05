//! In Phase 3, the blocks of text are analysed even more, and finer classification
//! may be given to them, for example they could be definition lists or tables.
//! During the analysis, some blocks of text that were misanalysed previously could be
//! corrected.

use super::phase2::{
    Element as Phase2Element, OrderedListStyle, ParagraphElement, Phase2Document, StartInfo,
    UnorderedListStyle,
};

pub struct Phase3Document {
    start_info: StartInfo,
    title: Box<str>,
    sections: Vec<Section>,
}

pub struct Section {
    id: Option<Box<str>>, // An optional fragment-identifier for the section.
    level: usize,         // Level of the section, starts from 0 as the outermost level
    title: Box<str>,
    elements: Vec<Element>,
}

pub enum Element {
    Paragraph {
        depth: u32,
        hanging: bool,
        elements: Vec<ParagraphElement>,
    },
    Preformatted {
        depth: u32,
        elements: Vec<ParagraphElement>,
    },
    DefinitionList {
        depth: u32,
        definitions: Vec<(String, Vec<ParagraphElement>)>,
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
        heading: Vec<String>,
        cells: Vec<Vec<Self>>,
    },
}

impl Element {
    fn from_phase2(element: Phase2Element) -> Self {
        match element {
            Phase2Element::Paragraph {
                depth,
                hanging,
                elements,
            } => Element::Paragraph {
                depth,
                hanging,
                elements,
            },

            Phase2Element::OrderedList {
                depth,
                style,
                items,
            } => Element::OrderedList {
                depth,
                style,
                items: items
                    .into_iter()
                    .map(|(marker, item)| (marker, Self::from_phase2(item)))
                    .collect(),
            },

            Phase2Element::UnorderedList {
                depth,
                style,
                items,
            } => Element::UnorderedList {
                depth,
                style,
                items: items
                    .into_iter()
                    .map(|(marker, item)| (marker, Self::from_phase2(item)))
                    .collect(),
            },
            Phase2Element::Preformatted { depth, elements } => {
                Element::Preformatted { depth, elements }
            }
        }
    }
}

impl Phase3Document {
    pub fn from_phase2(document: Phase2Document) -> Result<Phase3Document, String> {
        let (start_info, title, p2_sections) = document.into_raw_parts();

        let sections = p2_sections
            .into_iter()
            .map(|section| {
                let (id, level, title, p2_elements) = section.into_raw_parts();

                let elements = p2_elements.into_iter().map(Element::from_phase2).collect();

                Section {
                    id,
                    level,
                    title,
                    elements,
                }
            })
            .collect();

        let mut this = Self {
            start_info,
            title,
            sections,
        };

        this.combine_paragraphs();

        Ok(this)
    }

    pub fn print(&self) -> String {
        let mut result = String::with_capacity(65536);

        fn print_element(element: &Element, outer_depth: u32, output: &mut String) {
            match element {
                Element::Paragraph { elements, depth, hanging, .. } => {
                    output.push_str("<p>");
                    output.push_str(&format!("<!--depth={}-->", depth));
                    if *hanging { output.push_str("<!--hanging-->"); }
                    for para_el in elements {
                        para_el.print_paragraph_element(0, output);
                    }
                    output.push_str("</p>\n");
                }

                Element::OrderedList {
                    depth,
                    items,
                    style,
                    ..
                } => {
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
                    print_element(&items[0].1, outer_depth + *depth, output);
                    for (marker, item) in &items[1..] {
                        if marker.is_some() {
                            output.push_str("</li>");
                            output.push_str("<li>");
                        }
                        print_element(item, outer_depth + *depth, output);
                    }
                    output.push_str("</li>");
                    output.push_str("</ol>\n");
                }

                Element::UnorderedList { depth, items, .. } => {
                    output.push_str("<ul>");
                    output.push_str("<li>");
                    print_element(&items[0].1, outer_depth + *depth, output);
                    for (marker, item) in &items[1..] {
                        if *marker {
                            output.push_str("</li>");
                            output.push_str("<li>");
                        }
                        print_element(item, outer_depth + *depth, output);
                    }
                    output.push_str("</li>");
                    output.push_str("</ul>\n");
                }
                Element::Preformatted { depth, elements } => {
                    output.push_str("<pre>");
                    let mut is_partial = false;
                    for para_el in elements.iter() {
                        let depth = if is_partial { 0 } else { *depth };
                        para_el.print_paragraph_element(depth, output);
                        is_partial = !matches!(para_el, ParagraphElement::Text(text)
                            if text.ends_with('\n'));
                    }
                    output.push_str("</pre>\n");
                }
                Element::DefinitionList { depth, definitions } => todo!(),
                Element::Table {
                    depth,
                    heading,
                    cells,
                } => todo!(),
            }
        }

        for section in &self.sections {
            if section
                .title
                .to_ascii_lowercase()
                .contains("table of contents")
            {
                continue;
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

    fn combine_paragraphs(&mut self) {
        // This only handles the very basic paragraph-paragraph combination.
        // TODO: Add support for elements inside (un)ordered lists
        for section in &mut self.sections {
            for i in 0..section.elements.len().saturating_sub(1) {
                let Element::Paragraph {
                    depth: depth_1st,
                    elements: para_1st,
                    hanging: hanging_1st,
                } = &section.elements[i]
                else {
                    continue;
                };

                let Element::Paragraph {
                    depth: depth_2nd,
                    elements: para_2nd,
                    ..
                } = &section.elements[i + 1]
                else {
                    continue;
                };

                if (*depth_1st != *depth_2nd) && (*hanging_1st && *depth_1st + 3 != *depth_2nd) {
                    continue;
                }

                let ends_improperly = |para: &ParagraphElement| {
                    let last_word = if let ParagraphElement::Text(text) = para {
                        text.rsplit(' ').next()
                    } else {
                        return false;
                    };

                    let Some(last_word) = last_word else {
                        return false;
                    };

                    if last_word.ends_with(&['.', ':']) {
                        return false;
                    }

                    // Use bytes and leave non-ASCII paragraphs alone.
                    last_word
                        .bytes()
                        .all(|c| c.is_ascii_alphabetic() || c == b'-')
                };

                if para_1st.last().map(ends_improperly) != Some(true) {
                    continue;
                }

                let starts_improperly = |para: &ParagraphElement| {
                    let first_word = if let ParagraphElement::Text(text) = para {
                        text.split(' ').next()
                    } else {
                        return false;
                    };

                    let Some(last_word) = first_word else {
                        return false;
                    };

                    if last_word.starts_with(&['(']) {
                        return true;
                    }

                    // Use bytes and leave non-ASCII paragraphs alone.
                    last_word
                        .bytes()
                        .all(|c| c.is_ascii_lowercase() || c == b'-')
                };

                if para_2nd.first().map(starts_improperly) != Some(true) {
                    continue;
                }

                let para_2nd =
                    if let Element::Paragraph { elements, .. } = &mut section.elements[i + 1] {
                        std::mem::take(elements)
                    } else {
                        unreachable!()
                    };

                let Element::Paragraph {
                    elements: para_1st, ..
                } = &mut section.elements[i]
                else {
                    unreachable!()
                };

                if let Some(ParagraphElement::Text(text_1st)) = para_1st.last_mut()
                    && let ParagraphElement::Text(text_2nd) = &para_2nd[0]
                {
                    text_1st.push(' ');
                    text_1st.push_str(&text_2nd);
                }

                para_1st.extend_from_slice(&para_2nd[1..]);
            }

            section.elements.retain(|element| {
                !matches!(
                    element,
                    Element::Paragraph { elements, .. }
                        if elements.is_empty()
                )
            });
        }
    }

    fn find_double_column_tables(&mut self) {
        
    }
}
