//! In Phase 3, the blocks of text are analysed even more, and finer classification
//! may be given to them, for example they could be definition lists or tables.
//! During the analysis, some blocks of text that were misanalysed previously could be
//! corrected.

use super::phase2::{
    Element as Phase2Element, OrderedListStyle, ParagraphElement, Phase2Document, StartInfo,
    UnorderedListStyle,
};

#[derive(Debug, Clone)]
pub struct Phase3Document {
    start_info: StartInfo,
    title: Box<str>,
    sections: Vec<Section>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Section {
    id: Option<Box<str>>, // An optional fragment-identifier for the section.
    level: usize,         // Level of the section, starts from 0 as the outermost level
    title: Box<str>,
    elements: Vec<Element>,
}

#[derive(Debug, Clone, PartialEq)]
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
        headings: Vec<String>,
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
        this.find_double_column_deflist();

        Ok(this)
    }

    pub fn print(&self) -> String {
        let mut result = String::with_capacity(65536);

        fn print_element(element: &Element, output: &mut String) {
            match element {
                Element::Paragraph {
                    elements,
                    depth,
                    hanging,
                    ..
                } => {
                    output.push_str("<p>");
                    output.push_str(&format!("<!--depth={}-->", depth));
                    if *hanging {
                        output.push_str("<!--hanging-->");
                    }
                    for para_el in elements {
                        para_el.print_paragraph_element(0, output);
                    }
                    output.push_str("</p>\n");
                }

                Element::OrderedList {
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
                    print_element(&items[0].1, output);
                    for (marker, item) in &items[1..] {
                        if marker.is_some() {
                            output.push_str("</li>");
                            output.push_str("<li>");
                        }
                        print_element(item, output);
                    }
                    output.push_str("</li>");
                    output.push_str("</ol>\n");
                }

                Element::UnorderedList { items, .. } => {
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
                Element::DefinitionList { definitions, .. } => {
                    output.push_str("<dl>");
                    for (term, definition) in definitions {
                        output.push_str(&format!("<dt>{}</dt>\n", term));
                        output.push_str("<dd>");
                        for para_el in definition {
                            para_el.print_paragraph_element(0, output);
                        }
                        output.push_str("</dd>\n");
                    }
                    output.push_str("</dl>\n");
                }
                Element::Table {
                    depth: _,
                    headings,
                    cells,
                } => {
                    output.push_str("<table>");
                    for heading in headings {
                        output.push_str(&format!("<th>{}</th>\n", heading));
                    }
                    for row in cells {
                        output.push_str("<tr>");
                        for column in row {
                            output.push_str("<td>");
                            print_element(column, output);
                            output.push_str("</td>");
                        }
                        output.push_str("</tr>\n");
                    }
                    output.push_str("</table>\n");
                }
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
                print_element(element, &mut result);
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

                    if last_word.ends_with(['.', ':']) {
                        return false;
                    }

                    // Use bytes and leave non-ASCII paragraphs alone.
                    last_word
                        .bytes()
                        .all(|c| c.is_ascii_alphanumeric() || c == b'-')
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

                    if last_word.starts_with(['(']) {
                        return true;
                    }

                    let last_word = if last_word.ends_with([',', '.', ';']) {
                        &last_word[..last_word.len() - 1]
                    } else {
                        last_word
                    };

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
                    text_1st.push_str(text_2nd);
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

    fn find_double_column_deflist(&mut self) {
        // Find an unstyled definition list that looks like this:
        //   NAME                 Name of the person. The name shall be encoded in
        //                        UTF-8.
        //
        //   AGE                  Age of the person.
        //   ...

        const SPACE_THRESHOLD: usize = 3;
        let spaces = " ".repeat(SPACE_THRESHOLD);

        'this_section: for section in &mut self.sections {
            let mut deflist_depth = 0;
            let mut deflist_defs = Vec::new();

            let mut second_column_start = 0;
            let mut starting_element = None; // (element index, paragraph element index)
            let mut ending_element = None;

            'element: for (i, element) in section.elements.iter().enumerate() {
                match element {
                    Element::Paragraph {
                        depth,
                        hanging: _,
                        elements,
                    } => {
                        if starting_element.is_some() && deflist_depth != *depth {
                            break 'element;
                        }

                        if let ParagraphElement::Text(text) = elements.first().unwrap() {
                            if let Some(pos) = text.rfind(&spaces) {
                                if starting_element.is_none()
                                    && text
                                        .as_bytes()
                                        .get(pos + SPACE_THRESHOLD)
                                        .unwrap_or(&0)
                                        .is_ascii_alphabetic()
                                {
                                    starting_element = Some((i, 0));
                                    ending_element = starting_element;
                                    second_column_start = pos + SPACE_THRESHOLD;
                                    deflist_depth = *depth;
                                } else if starting_element.is_some() {
                                    ending_element = Some((i, 0));
                                }
                            } else if starting_element.is_some() {
                                ending_element = Some((i, 0));
                                
                                let before_not_space = text.as_bytes().get(second_column_start - 1)
                                    .map(|c| *c != b' ')
                                    .unwrap_or(true);

                                let not_alphabet = text.as_bytes().get(second_column_start)
                                    .map(|c| !c.is_ascii_alphanumeric())
                                    .unwrap_or(true);

                                if before_not_space || not_alphabet {
                                    starting_element = None;
                                    break 'element;
                                }
                            }
                        } else {
                            break 'element;
                        }
                    }
                    Element::Preformatted { depth, elements } => {
                        if starting_element.is_some() && deflist_depth != *depth {
                            break 'element;
                        }

                        let mut is_partial = false;

                        for (j, element) in elements.iter().enumerate() {
                            let ParagraphElement::Text(text) = &element else {
                                is_partial = true;
                                if starting_element.is_some() {
                                    ending_element = Some((i, j));
                                }
                                continue;
                            };

                            // If a line consists entirely of graphical chars (or whitespace),
                            // it's not a definition list.
                            if !is_partial && text.ends_with('\n') &&
                                text.trim().chars().all(|c| c == ' ' || (c.is_ascii_graphic() && !c.is_ascii_alphanumeric())){
                                starting_element = None;
                                continue 'element;
                            }

                            if is_partial {
                                ending_element = Some((i, j));
                                is_partial = !text.ends_with('\n');
                                continue;
                            }

                            if text.starts_with(' ') {
                                if starting_element.is_none() {
                                    continue;
                                } else if text.bytes().take_while(|c| *c == b' ').count()
                                    != second_column_start
                                {
                                    ending_element = Some((i, j));
                                    break 'element;
                                }
                            }

                            if let Some(pos) = text.rfind(&spaces) {
                                if starting_element.is_none()
                                    && text
                                        .as_bytes()
                                        .get(pos + SPACE_THRESHOLD)
                                        .unwrap_or(&0)
                                        .is_ascii_alphabetic()
                                {
                                    starting_element = Some((i, j));
                                    ending_element = starting_element;
                                    second_column_start = pos + SPACE_THRESHOLD;
                                    deflist_depth = *depth;
                                } else if starting_element.is_some() {
                                    ending_element = Some((i, j))
                                }
                            } else if starting_element.is_some() {
                                ending_element = Some((i, j));

                                let too_short = text.len() < second_column_start + SPACE_THRESHOLD;
                                let before_not_space = text.as_bytes().get(second_column_start - 1)
                                    .map(|c| *c != b' ')
                                    .unwrap_or(true);

                                let not_alphabet = text.as_bytes().get(second_column_start)
                                    .map(|c| !c.is_ascii_alphanumeric())
                                    .unwrap_or(true);

                                if too_short || before_not_space || not_alphabet {
                                    starting_element = None;
                                    break 'element;
                                }
                            }
                        }

                        // If we've arrived here, the entire element is eligible. But
                        // the range is exclusive, so let's add 1 to it
                        if let Some((_, ref mut j)) = ending_element {
                            *j += 1;
                        }
                    }
                    _ => continue 'this_section,
                }
            }

            let Some(starting_element) = starting_element else {
                continue;
            };
            let ending_element = ending_element.unwrap();

            let mut term = String::new();
            let mut def = Vec::new();
            for (i, element) in section.elements[starting_element.0..=ending_element.0]
                .iter()
                .enumerate()
            {
                let (elements, is_para) = match element {
                    Element::Paragraph { elements, .. } => (elements, true),
                    Element::Preformatted { elements, .. } => (elements, false),
                    _ => unreachable!(),
                };

                let elements = if i == 0 {
                    &elements[starting_element.1..]
                } else if i + 1 == ending_element.0 {
                    &elements[..ending_element.1]
                } else {
                    &elements[..]
                };

                if is_para {
                    if elements.is_empty() {
                        continue;
                    }

                    let ParagraphElement::Text(text) = elements.first().unwrap() else {
                        unreachable!()
                    };

                    let new_term = text[..second_column_start].trim_end().to_owned();

                    if !new_term.is_empty() {
                        if !term.is_empty() {
                            deflist_defs.push((term, std::mem::take(&mut def)));
                        }
                        term = new_term;
                    }

                    def.push(ParagraphElement::Text(
                        text[second_column_start..].trim_end().to_owned(),
                    ));
                    def.extend_from_slice(&elements[1..]);
                } else {
                    let mut is_partial = false;
                    for para_element in elements.iter() {
                        if let ParagraphElement::Text(text) = para_element {
                            if !is_partial {
                                let new_term = text[..second_column_start].trim_end().to_owned();

                                if !new_term.is_empty() {
                                    if !term.is_empty() {
                                        deflist_defs.push((term, std::mem::take(&mut def)));
                                    }
                                    term = new_term;
                                }

                                let mut text = text[second_column_start..].trim_end().to_owned();
                                text.push(' ');
                                def.push(ParagraphElement::Text(text));
                            } else {
                                def.push(para_element.clone());
                            }

                            is_partial = !text.ends_with('\n');
                        } else {
                            def.push(para_element.clone());
                        };
                    }
                }
            }

            deflist_defs.push((term, def));

            let element = Element::DefinitionList {
                depth: deflist_depth,
                definitions: deflist_defs,
            };

            // Remove the original elements
            if starting_element.1 > 0 {
                if let Element::Preformatted { elements, .. } =
                    &mut section.elements[starting_element.0]
                {
                    elements.drain(starting_element.1..);
                } else {
                    unreachable!();
                }
            }
            if starting_element.0 != ending_element.0 && ending_element.1 > 0 {
                if let Element::Preformatted { elements, .. } =
                    &mut section.elements[ending_element.0]
                {
                    elements.drain(..ending_element.1);
                } else {
                    unreachable!();
                }
            }
            if starting_element.0 == ending_element.0 {
                if starting_element.1 > 0 {
                    section.elements.insert(starting_element.0 + 1, element);
                } else {
                    section.elements.remove(starting_element.0);
                    section.elements.insert(starting_element.0, element);
                }
            } else if starting_element.1 > 0 {
                section
                    .elements
                    .drain(starting_element.0 + 1..ending_element.0);
                section.elements.insert(starting_element.0 + 1, element);
            } else {
                section.elements.drain(starting_element.0..ending_element.0);
                section.elements.insert(starting_element.0, element);
            }
        }
    }
}
