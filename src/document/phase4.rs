//! In Phase 4, the blocks of text are analysed even more, and finer classification
//! may be given to them, for example they could be definition lists or tables.
//! During the analysis, some blocks of text that were misanalysed previously could be
//! corrected.

use std::convert::identity;
use std::sync::LazyLock;

use regex::Regex;

use super::phase2::{Line, StartInfo};
use super::phase3::{
    Element as Phase3Element, OrderedListStyle, Phase3Document, Section as Phase3Section,
    UnorderedListStyle,
};

#[derive(Debug, Clone)]
pub struct Phase4Document {
    pub start_info: StartInfo,
    pub title: Box<str>,
    pub sections: Vec<Section>,
}

#[derive(Debug, Clone)]
pub struct Section {
    pub id: Option<Box<str>>, // An optional fragment-identifier for the section.
    pub level: usize,         // Level of the section, starts from 0 as the outermost level
    pub title: Box<str>,
    pub elements: Vec<Element>,
}

#[derive(Debug, Clone)]
pub enum Element {
    Paragraph {
        depth: u32,
        preformatted: bool,
        lines: Vec<Line>,
    },
    DefinitionList {
        depth: u32,
        definitions: Vec<(Line, Vec<Line>)>,
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
    fn from_phase3(element: Phase3Element) -> Self {
        match element {
            Phase3Element::Paragraph {
                depth,
                preformatted,
                lines,
            } => Element::Paragraph {
                depth,
                preformatted,
                lines,
            },

            Phase3Element::OrderedList {
                depth,
                style,
                items,
            } => Element::OrderedList {
                depth,
                style,
                items: items
                    .into_iter()
                    .map(|(marker, item)| (marker, Self::from_phase3(item)))
                    .collect(),
            },

            Phase3Element::UnorderedList {
                depth,
                style,
                items,
            } => Element::UnorderedList {
                depth,
                style,
                items: items
                    .into_iter()
                    .map(|(marker, item)| (marker, Self::from_phase3(item)))
                    .collect(),
            },
        }
    }
}

impl Phase4Document {
    pub fn from_phase3(document: Phase3Document) -> Result<Phase4Document, String> {
        let Phase3Document {
            start_info,
            title,
            sections,
        } = document;

        let sections = sections
            .into_iter()
            .map(|section| {
                let Phase3Section {
                    id,
                    level,
                    title,
                    elements,
                } = section;

                let elements = elements.into_iter().map(Element::from_phase3).collect();

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
        this.fixup_broken_table();
        this.fixup_broken_author_list();
        this.fixup_broken_references();

        Ok(this)
    }

    pub fn print(&self) -> String {
        let mut result = String::with_capacity(65536);

        fn print_element(element: &Element, output: &mut String) {
            match element {
                Element::Paragraph {
                    lines,
                    depth,
                    preformatted,
                } => {
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
                        let text = &line
                            .text
                            .replace('&', "&amp;")
                            .replace('<', "&lt;")
                            .replace('>', "&gt;");
                        output.push_str(text);
                        if let Some(connector) = line.connector {
                            output.push(connector);
                        }
                    }

                    if *preformatted {
                        output.push_str("</pre>");
                    } else {
                        output.push_str("</p>");
                    }
                }

                Element::OrderedList { items, style, .. } => {
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
                Element::DefinitionList { definitions, .. } => {
                    output.push_str("<dl>");
                    for (term, definition) in definitions {
                        output.push_str("<dt>");
                        output.push_str(&term.text);
                        output.push_str("</dt>");

                        output.push_str("<dd>");
                        for line in definition {
                            let text = &line
                                .text
                                .replace('&', "&amp;")
                                .replace('<', "&lt;")
                                .replace('>', "&gt;");
                            output.push_str(&text);
                            if let Some(connector) = line.connector {
                                output.push(connector);
                            }
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

    fn combine_paragraphs(&mut self) {
        // This only handles the very basic paragraph-paragraph combination.
        // TODO: Add support for elements inside (un)ordered lists
        for section in &mut self.sections {
            let mut i = 0;
            while i < section.elements.len().saturating_sub(1) {
                let Element::Paragraph {
                    depth: depth_this,
                    preformatted: false,
                    lines: lines_this,
                } = &section.elements[i]
                else {
                    i += 1;
                    continue;
                };
                let depth_this = *depth_this;

                let Element::Paragraph {
                    depth: depth_next,
                    preformatted: false,
                    lines: lines_next,
                } = &section.elements[i + 1]
                else {
                    i += 1;
                    continue;
                };

                let true_depth_this = depth_this
                    + lines_this
                        .last()
                        .map(|line| line.text.chars().take_while(|c| *c == ' ').count() as u32)
                        .unwrap_or(0);

                if true_depth_this != *depth_next {
                    i += 1;
                    continue;
                }

                let Some(ends_improperly) = lines_this.last().map(|line| {
                    let last_word = line.text.rsplit(' ').next().unwrap_or("^");
                    last_word
                        .bytes()
                        .all(|c| c.is_ascii_alphanumeric() || c == b'-')
                        || last_word.ends_with(",")
                }) else {
                    i += 1;
                    continue;
                };

                let Some(starts_improperly) = lines_next.first().map(|line| {
                    let first_word = line.text.split(' ').next().unwrap_or("^");

                    if first_word.starts_with(['(']) {
                        return true;
                    }

                    let first_word = if first_word.ends_with([',', '.', ';', ')', ']']) {
                        &first_word[..first_word.len() - 1]
                    } else {
                        first_word
                    };

                    // Use bytes and leave non-ASCII paragraphs alone.
                    first_word
                        .bytes()
                        .all(|c| c.is_ascii_lowercase() || c == b'-')
                }) else {
                    i += 1;
                    continue;
                };

                if !(ends_improperly && starts_improperly) {
                    i += 1;
                    continue;
                }

                let Element::Paragraph {
                    lines: lines_next, ..
                } = section.elements.remove(i + 1)
                else {
                    unreachable!();
                };

                let Element::Paragraph {
                    lines: lines_this, ..
                } = &mut section.elements[i]
                else {
                    unreachable!()
                };

                lines_this.extend(
                    lines_next
                        .iter()
                        .map(|line| line.pad(true_depth_this - depth_this)),
                );
            }
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
        let separator = " ".repeat(SPACE_THRESHOLD);

        static REFERENCE_LEFT_COLUMN: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new(r"^[ ]*\[[A-Za-z0-9._\-]+\][ ]+").unwrap()
        });

        for section in &mut self.sections {
            // Reference sections will use a different logic to determine left and
            // right columns.
            let is_reference = section.title.ends_with("References");

            let mut skip_element = 0;

            while skip_element < section.elements.len() {
                let mut deflist_depth = 0;
                let mut deflist_defs = Vec::new();
                let mut second_column_start = 0;
                let mut starting_element = None;
                let mut ending_element = None;

                'this_element: for (i, element) in
                    section.elements.iter_mut().enumerate().skip(skip_element)
                {
                    let Element::Paragraph {
                        depth,
                        lines,
                        preformatted,
                        ..
                    } = element
                    else {
                        if starting_element.is_none() {
                            continue 'this_element;
                        } else {
                            break 'this_element;
                        }
                    };

                    let (depth, preformatted) = (*depth, *preformatted);

                    if starting_element.is_some() && deflist_depth != depth {
                        // Some deflist items can be misinterpreted as very deep paragraphs.
                        // If we see such paragraphs, correct them.
                        // Example pathological case:
                        //    Foobar                This is a paragraph in a deflist.
                        //
                        //                          And this paragraph here can be misinterpreted
                        //                          as a very deep paragraph instead of as a
                        //                          continuation of the first paragraph.
                        if deflist_depth + second_column_start as u32 == depth {
                            let mut lines = lines.clone();

                            for line in &mut lines {
                                line.text = " ".repeat(second_column_start) + &line.text;
                            }

                            *element = Element::Paragraph {
                                depth: deflist_depth,
                                preformatted,
                                lines,
                            };
                        } else {
                            break 'this_element;
                        }
                    }

                    let Element::Paragraph { lines, .. } = element else {
                        unreachable!()
                    };

                    for (j, line) in lines.iter().enumerate() {
                        if line.text.is_empty() {
                            ending_element = Some((i, j));
                            continue;
                        }

                        // If a line consists entirely of graphical chars (or whitespace),
                        // it's not a definition list.
                        if line.connector == Some('\n')
                            && line.text.trim().chars().all(|c| {
                                c == ' ' || (c.is_ascii_graphic() && !c.is_ascii_alphanumeric())
                            })
                        {
                            starting_element = None;
                            continue 'this_element;
                        }

                        if line.text.starts_with(' ') {
                            if starting_element.is_none() {
                                continue 'this_element;
                            } else if line.text.bytes().take_while(|c| *c == b' ').count()
                                != second_column_start
                            {
                                if ending_element.is_some() {
                                    ending_element = Some((i, j));
                                }
                                break 'this_element;
                            }
                        }

                        if is_reference
                            && let Some(found) = REFERENCE_LEFT_COLUMN.find(&line.text)
                        {
                            if starting_element.is_none() {
                                starting_element = Some((i, j));
                                second_column_start = found.len();
                                deflist_depth = depth;
                            } else {
                                ending_element = Some((i, j));
                            }
                        } else if !is_reference
                            && let Some(found) = line.text.rfind(&separator)
                            && line
                                .text
                                .as_bytes()
                                .get(found + separator.len())
                                .unwrap_or(&0)
                                .is_ascii_alphanumeric()
                            && found < 60
                        {
                            if starting_element.is_none() {
                                starting_element = Some((i, j));
                                second_column_start = found + separator.len();
                                deflist_depth = depth;
                            } else {
                                ending_element = Some((i, j));
                            }
                        } else if starting_element.is_some() {
                            let too_short = line.text.len() < second_column_start + 1;

                            let before_not_space = line
                                .text
                                .as_bytes()
                                .get(second_column_start - 1)
                                .map(|c| *c != b' ')
                                .unwrap_or(true);

                            let itself_not_space = line
                                .text
                                .as_bytes()
                                .get(second_column_start)
                                .map(|c| *c == b' ')
                                .unwrap_or(true);

                            if too_short || before_not_space || itself_not_space {
                                starting_element = None;
                                continue 'this_element;
                            }

                            ending_element = Some((i, j));
                        }
                    }

                    // If we've arrived here, the entire element is eligible. But
                    // the range is exclusive, so let's add 1 to it
                    if let Some((_, ref mut j)) = ending_element {
                        *j += 1;
                    }
                }

                let Some(starting_element) = starting_element else {
                    skip_element = section.elements.len();
                    continue;
                };
                let Some(ending_element) = ending_element else {
                    skip_element = section.elements.len();
                    continue;
                };

                let mut term = Line::new();
                let mut def = Vec::new();
                for (i, element) in section.elements[starting_element.0..=ending_element.0]
                    .iter()
                    .enumerate()
                {
                    let Element::Paragraph { lines, .. } = element else {
                        unreachable!()
                    };

                    let lines = if i == 0 {
                        &lines[starting_element.1..]
                    } else if i == ending_element.0 {
                        &lines[..ending_element.1]
                    } else {
                        &lines[..]
                    };

                    for line in lines {
                        if line.text.is_empty() {
                            continue;
                        }

                        let (_, mut new_term) = line.carve_out(..second_column_start);
                        new_term.trim_end();

                        if !new_term.text.trim_end().is_empty() {
                            if !term.text.is_empty() {
                                deflist_defs.push((term, std::mem::take(&mut def)));
                            }
                            term = new_term;
                        }

                        def.push(line.cut(second_column_start as u32).to_owned());
                    }
                }
                deflist_defs.push((term, def));

                let element = Element::DefinitionList {
                    depth: deflist_depth,
                    definitions: deflist_defs,
                };

                // Remove the original elements
                if starting_element.1 > 0 {
                    if let Element::Paragraph { lines, .. } =
                        &mut section.elements[starting_element.0]
                    {
                        lines.drain(starting_element.1..);
                    } else {
                        unreachable!();
                    }
                }
                if starting_element.0 != ending_element.0 && ending_element.1 > 0 {
                    if let Element::Paragraph { lines, .. } =
                        &mut section.elements[ending_element.0]
                    {
                        lines.drain(..ending_element.1);
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

    fn fixup_broken_table(&mut self) {
        for section in &mut self.sections {
            for element in &mut section.elements {
                let Element::Paragraph {
                    preformatted,
                    lines,
                    ..
                } = element
                else {
                    continue;
                };

                if !*preformatted {
                    continue;
                }

                let mut i = 0;
                while i < lines.len() {
                    if !lines[i].text.is_empty() {
                        i += 1;
                        continue;
                    }
                    if lines[i - 1].text.len() != lines[i + 1].text.len() {
                        i += 1;
                        continue;
                    }

                    if !lines[i - 1].text.contains('|') {
                        i += 1;
                        continue;
                    }

                    let matches = lines[i - 1]
                        .text
                        .chars()
                        .zip(lines[i + 1].text.chars())
                        .filter(|&(c1, c2)| c1 == '|' || c2 == '|')
                        .map(|(c1, c2)| c1 == c2)
                        .all(identity);

                    if matches {
                        lines.remove(i);
                        i += 1;
                    }
                }
            }
        }
    }

    fn fixup_broken_references(&mut self) {
        let sections = self
            .sections
            .iter_mut()
            .filter(|sect| sect.title.to_ascii_lowercase().ends_with("references"));

        for section in sections {
            for element in &mut section.elements {
                let Element::Paragraph { preformatted, .. } = element else {
                    continue;
                };

                *preformatted = false;
            }
        }
    }

    fn fixup_broken_author_list(&mut self) {
        let section = self.sections.iter_mut().find(|sect| {
            sect.title
                .to_ascii_lowercase()
                .ends_with("authors' addresses")
        });

        let Some(section) = section else { return };

        let mut new_lines = Vec::new();

        for element in &section.elements {
            let Element::Paragraph { lines, .. } = element else {
                continue;
            };

            new_lines.extend_from_slice(lines);
        }

        for line in &mut new_lines {
            line.connector = Some('\n');
        }

        section.elements = vec![Element::Paragraph {
            depth: 3,
            preformatted: true,
            lines: new_lines,
        }];
    }
}
