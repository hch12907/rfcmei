//! Phase 5 only adds some flavour to the document.

use std::sync::LazyLock;

use regex::Regex;

use crate::document::phase2::{LineMetadata, LineMetadataKind};
use crate::document::phase4::Element;

use super::phase2::Line;
use super::phase3::{OrderedListStyle, UnorderedListStyle};
use super::phase4::Phase4Document;


#[derive(Debug, Clone)]
pub struct Phase5Document {
    document: Phase4Document,
}

impl Phase5Document {
    pub fn from_phase4(phase4: Phase4Document) -> Self {
        let mut this = Self {
            document: phase4
        };

        this.mark_keywords();

        this
    }

    pub fn print(&self) -> String {
        let mut result = String::with_capacity(65536);

        fn print_element(element: &Element, output: &mut String) {
            let print_line = |space, line: &Line, output: &mut String| {
                output.push_str(space);

                let max_column = line.text.chars().count();

                for (i, ch) in line.text.chars().enumerate() {
                    let start_meta = line.metadata
                        .iter()
                        .find(|meta| meta.column == i as u32);
                    let end_meta = line.metadata
                        .iter()
                        .find(|meta| {
                            let end = (meta.column + meta.length)
                                .min(max_column as u32 - 1);
                            end == i as u32
                        });

                    match end_meta {
                        Some(LineMetadata {
                            column: _,
                            length: _,
                            kind: LineMetadataKind::Keyword,
                        }) => {
                            output.push_str("</strong>")
                        },

                        Some(LineMetadata {
                            column: _,
                            length: _,
                            kind: LineMetadataKind::Anchor(_),
                        }) => {
                            output.push_str("</span>")
                        },

                        Some(LineMetadata {
                            column: _,
                            length: _,
                            kind: LineMetadataKind::Reference(_),
                        }) => {
                            output.push_str("</a>")
                        },

                        None => (),
                    };

                    match start_meta {
                        Some(LineMetadata {
                            column: _,
                            length: _,
                            kind: LineMetadataKind::Keyword,
                        }) => {
                            output.push_str("<strong>");
                        },

                        Some(LineMetadata {
                            column: _,
                            length: _,
                            kind: LineMetadataKind::Anchor(id),
                        }) => {
                            output.push_str(&format!("<span id=\"{}\">", id));
                        },

                        Some(LineMetadata {
                            column: _,
                            length: _,
                            kind: LineMetadataKind::Reference(r),
                        }) => {
                            output.push_str(&format!("<a href=\"{}\">", r));
                        },

                        None => (),
                    };

                    match ch {
                        '&' => output.push_str("&amp;"),
                        '<' => output.push_str("&lt;"),
                        '>' => output.push_str("&gt;"),
                        _ => output.push(ch),
                    }
                }

                if let Some(connector) = line.connector {
                    output.push(connector);
                }
            };

            match element {
                Element::Paragraph {
                    lines,
                    depth,
                    hanging,
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
                        print_line(&space, line, output);
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
                        output.push_str(&format!("<dt>{}</dt>\n", term));
                        output.push_str("<dd>");
                        for line in definition {
                            print_line("", line, output);
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

        result.push_str(&format!("<h1>{}</h1>", &self.document.title));

        for section in &self.document.sections {
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
                section.id.as_deref().unwrap_or("").trim_start_matches('#'),
            ));

            for element in &section.elements {
                print_element(element, &mut result);
            }
        }

        result
    }

    fn mark_keywords(&mut self) {
        static KEYWORDS_REGEX: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new(
                "(MUST NOT|MUST|REQUIRED|SHALL NOT|SHALL|SHOULD NOT|SHOULD|NOT RECOMMENDED|RECOMMENDED|MAY|OPTIONAL)+"
            ).unwrap()
        });

        fn mark_keywords_in_element(element: &mut Element) {
            match element {
                Element::Paragraph { lines, .. } => {
                    for line in lines {
                        for keyword in KEYWORDS_REGEX.find_iter(&line.text) {
                            line.metadata.push(LineMetadata {
                                column: keyword.start() as u32,
                                length: keyword.len() as u32,
                                kind: LineMetadataKind::Keyword,
                            });

                            line.metadata
                                .sort_by(|meta1, meta2| meta1.column.cmp(&meta2.column));
                        }
                    }
                },
                Element::DefinitionList { definitions, .. } => {
                    for def in definitions {
                        for line in &mut def.1 {
                            for keyword in KEYWORDS_REGEX.find_iter(&line.text) {
                                line.metadata.push(LineMetadata {
                                    column: keyword.start() as u32,
                                    length: keyword.len() as u32,
                                    kind: LineMetadataKind::Keyword,
                                });
                            }

                            line.metadata
                                .sort_by(|meta1, meta2| meta1.column.cmp(&meta2.column));
                        }
                    }
                },
                Element::OrderedList { items, .. } => {
                    for item in items {
                        mark_keywords_in_element(&mut item.1);
                    }
                },
                Element::UnorderedList { items, .. } => {
                    for item in items {
                        mark_keywords_in_element(&mut item.1);
                    }
                },
                Element::Table { cells: _, .. } => todo!(),
            }
        }

        for section in &mut self.document.sections {
            for element in &mut section.elements {
                mark_keywords_in_element(element);
            }
        }
    }
}
