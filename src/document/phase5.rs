//! Phase 5 only adds some flavour to the document.

use std::sync::LazyLock;

use regex::Regex;

use crate::document::phase2::{LineMetadata, LineMetadataKind, StartInfo};
use crate::document::phase4::Element;

use super::phase2::Line;
use super::phase3::OrderedListStyle;
use super::phase4::Phase4Document;


macro_rules! html_template {
    ($($key:ident = $val:ident),*) => {
        format!(
r##"
<!DOCTYPE html>
<html>
    <head>
        <title>{rfc} - {title} [Prettified]</title>
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <meta charset="UTF-8" />
        <style>
            body {{
                font-family: Helvetica, Roboto, Arial, sans-serif;
                line-height: 1.6em;
                background-color: #fbfbf9;
                max-width: 700px;
                margin: auto;
            }}
            h1 {{
                text-align: center;
                padding: 0.5em 0em;
                margin: 2em;
                font-size: 1.8em;
                line-height: 1em;
            }}
            h2 {{
                padding: 0.5em 0em;
                font-size: 1.6em;
            }}
            h3 {{
                padding: 0.5em 0em;
                font-size: 1.4em;
            }}
            h4 {{
                padding: 0.5em 0em;
                font-size: 1.3em;
            }}
            h5 {{
                font-size: 1.2em;
            }}
            h6 {{
                font-size: 1.15em;
            }}
            dl dt {{
                width: 8em;
                float: left;
                clear: left;
                padding: 0.5em 0 0 0.5em;
            }}
            dl dd {{
                margin-left: 8.5em;
                padding: 0.5em 0 0.5em 0.5em;
            }}
            pre {{
                line-height: 1.25em;
            }}
            .author {{
                display: block;
                float: left;
                margin: 0;
                text-align: center;
            }}
            .author-org {{
                font-style: italic;
            }}
            .obsoletes {{
                float: left;
            }}
            .indent-1 {{
                padding-left: 1.5rem;
            }}
            .indent-2 {{
                padding-left: 3rem;
            }}
            .indent-3 {{
                padding-left: 4.5rem;
            }}
            .indent-4 {{
                padding-left: 6rem;
            }}
            .indent-5 {{
                padding-left: 7.5rem;
            }}
            .indent-6,
            .indent-7,
            .indent-8,
            .indent-9,
            .indent-10 {{
                padding-left: 9rem;
            }}
            .start-info {{
                font-size: 0.9em;
                padding-bottom: 5em;
            }}
            .start-info dt,
            .start-info dd {{
                padding: 0;
            }}
            .hanging {{
                padding-left: 1.5rem;
                text-indent: -1.5rem;
            }}
        </style>
    </head>
    <body>
        <article>
{body}
        </article>
    </body>
</html>
"##, $($key=$val,)*)
    };
}

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

                    let approaching_end = end_meta.is_some() && max_column - 1 == i;
                    if approaching_end {
                        match ch {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            _ => output.push(ch),
                        }
                    }

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

                    if !approaching_end {
                        match ch {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            _ => output.push(ch),
                        }
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
                    preformatted,
                } => {
                    let is_hanging = 'block: {
                        let first_depth = lines.first()
                            .map(|line| line.text.chars().take_while(|c| *c == ' ').count())
                            .unwrap_or(0);
                        let next_depth = lines.iter()
                            .nth(1)
                            .map(|line| line.text.chars().take_while(|c| *c == ' ').count())
                            .unwrap_or(0);

                        if first_depth >= next_depth {
                            break 'block false;
                        }

                        let mut same_depth = true;
                        for line in &lines[2..] {
                            let depth = line.text.chars().take_while(|c| *c == ' ').count();
                            same_depth &= depth == next_depth;
                        }

                        same_depth
                    };

                    let depth_class = depth.saturating_sub(3) / 3;
                    let class = if depth_class > 0 {
                        &format!("indent-{}", depth_class)
                    } else {
                        ""
                    };
                    let class = if is_hanging {
                        &format!("hanging {}", class)
                    } else {
                        class
                    };

                    if *preformatted {
                        output.push_str("<pre>");
                    } else {
                        if class.is_empty() {
                            output.push_str("<p>");
                        } else {
                            output.push_str(&format!("<p class=\"{}\">", class.trim()));
                        }
                    }

                    let space = if *preformatted {
                        &" ".repeat(*depth as usize)
                    } else {
                        ""
                    };

                    for (i, line) in lines.iter().enumerate() {
                        print_line(&space, line, output);

                        if !*preformatted
                            && let Some(next_line) = lines.get(i + 1)
                        {
                            let depth_now = line.text.chars().take_while(|c| *c == ' ').count();
                            let depth_next = next_line.text.chars().take_while(|c| *c == ' ').count();

                            if depth_now != depth_next {
                                output.push_str("<br>");
                            }
                        }
                    }

                    if *preformatted {
                        output.push_str("</pre>");
                    } else {
                        output.push_str("</p>");
                    }
                }

                Element::OrderedList { depth, items, style, .. } => {
                    let depth_class = depth.saturating_sub(3) / 3;
                    let class = if depth_class > 0 {
                        &format!("class=\"indent-{}\"", depth_class)
                    } else {
                        ""
                    };

                    output.push_str(&format!(
                        "<ol {} type=\"{}\" start=\"{}\">",
                        class,
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

                Element::UnorderedList { depth, items, .. } => {
                    let depth_class = depth.saturating_sub(3) / 3;
                    let class = if depth_class > 0 {
                        &format!("class=\"indent-{}\"", depth_class)
                    } else {
                        ""
                    };

                    output.push_str(&format!("<ul {}>", class));
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
                Element::DefinitionList { depth, definitions, .. } => {
                    let depth_class = depth.saturating_sub(3) / 3;
                    let class = if depth_class > 0 {
                        &format!("class=\"indent-{}\"", depth_class)
                    } else {
                        ""
                    };

                    output.push_str(&format!("<dl {}>", class));
                    for (term, definition) in definitions {
                        output.push_str("<dt>");
                        print_line("", term, output);
                        output.push_str("</dt>");
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

        let StartInfo { stream, rfc, obsoletes, updates, date, category, others, authors } = &self.document.start_info;
        result.push_str("<dl class=\"start-info\">");
        result.push_str(&format!("<dt>Stream:</dt><dd>{}</dd>", stream));
        result.push_str(&format!("<dt>RFC:</dt><dd>{}</dd>", rfc));
        if !obsoletes.is_empty() {
            result.push_str(&format!("<dt>Obsoletes:</dt><dd>"));
            for (rfc, name) in obsoletes {
                result.push_str(&format!("<a href=\"./rfc{}\">{}</a>, ", rfc, name));
            }
            result.pop();
            result.pop();
            result.push_str("</dd>");
        }
        if !updates.is_empty() {
            result.push_str(&format!("<dt>Updates:</dt><dd>"));
            for (rfc, name) in updates {
                result.push_str(&format!("<a href=\"./rfc{}\">{}</a>, ", rfc, name));
            }
            result.pop();
            result.pop();
            result.push_str("</dd>");
        }
        result.push_str(&format!("<dt>Category:</dt><dd>{}</dd>", category));
        result.push_str(&format!("<dt>Date:</dt><dd>{}</dd>", date));
        for (term, def) in others {
            result.push_str(&format!("<dt>{}:</dt><dd>{}</dd>", term, def));
        }
        result.push_str(&format!("<dt>Authors:</dt><div class=\"authors\">"));
        for (name, org) in authors {
            result.push_str(&format!("<dd class=\"author\"><div class=\"author-name\">{}</div><div class=\"author-org\">{}</div></dd>", name, org));
        }
        result.push_str("</div>");
        result.push_str("</dl>");

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

        let rfc = &self.document.start_info.rfc;
        let title = &self.document.title;
        html_template!(body=result, title=title, rfc=rfc)
    }

    fn mark_keywords(&mut self) {
        static KEYWORDS_REGEX: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new(
                r"\b(MUST NOT|MUST|REQUIRED|SHALL NOT|SHALL|SHOULD NOT|SHOULD|NOT RECOMMENDED|RECOMMENDED|MAY|OPTIONAL)\b"
            ).unwrap()
        });

        fn mark_keywords_in_element(element: &mut Element) {
            match element {
                Element::Paragraph { preformatted, lines, .. } => {
                    if *preformatted {
                        return                        
                    }

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

