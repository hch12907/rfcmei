//! Phase 5 removes the concept of lines from the document.

use std::sync::LazyLock;

use regex::Regex;

use super::phase2::{Line, LineMetadataKind, StartInfo};
use super::phase3::{OrderedListStyle, UnorderedListStyle};
use super::phase4::{Phase4Document, Element as Phase4Element, Section as Phase4Section};

#[derive(Debug, Clone)]
pub struct Phase5Document {
    pub start_info: StartInfo,
    pub title: Box<str>,
    pub sections: Vec<Section>,
}

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
                padding: 0.8em;
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
                clear: both;
                padding: 0.5em 0.5em 0 0;
            }}
            dl dd {{
                margin-left: 8.5em;
                padding: 0.5em 0 0.5em 0.5em;
            }}
            pre {{
                line-height: 1.25em;
                overflow: auto;
            }}
            .author {{
                display: block;
                float: left;
                margin: 0 0.5em 0.5em;
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

impl Phase5Document {
    pub fn from_phase4(phase4: Phase4Document) -> Self {
        let Phase4Document { start_info, title, sections } = phase4;

        let mut this = Self {
            start_info,
            title,
            sections: sections.into_iter().map(Section::from_phase4).collect(),
        };

        this.mark_keywords();

        this
    }

    pub fn print(&self) -> String {
        let mut result = String::new();

        fn print_inner_element(element: &InnerElement, output: &mut String) {
            match element {
                InnerElement::Break => {
                    output.push_str("<br>");
                },
                InnerElement::Text(text) => {
                    for ch in text.chars() {
                        match ch {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            _ => output.push(ch),
                        }
                    }
                },
                InnerElement::Anchor(id, inner_element) => {
                    output.push_str(&format!("<span id=\"{}\">", id));
                    print_inner_element(&inner_element, output);
                    output.push_str("</span>");
                },
                InnerElement::Reference(href, inner_element) => {
                    output.push_str(&format!("<a href=\"{}\">", href));
                    print_inner_element(&inner_element, output);
                    output.push_str("</a>");
                },
                InnerElement::Keyword(word) => {
                    output.push_str("<strong>");
                    output.push_str(word);
                    output.push_str("</strong>");
                },
            }
        }

        fn print_element(element: &Element, output: &mut String) {
            match element {
                Element::Paragraph {
                    elements,
                    hanging,
                    depth,
                    preformatted,
                } => {
                    let depth_class = depth.saturating_sub(3) / 3;
                    let class = if depth_class > 0 {
                        &format!("indent-{}", depth_class)
                    } else {
                        ""
                    };
                    let class = if *hanging {
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

                    for element in elements {
                        print_inner_element(element, output);
                    }

                    if *preformatted {
                        output.push_str("</pre>");
                    } else {
                        output.push_str("</p>");
                    }
                }

                Element::DefinitionList { depth, definitions } => {
                    let depth_class = depth.saturating_sub(3) / 3;
                    let class = if depth_class > 0 {
                        &format!("class=\"indent-{}\"", depth_class)
                    } else {
                        ""
                    };

                    output.push_str(&format!("<dl {}>", class));
                    for (term, definition) in definitions {
                        output.push_str("<dt>");
                        for element in term {
                            print_inner_element(element, output);
                        }
                        output.push_str("</dt>");
                        output.push_str("<dd>");
                        for element in definition {
                            print_inner_element(element, output);
                        }
                        output.push_str("</dd>\n");
                    }
                    output.push_str("</dl>\n");
                },

                Element::OrderedList { depth, style, items } => {
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
                },

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

                Element::Table {
                    depth: _,
                    headings,
                    cells,
                } => {
                    output.push_str("<table>");
                    for heading in headings {
                        output.push_str("<th>");
                        for element in heading {
                            print_inner_element(element, output);
                        }
                        output.push_str("</th>");
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

        let StartInfo {
            stream,
            rfc,
            obsoletes,
            updates,
            date,
            category,
            others,
            authors,
        } = &self.start_info;
        result.push_str("<dl class=\"start-info\">");
        result.push_str(&format!("<dt>Stream:</dt><dd>{}</dd>", stream));
        result.push_str(&format!("<dt>RFC:</dt><dd>{}</dd>", rfc));
        if !obsoletes.is_empty() {
            result.push_str("<dt>Obsoletes:</dt><dd>");
            for (rfc, name) in obsoletes {
                result.push_str(&format!("<a href=\"./rfc{}\">{}</a>, ", rfc, name));
            }
            result.pop();
            result.pop();
            result.push_str("</dd>");
        }
        if !updates.is_empty() {
            result.push_str("<dt>Updates:</dt><dd>");
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
        result.push_str("<dt>Authors:</dt><dd class=\"authors\">");
        for (name, org) in authors {
            result.push_str(&format!("<div class=\"author\"><div class=\"author-name\">{}</div><div class=\"author-org\">{}</div></div>", name, org));
        }
        result.push_str("</dd></dl>");

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
                section.id.as_deref().unwrap_or("").trim_start_matches('#'),
            ));

            for element in &section.elements {
                print_element(element, &mut result);
            }
        }

        let rfc = &self.start_info.rfc;
        let title = &self.title;
        html_template!(body = result, title = title, rfc = rfc)
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
                            let text = InnerElement::Text(
                                std::mem::take(&mut current)
                            );
                            buffer.push(text);
                        }
                    },
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
                Element::Paragraph { preformatted, elements, .. } => {
                    if *preformatted {
                        return;
                    }
                    mark_keywords_in_inner_elements(elements);
                },
                Element::DefinitionList { definitions, .. } => {
                    for (_term, def) in definitions {
                        mark_keywords_in_inner_elements(def);
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
                }
                Element::Table { depth: _, headings: _, cells: _ } => todo!(),
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
        let Phase4Section { id, level, title, elements } = phase4;

        Self {
            id,
            level,
            title,
            elements: elements.into_iter().map(Element::from_phase4).collect()
        }
    }
}

#[derive(Debug, Clone)]
pub enum Element {
    Paragraph {
        depth: u32,
        hanging: bool,
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
            Phase4Element::Paragraph { depth, preformatted, lines } => {
                let hanging = 'block: {
                    let first_depth = lines
                        .first()
                        .map(|line| line.text.chars().take_while(|c| *c == ' ').count())
                        .unwrap_or(0);
                    let next_depth = lines
                        .get(1)
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

                let elements = InnerElement::from_lines(preformatted, depth, lines);

                Element::Paragraph { depth, hanging, preformatted, elements }
            },
            Phase4Element::DefinitionList { depth, definitions } => {
                let definitions = definitions.into_iter()
                    .map(|(term, def)| {
                        let term = InnerElement::from_line(&term);
                        let def = InnerElement::from_lines(false, 0, def);
                        (term, def)
                    })
                    .collect();
                Element::DefinitionList { depth, definitions }
            },
            Phase4Element::OrderedList { depth, style, items } => {
                let items = items.into_iter().map(|item| {
                    (item.0, Self::from_phase4(item.1))
                }).collect();
                
                Element::OrderedList {
                    depth,
                    style,
                    items
                }
            },
            Phase4Element::UnorderedList { depth, style, items } => {
                let items = items.into_iter().map(|item| {
                    (item.0, Self::from_phase4(item.1))
                }).collect();
                
                Element::UnorderedList {
                    depth,
                    style,
                    items
                }
            },
            Phase4Element::Table { depth, headings, cells } => {
                let headings = headings
                    .iter()
                    .map(InnerElement::from_line)
                    .collect();
                let cells = cells
                    .into_iter()
                    .map(|row| row
                        .into_iter()
                        .map(Self::from_phase4)
                        .collect())
                    .collect();
                Element::Table { depth, headings, cells }
            },
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
                let depth_next =
                    next_line.text.chars().take_while(|c| *c == ' ').count();

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

    fn from_line_inner(
        line: &Line,
        start: u32,
        len: u32,
        ignore: Option<usize>
    ) -> Vec<Self> {
        let mut iter = line.text
            .as_bytes()
            .iter()
            .enumerate()
            .skip(start as usize)
            .take(len as usize);

        let mut result = Vec::new();
        let mut current = Vec::<u8>::new();

        while let Some((i, c)) = iter.next() {
            let metadata_idx = line.metadata.iter().position(|meta| meta.column as usize == i);

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
                        let mut inner = Self::from_line_inner(line, metadata.column, metadata.length, metadata_idx);
                        assert!(inner.len() == 1);

                        if metadata.length > 1 {
                            iter.nth((metadata.length - 2) as usize);
                        }

                        result.push(Self::Reference(
                            r.clone().into(),
                            Box::new(inner.pop().unwrap()))
                        )
                    },
                    LineMetadataKind::Anchor(a) => {
                        let mut inner = Self::from_line_inner(line, metadata.column, metadata.length, metadata_idx);
                        assert!(inner.len() == 1);

                        if metadata.length > 1 {
                            iter.nth((metadata.length - 2) as usize);
                        }

                        result.push(Self::Anchor(
                            a.clone().into(),
                            Box::new(inner.pop().unwrap()))
                        )
                    },
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
