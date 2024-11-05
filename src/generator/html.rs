use crate::document::phase2::StartInfo;
use crate::document::phase3::OrderedListStyle;
use crate::document::phase5::{Element, Indent, InnerElement};
use crate::document::Document;

use super::Generator;

pub struct Html;

macro_rules! html_template {
    ($($key:ident = $val:ident),*) => {
        format!(
r##"
<!DOCTYPE html>
<html>
    <head>
        <title>RFC {rfc} - {title} [Prettified]</title>
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <meta charset="UTF-8" />
        <style>
            body {{
                display: flex;
                font-family: Helvetica, Roboto, Arial, sans-serif;
                line-height: 1.6em;
                background-color: #fbfbf9;
                padding: 0.8em;
                flex-direction: row-reverse;
                justify-content: flex-end;
                max-width: 80%;
                margin: auto;
            }}
            article {{
                flex-grow: 2;
                width: 70%;
                overflow-y: auto;
                margin: 1em 2em 1em 0em;
            }}
            aside {{
                display: block;
                position: sticky;
                overflow-y: auto;
                align-self: flex-start;
                z-index: 1;
                background-color: #fbfbf9;
                opacity: 92%;
                padding: 1em;
                width: 30%;
                height: 100vh;
                top: 0;
            }}
            ol.table-of-contents,
            .table-of-contents ol {{
                counter-reset: item;
            }}
            .table-of-contents li {{
                display: block;
                margin: 0.2em 0 0.2em -1em;
            }}
            .table-of-contents li:before {{
                content: counters(item, ".")". ";
                counter-increment: item;
            }}
            .table-of-contents li.toc-appendix:before,
            .table-of-contents .toc-appendix li:before {{
                content: "";
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
            .indented {{
                text-indent: 1.5rem;
            }}
        </style>
    </head>
    <body>
        <aside>
{toc}
        </aside>
        <article>
{body}
        </article>
    </body>
</html>
"##, $($key=$val,)*)
    };
}

impl Generator for Html {
    fn name(&self) -> &'static str {
        "html"
    }

    fn generate(&self, document: &Document) -> String {
        let mut result = String::new();

        fn print_inner_element(element: &InnerElement, output: &mut String) {
            match element {
                InnerElement::Break => {
                    output.push_str("<br>");
                }
                InnerElement::Text(text) => {
                    for ch in text.chars() {
                        match ch {
                            '&' => output.push_str("&amp;"),
                            '<' => output.push_str("&lt;"),
                            '>' => output.push_str("&gt;"),
                            _ => output.push(ch),
                        }
                    }
                }
                InnerElement::Anchor(id, inner_element) => {
                    output.push_str(&format!("<span id=\"{}\">", id));
                    print_inner_element(&inner_element, output);
                    output.push_str("</span>");
                }
                InnerElement::Reference(href, inner_element) => {
                    output.push_str(&format!("<a href=\"{}\">", href));
                    print_inner_element(&inner_element, output);
                    output.push_str("</a>");
                }
                InnerElement::Keyword(word) => {
                    output.push_str("<strong>");
                    output.push_str(word);
                    output.push_str("</strong>");
                }
            }
        }

        fn print_element(element: &Element, output: &mut String) {
            match element {
                Element::Paragraph {
                    elements,
                    indentation,
                    depth,
                    preformatted,
                } => {
                    let depth_class = depth.saturating_sub(3) / 3;
                    let class = if depth_class > 0 {
                        &format!("indent-{}", depth_class)
                    } else {
                        ""
                    };
                    let class = if *indentation == Indent::Hanging {
                        &format!("hanging {}", class)
                    } else if *indentation == Indent::Indented {
                        &format!("indented {}", class)
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
                }

                Element::OrderedList {
                    depth,
                    style,
                    items,
                } => {
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
        } = &document.start_info;
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

        result.push_str(&format!("<h1>{}</h1>", document.title));

        let mut table_of_contents = Vec::new();

        for section in &document.sections {
            if section
                .title
                .to_ascii_lowercase()
                .contains("table of contents")
            {
                continue;
            }
            result.push_str(&format!(
                "<h{0} id=\"{2}\">{1}</h{0}>\n",
                section.level + 2,
                section.title,
                section.id.as_deref().unwrap_or("").trim_start_matches('#'),
            ));

            if section.id.is_some() {
                table_of_contents.push((
                    section.title.as_ref(),
                    section.level,
                    section.id.as_deref().unwrap_or("").trim_start_matches('#'),
                ));
            }

            for element in &section.elements {
                print_element(element, &mut result);
            }
        }

        let mut toc_html = String::new();
        toc_html.push_str("<ol class=\"table-of-contents\">");

        for (i, &(title, level, id)) in table_of_contents.iter().enumerate() {
            let next_level = table_of_contents
                .get(i + 1)
                .map(|&(_, level, _)| level)
                .unwrap_or(0);

            let is_appendix = title.contains("Appendix");
            let title = if let Some(split) = title.split_once(".  ")
                && !is_appendix
            {
                split.1
            } else {
                title
            };

            toc_html.push_str(&format!(
                "<li {}><a href=\"#{}\">{}</a>",
                if is_appendix { "class=\"toc-appendix\"" } else { "" },
                id,
                title
            ));

            if next_level > level {
                assert!(next_level - level == 1);
                toc_html.push_str("<ol>");
            } else if next_level == level {
                toc_html.push_str("</li>");
            } else if next_level < level {
                for _ in 0..level - next_level {
                    toc_html.push_str("</li></ol>");
                }
                toc_html.push_str("</li>");
            }
        }

        toc_html.push_str("</ol>");

        let rfc = document.start_info.rfc;
        let title = &document.title;

        html_template!(toc = toc_html, body = result, title = title, rfc = rfc)
    }
}
