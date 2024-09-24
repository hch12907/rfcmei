
pub struct Phase2Document {
    start_info: StartInfo,
    sections: Vec<Section>,
}

pub struct StartInfo {
    date: Box<str>,
    category: Box<str>,
    issn: Box<str>,
    authors: Vec<Box<str>>,
}

pub struct Section {
    id: Box<str>,
    title: Box<str>,
    elements: Vec<Element>,
}

pub(super) enum Element {
    Section(),
    Paragraph(),
    OrderedList(),
    UnorderedList(), // note: may start with 'o' or '-'
    DocReference(), // e.g. [RFC1234]
    CrossReference(), // e.g. Section 3 of [RFC1234]
    SelfReference(), // e.g. Section 3
    AsciiArt(),
    Keyword(), // e.g. MAY, SHOULD, ...
    AuthorsList(),
}

// figuring out whether next section of the text is part of current paragraph
// - if len(first word in next sect) + len(last line) + 1 > 74
// - if !ends_with(last line, '.') and first_word(next sect) is in lowercase
//
//
// figuring out whether the following section is graphical:
// - if extensive amount of ASCII graphical character (-+/_:*) is involved
// - if the whitespace in between characters is excessive (>5 between chars)
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
