# rfcmei

This tool prettifies RFC documents, which are typically (prior to RFC 8xxx)
published in a plain-text ASCII format, into a richer HTML document which can
then decorated with CSS.

Here is an example of [RFC 6887](https://www.rfc-editor.org/rfc/rfc6887) after
being prettified by the tool (the secret CSS sauce is **not** included):

![RFC 6887 prettified](./screenshot.png)

But please understand that **the tool is still in development and its output is not perfect**.
Thanks.

## Usage

```bash
# Building the program
cargo build --release

# Using it (requires the original RFC document to be in "HTMLized" format)
cargo run --release -- /path/to/rfc.html >prettified.html
```

## License

This tool is GPLv3-licensed.
