#[derive(Debug)]

struct ParserState {}

parser_macro::parser! {
    State(ParserState) = ParserState {},
    Start(Identifier),
    Identifier => Regex(".*")
}

fn main() {
    let s = String::from("test");
    let mut s_slice = &s[0..];
    let engine = create_parsing_engine(&mut s_slice);
}
