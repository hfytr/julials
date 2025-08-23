use std::io::Read;

use julials::parser::{make_parser, NodeKind, LexingState};

fn main() {
    let mut engine = make_parser().unwrap();
    let mut f = std::fs::File::open("test.jl").unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    let mut state = LexingState {
        file: String::new(),
        line: 0,
        col: 0,
        errors: vec![],
    };
    let result = engine.parse(NodeKind::ExprList, &s, &mut state).unwrap();
    dbg!(result);
}
