use std::io::Read;

use julials::ast::AST;
use julials::parser::{make_parser, NodeKind, ParseState};

fn main() {
    let mut engine = make_parser().unwrap();
    let mut f = std::fs::File::open("test.jl").unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    let mut ast = AST::default();
    ast.add_file("test.jl".into());
    let mut state = ParseState {
        file_ind: 0,
        errors: vec![],
        line: 0,
        col: 0,
        ast,
    };
    let result = engine.parse(NodeKind::ExprList, &s, &mut state).unwrap();
    dbg!(result);
}
