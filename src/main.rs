use parser::parser;

struct LexingState {
    pub file: String,
    pub line: usize,
    pub col: usize
}

#[derive(Clone)]
#[repr(u32)]
enum NodeKind {
    FunctionToken
}

#[derive(Clone)]
struct Span {
    file: String, 
    start: (usize, usize),
    end: (usize, usize)
}

#[derive(Clone)]
struct Node {
    kind: NodeKind,
    span: Span
}

fn make_lit_lexeme<'a>(state: &'a mut LexingState, text: &'_ str, kind: NodeKind) -> Node {
    let start = (state.line, state.col);
    state.col += text.len();
    Node {
        kind,
        span: Span {
            file: state.file.clone(),
            start,
            end: (state.line, state.col),
        }
    }
}

parser! {
    State(LexingState),
    Output(Node),
    FunctionToken => Literal("function" |state, text| make_lit_lexeme(state, text, NodeKind::FunctionToken)),
}

fn main() {
    let _ = create_parsing_engine().unwrap();
}
