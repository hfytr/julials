use parser::parser;

struct LexingState {

}

#[repr(u32)]
enum NodeKind {
}

struct Node {
    kind: NodeKind,
    span: ((usize, usize), (usize, usize))
}

parser! {
    State(LexingState)
    Out()
}

fn main() {}
