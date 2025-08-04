#[derive(Default)]
struct Empty();

struct Expr {
    terms: Vec<Box<Node>>,
}

struct Children(Vec<Node>);

enum Node {
    Expr(Vec<Node>),
    Term(Vec<Node>),
    Literal(usize),
    Null,
    Multiply,
    LeftParen,
    RightParen,
}

parser_macro::parser! {
    Start(Expr),
    State(Empty = Empty()),
    Output(Node),
    Node::Expr => Rule(Term Expr),
    Node::Term => Rule(
        Literal Term | LeftParen Expr RightParen Term | Null
    ),
    Node::Literal => Regex("[0-9]*" |_, text: &str| {
        Node::Literal(text.parse().unwrap())
    }),
    Node::Null => Literal("" |_, _| {Node::Null } ),
    Node::Multiply => Literal("*" |_, _| Node::Multiply),
    Node::LeftParen => Literal("(" |_, _| Node::LeftParen),
    Node::RightParen => Literal(")" |_, _| Node::RightParen),
}

#[test]
fn parse_expr() {}
