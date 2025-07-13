#[derive(Default)]
struct Empty();

struct Expr {
    terms: Vec<Box<Node>>,
}

struct Children(Vec<Node>);

parser_macro::parser! {
    Start(Expr),
    State(Empty = Empty()),
    Expr => Rule(Children,
        Term Expr
    ),
    Term => Rule(Children,
        Literal Term | LeftParen Expr RightParen Term | Null
    ),
    Literal => Regex(usize, "[0-9]*" |_, text: &str| {
        Node::Literal(text.parse().unwrap())
    }),
    Null => Literal(Empty, ""),
    Multiply => Literal(Empty, "*"),
    LeftParen => Literal(Empty, "("),
    RightParen => Literal(Empty, ")"),
}

#[test]
fn parse_expr() {}
