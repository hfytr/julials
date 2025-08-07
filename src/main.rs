#![feature(box_patterns)]

use std::fmt::Display;

#[derive(Default, Debug)]
struct Empty();

#[derive(Debug, Clone)]
enum Node {
    Expr(Vec<Box<Node>>),
    Term(Vec<Box<Node>>),
    Literal(usize),
    Multiply,
    Plus,
    LeftParen,
    RightParen,
}

impl Node {
    fn eval(&self) -> usize {
        match self {
            Node::Expr(terms) => terms.into_iter().map(|node| node.eval()).sum(),
            Node::Term(factors) => factors.into_iter().map(|node| node.eval()).product(),
            Node::Literal(val) => *val,
            _ => panic!("Cannot evaluate lexeme."),
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Expr(terms) => {
                f.write_str("(")?;
                for (i, term) in terms.iter().enumerate() {
                    term.fmt(f)?;
                    if i != terms.len() - 1 {
                        f.write_str(" + ")?;
                    }
                }
                f.write_str(")")?;
            }
            Node::Term(factors) => {
                for (i, factor) in factors.iter().enumerate() {
                    factor.fmt(f)?;
                    if i != factors.len() - 1 {
                        f.write_str(" * ")?;
                    }
                }
            }
            Node::Literal(val) => write!(f, "{val}")?,
            _ => panic!("Cannot display lexeme."),
        }
        Ok(())
    }
}

fn expr_node(children: Vec<Node>) -> Node {
    if let Result::Ok([term @ Node::Term(_), Node::Plus, mut expr @ Node::Expr(_)]) =
        <[_; 3]>::try_from(children)
        && let Node::Expr(ref mut components) = expr
    {
        components.push(Box::new(term));
        expr
    } else {
        panic!();
    }
}

fn term_node(children: Vec<Node>) -> Node {
    if let Result::Ok([factor, Node::Multiply, mut term @ Node::Term(_)]) =
        <[_; 3]>::try_from(children)
        && let Node::Term(ref mut components) = term
    {
        components.push(Box::new(factor));
        term
    } else {
        panic!();
    }
}

parser::parser! {
    Start(Expr),
    State(Empty = Empty()),
    Output(Node),
    Expr => Rule(
        Term Plus Expr |children| expr_node(children),
        Term |mut children| Node::Expr(vec![Box::new(children.pop().unwrap())])
    ),
    Term => Rule(
        Factor Multiply Term |children| term_node(children),
        Factor |mut children| Node::Term(vec![Box::new(children.pop().unwrap())])
    ),
    Factor => Rule(
        Literal,
        LeftParen Expr RightParen |mut children| children.swap_remove(1)
    ),
    Literal => Regex("[0-9]*" |_, text: &str| {
        Node::Literal(text.parse().unwrap())
    }),
    Multiply => Literal("*" |_, _| Node::Multiply),
    Plus => Literal("+" |_, _| Node::Plus),
    LeftParen => Literal("(" |_, _| Node::LeftParen),
    RightParen => Literal(")" |_, _| Node::RightParen),
}

fn main() {
    let s = String::from("1*7+(5+8)*(6+3*(5+7)+1)");
    let mut slice = s.as_str();
    let mut engine = create_parsing_engine(&mut slice).unwrap();
    let expr = engine.parse();
    let expr = expr.unwrap();
    dbg!(&expr);
    dbg!(expr.eval());
}
