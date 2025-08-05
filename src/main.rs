#![feature(box_patterns)]

use std::fmt::Display;

#[derive(Default, Debug)]
struct Empty();

#[derive(Debug)]
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

fn expr_node(children: Vec<Box<Node>>) -> Box<Node> {
    if let Result::Ok([term @ box Node::Term(_), box Node::Plus, mut expr @ box Node::Expr(_)]) =
        <[_; 3]>::try_from(children)
    {
        if let box Node::Term(ref mut components) = expr {
            components.push(term);
        } else {
            panic!();
        }
        expr
    } else {
        panic!();
    }
}

fn term_node(children: Vec<Box<Node>>) -> Box<Node> {
    dbg!(&children);
    if let Result::Ok([factor, box Node::Multiply, mut term @ box Node::Term(_)]) =
        <[_; 3]>::try_from(children)
    {
        if let box Node::Term(ref mut components) = term {
            components.push(factor);
        } else {
            panic!();
        }
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
        Term |mut children| Box::new(Node::Expr(vec![children.pop().unwrap()]))
    ),
    Term => Rule(
        Factor Multiply Term |children| term_node(children),
        Factor |mut children| Box::new(Node::Term(vec![children.pop().unwrap()]))
    ),
    Factor => Rule(
        Literal,
        LeftParen Expr RightParen |mut children| children.swap_remove(1)
    ),
    Literal => Regex("[0-9]*" |_, text: &str| {
        Box::new(Node::Literal(text.parse().unwrap()))
    }),
    Multiply => Literal("*" |_, _| Box::new(Node::Multiply)),
    Plus => Literal("+" |_, _| Box::new(Node::Plus)),
    LeftParen => Literal("(" |_, _| Box::new(Node::LeftParen)),
    RightParen => Literal(")" |_, _| Box::new(Node::RightParen)),
}

fn main() {
    let s = String::from("1+5");
    let mut slice = s.as_str();
    let mut engine = create_parsing_engine(&mut slice).unwrap();
    let expr = engine.parse().unwrap();
    dbg!(&expr);
    dbg!(expr.eval());
}
