use std::io::Read;

use parser::{Engine, parser};

#[derive(Debug)]
enum ErrorKind {
    TooBigLit,
}

#[derive(Debug, Clone)]
enum NodeData {
    IntLit(i128),
    UIntLit(u128),
    FloatLit(f64),
    String(String),
    None,
    Error,
}

#[derive(Clone, Debug)]
struct Span {
    file: String,
    start: (usize, usize),
    end: (usize, usize),
}

#[derive(Clone, Debug)]
struct Node {
    kind: NodeKind,
    span: Span,
    data: NodeData,
}

#[derive(Debug)]
struct LexingState {
    pub file: String,
    pub errors: Vec<String>,
    pub line: usize,
    pub col: usize,
}

fn get_span(state: &'_ mut LexingState, start: (usize, usize)) -> Span {
    Span {
        file: state.file.clone(),
        start,
        end: (state.line, state.col),
    }
}

fn make_lit<'a>(state: &'a mut LexingState, text: &'_ str, kind: NodeKind) -> (Node, usize) {
    let start = (state.line, state.col);
    state.col += text.len();
    let node = Node {
        kind,
        span: get_span(state, start),
        data: NodeData::None,
    };
    (node, kind as usize)
}

fn new_line(state: &mut LexingState) -> (Node, usize) {
    state.col = 0;
    state.line += 1;
    let node = Node {
        kind: NodeKind::TermL,
        span: get_span(state, (state.line, state.col)),
        data: NodeData::None,
    };
    (node, NodeKind::TermL as usize)
}

fn int_lit(state: &mut LexingState, mut text: &str, base: u32) -> (Node, usize) {
    let start = (state.line, state.col);
    state.col += text.len();
    let neg = text.as_bytes()[0] == b'-';
    if neg {
        text = &text[1..];
    }
    if base != 10 {
        text = &text[2..]
    }
    dbg!(text);
    dbg!(base);
    let neg = if neg { -1 } else { 1 };
    let (kind, data) = if let Ok(x) = i128::from_str_radix(text, base) {
        (NodeKind::IntLitL, NodeData::IntLit(x * neg))
    } else if let Ok(x) = u128::from_str_radix(text, base) {
        (NodeKind::UIntLitL, NodeData::UIntLit(x))
    } else {
        state
            .errors
            .push(format!("found too large integer literal"));
        (NodeKind::IntLitL, NodeData::Error)
    };
    let node = Node {
        kind,
        data,
        span: get_span(state, start),
    };
    (node, kind as usize)
}

fn float_lit(state: &mut LexingState, mut text: &str, base: u32) -> (Node, usize) {
    let start = (state.line, state.col);
    state.col += text.len();
    let pred = if base == 16 {
        |c: char| c == 'p' || c == 'P' || c == '.'
    } else {
        |c: char| c == '.'
    };
    let (dec_point, dec_char) = text
        .chars()
        .enumerate()
        .filter(|(_, c)| pred(*c))
        .next()
        .unwrap();
    let whole_part = u64::from_str_radix(&text[..dec_point], base).unwrap() as f64;
    let exp_neg = text[dec_point + 1..]
        .chars()
        .enumerate()
        .filter(|(_, c)| *c == '-' || *c == '+')
        .next()
        .map(|x| (x.0, x.1 == '-'));
    let (exp_point, exp_char) = text[dec_point + 1..]
        .chars()
        .enumerate()
        .filter(|(_, c)| ['e', 'f', 'p', 'P'].contains(c))
        .next()
        .unzip();
    let dec_part = u64::from_str_radix(&text[dec_point + 1..exp_point.unwrap_or(text.len())], base)
        .unwrap() as f64;
    let mut val = whole_part + dec_part / (base as f64).powi((text.len() - dec_point - 1) as i32);
    if let Some(exp_point) = exp_point
        && let Some(exp_char) = exp_char
    {
        let neg = if exp_neg.is_some() { -1 } else { 1 };
        if dec_char != '.' {
            let lit_type = if base == 16 { "hex" } else { "decimal" };
            state.errors.push(format!("{lit_type} float literals with an exponent suffix must use a . decimal point (not {dec_char})"));
        }
        if base == 16 && exp_char != 'p' && exp_char != 'P' {
            state.errors.push(format!(
                "hex float literals must use a p or P as the exponentiation suffix (not {exp_char})"
            ));
        }
        let exp = neg * text[exp_point + 1..].parse::<i32>().unwrap();
        val = val * (base as f64).powi(exp)
    } else if base == 16 && dec_char != 'p' && dec_char != 'P' {
        state.errors.push(format!("hex float without exponent suffix must use p or P as the decimal point (not {dec_char})"));
    }

    let node = Node {
        kind: NodeKind::FloatLitL,
        span: get_span(state, start),
        data: NodeData::FloatLit(val),
    };
    (node, NodeKind::FloatLitL as usize)
}

fn str_lit(state: &mut LexingState, text: &'_ str) -> (Node, usize) {
    let start = (state.line, state.col);
    state.col += text.len();
    let node = Node {
        kind: NodeKind::StrLitL,
        data: NodeData::String(text.to_string()),
        span: get_span(state, start),
    };
    (node, NodeKind::StrLitL as usize)
}

parser! {
    State(LexingState),
    Output(Node),
    Kind(NodeKind),
    Update("[ \t]*" |state: &mut LexingState, text: &str| state.col += text.len()),
    Update("#=([^=]|=[^#])*=#" |state: &mut LexingState, text: &str| {}),
    Update("#[^\\n]*" |state: &mut LexingState, text: &str| state.col += text.len()),

    IdentifierL => Regex("[a-zA-Z_][a-zA-Z0-9_]*" |state: &mut LexingState, text: &str| {
        let start = (state.line, state.col);
        state.col += text.len();
        let node = Node {
            kind: NodeKind::IdentifierL,
            span: get_span(state, start),
            data: NodeData::String(text.to_string())
        };
        (node, NodeKind::IdentifierL as usize)
    }),

    UIntLitL,
    IntLitL => Regex("0b[01_]*" |state, text| int_lit(state, text, 2)),
    IntLitL => Regex("0o[0-7_]*" |state, text| int_lit(state, text, 8)),
    IntLitL => Regex("[0-9_]*" |state, text| int_lit(state, text, 10)),
    IntLitL => Regex("0x[0-9a-fA-F_]*" |state, text| int_lit(state, text, 16)),
    FloatLitL => Regex("[0-9_]*\\.[0-9_]*([ef](-|+|)[0-9][0-9]*|)" |state, text| float_lit(state, text, 10)),
    FloatLitL => Regex("0x[0-9a-fA-F_]*[pP.][0-9a-fA-F_]*([pP](-|+|)[0-9]|)*" |state, text: &str| float_lit(state, &text[2..], 16)),
    StrLitL => Regex("\"([^\"]|\\\\\")*\"" |state, text: &str| str_lit(state, &text[1..text.len()-1])),
    StrLitL => Regex("\"\"\"([^\"]|\"[^\"]|\"\"[^\"])*\"\"\"" |state, text: &str| str_lit(state, &text[3..text.len()-3])),

    FunctionL => Literal("function" |state, text| make_lit(state, text, NodeKind::FunctionL)),
    BeginL => Literal("begin" |state, text| make_lit(state, text, NodeKind::BeginL)),
    EndL => Literal("end" |state, text| make_lit(state, text, NodeKind::EndL)),
    IfL => Literal("if" |state, text| make_lit(state, text, NodeKind::IfL)),
    ElseL => Literal("else" |state, text| make_lit(state, text, NodeKind::ElseL)),
    ElseIfL => Literal("elseif" |state, text| make_lit(state, text, NodeKind::ElseIfL)),
    ForL => Literal("for" |state, text| make_lit(state, text, NodeKind::ForL)),
    InL => Literal("in" |state, text| make_lit(state, text, NodeKind::InL)),
    ModuleL => Literal("module" |state, text| make_lit(state, text, NodeKind::ModuleL)),
    StructL => Literal("struct" |state, text| make_lit(state, text, NodeKind::StructL)),
    MutableL => Literal("mutable" |state, text| make_lit(state, text, NodeKind::MutableL)),
    WhileL => Literal("while" |state, text| make_lit(state, text, NodeKind::WhileL)),
    BreakL => Literal("break" |state, text| make_lit(state, text, NodeKind::BreakL)),
    ContinueL => Literal("continue" |state, text| make_lit(state, text, NodeKind::ContinueL)),
    ReturnL => Literal("return" |state, text| make_lit(state, text, NodeKind::ReturnL)),
    TrueL => Literal("true" |state, text| make_lit(state, text, NodeKind::TrueL)),
    FalseL => Literal("false" |state, text| make_lit(state, text, NodeKind::FalseL)),
    NothingL => Literal("nothing" |state, text| make_lit(state, text, NodeKind::NothingL)),
    ConstL => Literal("const" |state, text| make_lit(state, text, NodeKind::ConstL)),
    GlobalL => Literal("global" |state, text| make_lit(state, text, NodeKind::GlobalL)),
    LocalL => Literal("local" |state, text| make_lit(state, text, NodeKind::LocalL)),

    LParenL => Literal("(" |state, text| make_lit(state, text, NodeKind::LParenL)),
    RParenL => Literal(")" |state, text| make_lit(state, text, NodeKind::RParenL)),
    LCurlL => Literal("{" |state, text| make_lit(state, text, NodeKind::LCurlL)),
    RCurlL => Literal("}" |state, text| make_lit(state, text, NodeKind::RCurlL)),
    LSquareL => Literal("[" |state, text| make_lit(state, text, NodeKind::LSquareL)),
    RSquareL => Literal("]" |state, text| make_lit(state, text, NodeKind::RSquareL)),

    CommaL => Literal("," |state, text| make_lit(state, text, NodeKind::CommaL)),
    TermL => Literal(";" |state, text| make_lit(state, text, NodeKind::TermL)),
    TermL => Literal("\n" |state, _| new_line(state)),

    LArrowL => Literal("<-" |state, text| make_lit(state, text, NodeKind::LArrowL)),
    RArrowL => Literal("->" |state, text| make_lit(state, text, NodeKind::RArrowL)),

    LAndL => Literal("&&" |state, text| make_lit(state, text, NodeKind::LAndL)),
    LOrL => Literal("||" |state, text| make_lit(state, text, NodeKind::LOrL)),

    ExpL => Literal("^" |state, text| make_lit(state, text, NodeKind::ExpL)),
    DivL => Literal("/" |state, text| make_lit(state, text, NodeKind::DivL)),
    ModL => Literal("÷" |state, text| make_lit(state, text, NodeKind::ModL)),
    ModL => Literal("%" |state, text| make_lit(state, text, NodeKind::ModL)),
    AndL => Literal("&" |state, text| make_lit(state, text, NodeKind::AndL)),
    OrL => Literal("|" |state, text| make_lit(state, text, NodeKind::OrL)),
    MulL => Literal("⋅" |state, text| make_lit(state, text, NodeKind::MulL)),
    MulL => Literal("*" |state, text| make_lit(state, text, NodeKind::MulL)),
    LShiftL => Literal("<<" |state, text| make_lit(state, text, NodeKind::LShiftL)),
    RShiftL => Literal(">>" |state, text| make_lit(state, text, NodeKind::RShiftL)),
    ULShiftL => Literal("<<<" |state, text| make_lit(state, text, NodeKind::ULShiftL)),
    URShiftL => Literal(">>>" |state, text| make_lit(state, text, NodeKind::URShiftL)),
    RevDivL => Literal("\\" |state, text| make_lit(state, text, NodeKind::RevDivL)),
    AddL => Literal("+" |state, text| make_lit(state, text, NodeKind::AddL)),
    SubL => Literal("-" |state, text| make_lit(state, text, NodeKind::SubL)),
    BNotL => Literal("~" |state, text| make_lit(state, text, NodeKind::BNotL)),

    EqL => Literal("=" |state, text| make_lit(state, text, NodeKind::EqL)),
    DivEqL => Literal("/=" |state, text| make_lit(state, text, NodeKind::DivEqL)),
    ModEqL => Literal("÷=" |state, text| make_lit(state, text, NodeKind::ModEqL)),
    ModEqL => Literal("%=" |state, text| make_lit(state, text, NodeKind::ModEqL)),
    AndEqL => Literal("&=" |state, text| make_lit(state, text, NodeKind::AndEqL)),
    OrEqL => Literal("|=" |state, text| make_lit(state, text, NodeKind::OrEqL)),
    MulEqL => Literal("⋅=" |state, text| make_lit(state, text, NodeKind::MulEqL)),
    MulEqL => Literal("*=" |state, text| make_lit(state, text, NodeKind::MulEqL)),
    LShiftEqL => Literal("<<=" |state, text| make_lit(state, text, NodeKind::LShiftEqL)),
    RShiftEqL => Literal(">>=" |state, text| make_lit(state, text, NodeKind::RShiftEqL)),
    ULShiftEqL => Literal("<<<=" |state, text| make_lit(state, text, NodeKind::ULShiftEqL)),
    URShiftEqL => Literal(">>>=" |state, text| make_lit(state, text, NodeKind::URShiftEqL)),
    RevDivEqL => Literal("\\=" |state, text| make_lit(state, text, NodeKind::RevDivEqL)),
    AddEqL => Literal("+=" |state, text| make_lit(state, text, NodeKind::AddEqL)),
    SubEqL => Literal("-=" |state, text| make_lit(state, text, NodeKind::SubEqL)),
    BNotEqL => Literal("~=" |state, text| make_lit(state, text, NodeKind::BNotEqL)),
    ExpEqL => Literal("^=" |state, text| make_lit(state, text, NodeKind::ExpEqL)),

    L => Literal("" |state, text| make_lit(state, text, NodeKind::L)),
    TripleEqL => Literal("===" |state, text| make_lit(state, text, NodeKind::TripleEqL)),
    NotTripleEqL => Literal("!==" |state, text| make_lit(state, text, NodeKind::NotTripleEqL)),
    DoubleEqL => Literal("==" |state, text| make_lit(state, text, NodeKind::DoubleEqL)),
    NotEqL => Literal("!=" |state, text| make_lit(state, text, NodeKind::NotEqL)),
    MoreEqL => Literal(">=" |state, text| make_lit(state, text, NodeKind::MoreEqL)),
    LessEqL => Literal("<=" |state, text| make_lit(state, text, NodeKind::LessEqL)),
    MoreL => Literal(">" |state, text| make_lit(state, text, NodeKind::MoreL)),
    LessL => Literal("<" |state, text| make_lit(state, text, NodeKind::LessL)),
    SubTypeL => Literal("<:" |state, text| make_lit(state, text, NodeKind::SubTypeL)),
    SupTypeL => Literal(":>" |state, text| make_lit(state, text, NodeKind::SupTypeL)),

    ColonL => Literal(":" |state, text| make_lit(state, text, NodeKind::ColonL)),
    DColonL => Literal("::" |state, text| make_lit(state, text, NodeKind::DColonL)),
    QuestionL => Literal("?" |state, text| make_lit(state, text, NodeKind::QuestionL)),
    DollarL => Literal("$" |state, text| make_lit(state, text, NodeKind::DollarL)),
    AtL => Literal("@" |state, text| make_lit(state, text, NodeKind::AtL)),
    SplatL => Literal("..." |state, text| make_lit(state, text, NodeKind::SplatL)),
}

fn main() {
    let engine: Engine<_, _, _, _, _, _, _, _, _, _, _> = create_parsing_engine().unwrap();
    let mut f = std::fs::File::open("../hipsat/ExaPowerIO.jl/src/ExaPowerIO.jl").unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    let mut state = LexingState {
        file: String::new(),
        line: 0,
        col: 0,
        errors: vec![],
    };
    // dbg!(engine.parse(1, s.as_str(), &mut state));
    let mut iter = engine.lexemes(s.as_str(), &mut state);
    while let Some(Ok(l)) = iter.next() {
        dbg!(l);
    }
}
