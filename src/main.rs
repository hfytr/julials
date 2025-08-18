use std::io::Read;

use parser::{parser, Engine};

#[derive(Debug)]
enum ErrorKind {
    TooBigLit
}

#[derive(Debug)]
struct Error {
    kind: ErrorKind,
    text: String,
}

#[derive(Debug, Clone)]
enum NodeData {
    IntLit(i128),
    UIntLit(u128),
    FloatLit(f64),
    String(String),
    None,
    Error
}

#[derive(Clone, Debug)]
struct Span {
    file: String, 
    start: (usize, usize),
    end: (usize, usize)
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
    pub errors: Vec<Error>,
    pub line: usize,
    pub col: usize
}

fn get_span(state: &'_ mut LexingState, start: (usize, usize)) -> Span {
    Span {
        file: state.file.clone(),
        start,
        end: (state.col, state.line),
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
    let neg = if neg {-1} else {1};
    let (kind, data) = if let Ok(x) = i128::from_str_radix(text, base) {
        (NodeKind::IntLitL, NodeData::IntLit(x * neg))
    } else if let Ok(x) = u128::from_str_radix(text, base) {
        (NodeKind::UIntLitL, NodeData::UIntLit(x))
    } else {
        state.errors.push(Error {
            kind: ErrorKind::TooBigLit,
            text: text.to_string()
        });
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
    let neg = text.as_bytes()[0] == b'-';
    if neg {
        text = &text[1..];
    }
    if base != 10 {
        text = &text[2..];
    }
    let pred = if base == 16 {
        |c: char| c == 'p' || c == 'P'
    } else {
        |c: char| c == '.'
    };
    let decimal_point = text.chars().enumerate().filter(|(_, c)| pred(*c)).next().unwrap().0;
    let whole_part = u64::from_str_radix(&text[..decimal_point], base).unwrap() as f64;
    let dec_part = u64::from_str_radix(&text[decimal_point+1..], base).unwrap() as f64;
    let val = whole_part + dec_part / (base as f64).powi((text.len() - decimal_point - 1) as i32);
    let val = if neg {
        -1.0 * val
    } else {
        val
    };
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
    Update("[ \t]*" |state: &mut LexingState, text: &str| {
        state.col += text.len()
    }),
    Update("\n" |state: &mut LexingState, _: &str| {
        state.col = 0;
        state.line += 1;
    }),

    IdentifierL => Regex("[a-zA-Z_]*" |state: &mut LexingState, text: &str| {
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
    IntLitL => Regex("(-|)0o[0-7]*" |state, text| int_lit(state, text, 8)),
    IntLitL => Regex("(-|)0x[0-9a-f]*" |state, text| int_lit(state, text, 16)),
    IntLitL => Regex("(-|)[0-9]*" |state, text| int_lit(state, text, 10)),
    FloatLitL => Regex("(-|)0o[0-7]*\\.[0-7][0-7]*" |state, text| float_lit(state, text, 8)),
    FloatLitL => Regex("(-|)[0-9]*\\.[0-9][0-9]*" |state, text| float_lit(state, text, 10)),
    FloatLitL => Regex("(-|)0x[0-9a-f]*[pP][0-9a-f][0-9a-f]*" |state, text| float_lit(state, text, 16)),
    StrLitL => Regex("\"([^\"]|\\\\\")*\"" |state, text| str_lit(state, text)),

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

    DivL => Literal("/" |state, text| make_lit(state, text, NodeKind::DivL)),
    ModL => Literal("÷" |state, text| make_lit(state, text, NodeKind::ModL)),
    ModL => Literal("%" |state, text| make_lit(state, text, NodeKind::ModL)),
    AndL => Literal("&" |state, text| make_lit(state, text, NodeKind::AndL)),
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
}
// < parametrized_choice > ::= '::' | '< :'  
// < or_expression > ::= < or_expression > '||' < and_expression > |   
//   
// < and_expression > ::= < and_expression > '&&' < assignment_expression > |   
//   
// < pair_expression > ::= < expression > '= >' < pair_expression > |  
//                       
// < identifier > ::= /[a-zA-Z]\w*/  
// < number > ::= /\d+/  
// < string > ::= '"' < characters > '"'  
// < characters > ::= < character > | < character > < characters >  
// < character > ::= < identifier > | < number >  
//   
// < times_operator > ::= '*' | '/' | '÷' | '%' | '&' | '⋅' | '*' | '\\'  
// < plus_opereator > ::= '+' | '-'  
// < arrow_operator > ::=  '←' | '→'  
// < assign_opereator > ::= ':=' | '~' | '$=' | '=' | '+=' | '-=' | '*=' |  
//                          '/=' | '//=' | '|\=|' | '^=' | '÷=' | '%=' | '&='  
// < comment > ::= '#' | /.*/  

fn main() {
    let engine: Engine<_, _, _, _, _, _, _, _, _, _, _> = create_parsing_engine().unwrap();
    let mut f = std::fs::File::open("../hipsat/ExaPowerIO.jl/src/ExaPowerIO.jl").unwrap();
    let mut s = "\"hello thre\"".into();
    f.read_to_string(&mut s).unwrap();
    let mut state = LexingState {
        file: String::new(),
        line: 0,
        col: 0,
        errors: vec![],
    };
    // dbg!(engine.parse(1, s.as_str(), &mut state));
    let mut iter = engine.lexemes(s.as_str(), &mut state);
    dbg!(iter.next());
}
