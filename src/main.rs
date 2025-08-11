use parser::parser;

struct LexingState {
    pub file: String,
    pub line: usize,
    pub col: usize
}

#[derive(Clone)]
#[repr(u32)]
enum NodeKind {
    FunctionL
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

fn make_lit<'a>(state: &'a mut LexingState, text: &'_ str, kind: NodeKind) -> Node {
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
    TermL => Literal("\n" |state, text| {
        state.col = 0;
        state.line += 1;
        NodeKind::TermL
    }),
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
    let engine = create_parsing_engine().unwrap();
}
