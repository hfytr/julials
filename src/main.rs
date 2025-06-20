fn main() {
    let nfa = julials::NFA::new(vec![("b*(abc|a)", julials::Lexeme::A)]);
    println!("{:?}", nfa);
}
