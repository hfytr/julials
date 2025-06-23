fn main() {
    let dfa = julials::DFA::from_regex(".*");
    println!("{:?}", dfa);
}
