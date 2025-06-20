extern crate proc_macro;
use std::collections::{BTreeMap, BTreeSet, HashMap};

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
enum NFAEdge {
    Char(u8, bool),
    Range(u8, u8, bool),
    Epsilon,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
enum DFAEdge {
    Char(u8, bool),
    Range(u8, u8, bool),
}

#[derive(Debug, Clone, Copy)]
pub enum Lexeme {
    A = 0,
}

#[derive(Debug)]
pub struct NFA {
    edges: Vec<BTreeMap<NFAEdge, usize>>,
}

const ESCAPE_ARRAY: [(u8, u8); 9] = [
    (b'n', b'\n'),
    (b't', b'\t'),
    (b'(', b'('),
    (b')', b')'),
    (b'[', b'['),
    (b']', b']'),
    (b'^', b'^'),
    (b'|', b'|'),
    (b'-', b'-'),
];

struct IntegralSet(Vec<usize>);

const BITS: usize = usize::BITS as usize;

impl From<&Vec<usize>> for IntegralSet {
    fn from(value: &Vec<usize>) -> Self {
        let max = value.into_iter().max().unwrap_or(&0);
        let mut result = Self(vec![max / BITS + if max % BITS ==  0 {0} else {1}]);
        for x in value.into_iter() {
            result.0[x / BITS] |= 1 << (x % BITS);
        }
        result
    }
}

impl<const N: usize> From<[usize; N]> for IntegralSet {
    fn from(value: [usize; N]) -> Self {
        Self::from(&value.to_vec())
    }
}

impl IntegralSet {
    const fn new() -> Self{Self(vec![])}
    fn insert(&mut self, x: usize) {
        if x / BITS > self.0.len() {
            self.0.resize(x / BITS, 0);
        }
        self.0[x / BITS] |= 1 << (x % BITS);
    }
    fn delete(&mut self, x: usize) {
        self.0[x / BITS] &= !(1 << (x % BITS));
    }
}

impl IntoIterator for IntegralSet {
    type IntoIter = IntegralSetIterator;
    type Item = usize;
    fn into_iter(self) -> Self::IntoIter {
        IntegralSetIterator(self)
    }
}

struct IntegralSetIterator(IntegralSet);

impl Iterator for IntegralSetIterator {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        let mut should_pop = false;
        let offset = (self.0.0.len() - 1) * BITS;
        let res = self.0.0.last_mut().map(|last| {
            let lsb = last.trailing_zeros() as usize;
            *last &= !(1 << lsb);
            if *last == 0 {
                should_pop = true;
            }
            lsb + offset
        });
        if should_pop {
            self.0.0.pop();
        }
        res
    }
}

pub struct DFA {
    states: Vec<IntegralSet>,
    trans: Vec<BTreeMap<DFAEdge, usize>>
}

impl DFA {
    pub fn from_lexemes(lexemes: Vec<(&str, Lexeme)>) -> Self {
    }
    
    fn from_regex(regex: &str) -> (Self, usize) {
        let (nfa, fin) = NFA::from_regex(regex);
        let mut res = Self {
            states: Vec::new(),
            trans: vec![BTreeMap::new()]
        };
        let mut stack = vec![nfa.epsilon_closure(0)];
        while let Some(cur) = stack.pop() {
            for node in cur.into_iter() {
                for next in nfa.edges[node].iter().filter(|(e, _)| !matches!(e, NFAEdge::Epsilon)) {
                }
            }
        }
        (res, fin)
    }
}

impl NFA {
    pub fn epsilon_closure(&self, node: usize) -> IntegralSet {
        let mut stack = vec![node];
        let mut res = IntegralSet::new();
        while let Some(cur) = stack.pop() {
            res.insert(cur);
            self.edges[cur]
                .iter()
                .filter(|(e, _)| matches!(e, NFAEdge::Epsilon))
                .for_each(|(_, to)| res.insert(*to));
        }
        res
    }

    pub fn from_regex(regex: &str) -> (Self, usize) {
        let escape_lookup = HashMap::from(ESCAPE_ARRAY);
        let mut escaped = false;
        let mut groups = vec![];
        let mut sq = None;
        let mut sq_not = false;
        let mut last = (0, 0);
        let mut result = Self {
            edges: vec![BTreeMap::new()],
        };
        let mut in_group = 0;

        let mut s = regex.as_bytes();
        while s.len() != 0 {
            match (sq, s) {
                (_, [b'\\']) => panic!("ERROR: Last character is \\."),
                (_, [b'\\', tail @ ..]) if tail.len() != 0 => {
                    escaped = true;
                    s = tail;
                }
                (Some(_), [b'*', ..]) if !escaped => panic!("ERROR: Used * in []."),
                (None, [b'*', ..]) if in_group != 0 && !escaped => panic!("ERROR: Used * in ()."),
                (_, [b'*', tail @ ..]) if !escaped => {
                    result.edges[last.1].insert(NFAEdge::Epsilon, last.0);
                    s = tail;
                }
                (Some(_), [b'.', ..]) if !escaped => panic!("ERROR: . in []"),
                (None, [b'.', tail @ ..]) if !escaped => {
                    let new_node = result.edges.len();
                    result.edges[last.1].insert(NFAEdge::Range(u8::MIN, u8::MAX, false), new_node);
                    result.edges.push(BTreeMap::new());
                    last = (last.1, new_node);
                    s = tail;
                }
                (_, [b'-', tail @ ..]) if tail.len() != 0 => panic!("ERROR: Leading -"),
                (_, [b'-', tail @ ..]) if tail.len() == 0 => panic!("ERROR: Trailing -"),
                (None, [b'-', ..]) => panic!("ERROR: - Not in []."),
                (Some((sq_start, sq_end)), [c0, b'-', c1, tail @ ..]) if *c0 != b'\\' => {
                    let mut c0 = *c0;
                    if escaped {
                        c0 = *escape_lookup.get(&c0).expect("ERROR; Invalid escape code.");
                    }
                    let mut c1 = *c1;
                    let mut tail = tail;
                    if c1 == b'\\' {
                        c1 = tail[0];
                        c1 = *escape_lookup.get(&c1).expect("ERROR: Invalid escape code.");
                        tail = &tail[1..]
                    }
                    let new_node = result.edges.len();
                    let edge_row: &mut BTreeMap<NFAEdge, usize> = &mut result.edges[sq_start];
                    edge_row.insert(NFAEdge::Range(c0, c1, sq_not), new_node);
                    result
                        .edges
                        .push(BTreeMap::from([(NFAEdge::Epsilon, sq_end)]));
                    s = tail;
                }
                (Some(_), [b'(', ..]) => panic!("ERROR; () in []."),
                (_, [b'(', tail @ ..]) if !escaped => {
                    groups.push((last.1, result.edges.len()));
                    in_group += 1;
                    result.edges.push(BTreeMap::new());
                    let new_node = result.edges.len();
                    result.edges[last.1].insert(NFAEdge::Epsilon, new_node);
                    last = (last.1, new_node);
                    result.edges.push(BTreeMap::new());
                    s = tail;
                }
                (_, [b')', ..]) if !escaped && in_group == 0 => panic!("ERROR: Trailing )."),
                (_, [b')', tail @ ..]) if !escaped && groups.len() != 0 => {
                    result.edges[last.1].insert(NFAEdge::Epsilon, groups.last().unwrap().1);
                    last = groups.pop().unwrap();
                    s = tail;
                }
                (_, [b'|', ..]) if !escaped && in_group == 0 => panic!("ERROR: | not in ()."),
                (_, [b'|', tail @ ..]) if !escaped && in_group != 0 => {
                    result.edges[last.1].insert(NFAEdge::Epsilon, groups.last().unwrap().1);
                    last = (0, groups.last().unwrap().0);
                    let new_node = result.edges.len();
                    result.edges[last.1].insert(NFAEdge::Epsilon, new_node);
                    last = (last.1, new_node);
                    result.edges.push(BTreeMap::new());
                    s = tail;
                }
                (_, [b'[', tail @ ..]) if !escaped => {
                    sq = Some((last.1, result.edges.len()));
                    result.edges.push(BTreeMap::new());
                    s = tail;
                }
                (None, [b']', ..]) if !escaped => panic!("ERROR: Trailing ]"),
                (Some(_), [b']', tail @ ..]) if !escaped => {
                    last = sq.unwrap();
                    sq = None;
                    s = tail;
                }
                (None, [b'^', ..]) => panic!("ERROR: ^ not in []"),
                (Some(_), [b'^', ..]) if sq_not => panic!("ERROR: Multiple ^ in []"),
                (Some(_), [b'^', tail @ ..]) if !sq_not => {
                    sq_not = true;
                    s = tail;
                }
                (Some((sq_start, sq_end)), [c, tail @ ..]) => {
                    let mut c = *c;
                    if escaped {
                        c = *escape_lookup.get(&c).unwrap_or(&c);
                        escaped = false;
                    }
                    let new_node = result.edges.len();
                    result.edges[sq_start].insert(NFAEdge::Epsilon, new_node);
                    result
                        .edges
                        .push(BTreeMap::from([(NFAEdge::Char(c, sq_not), sq_end)]));
                    last = (last.1, new_node);
                    s = tail;
                }
                (None, [c, tail @ ..]) => {
                    let mut c = *c;
                    if escaped {
                        c = *escape_lookup.get(&c).unwrap_or(&c);
                        escaped = false;
                    }
                    let new_node = result.edges.len();
                    result.edges[last.1].insert(NFAEdge::Char(c, false), new_node);
                    result.edges.push(BTreeMap::new());
                    s = tail;
                    last = (last.1, new_node);
                }
                _ => {}
            }
        }

        (result, last.1)
    }
}
