extern crate proc_macro;
use std::{collections::BTreeMap, ops::{BitOr, BitOrAssign, Index}, rc::Rc};

type NFAEdge = Option<(u8, u8)>;

#[derive(Debug, Clone, Copy)]
pub enum Lexeme {
    A = 0,
}

#[derive(Debug)]
struct NFA {
    edges: Vec<Vec<(NFAEdge, usize)>>,
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

#[derive(Default, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct USizeSet(Vec<u64>);

impl<'a> USizeSet {
    fn set(&mut self, i: usize, b: bool) {
        if i / 64 >= self.0.len() {
            self.0.resize(i / 64 + 1, 0);
        }
        self.0[i / 64] &= !(1 << (i % 64));
        self.0[i / 64] |= (b as u64) << (i % 64);
    }
    fn get(&self, i: usize) -> bool {
        if i / 64 >= self.0.len() {
            return false;
        }
        self.0[i / 64] & (1 << (i % 64)) > 0
    }
    fn iter(&'a self) -> USizeSetIterator<'a> {
        USizeSetIterator {
            ind: self.0.len() - 1,
            cur: *self.0.last().unwrap_or(&0),
            set: &self,
        }
    }
}

impl<const N: usize> From<[u64; N]> for USizeSet {
    fn from(value: [u64; N]) -> Self {
        Self::from(&value.to_vec())
    }
}

impl From<&Vec<u64>> for USizeSet {
    fn from(value: &Vec<u64>) -> Self {
        let mut res = Self::default();
        for (i, v) in value.into_iter().enumerate() {
            res.0[i] = *v;
        }
        res
    }
}

impl BitOrAssign<&USizeSet> for USizeSet {
    fn bitor_assign(&mut self, rhs: &Self) {
        let len = if rhs.0.len() > self.0.len() {
            let res = self.0.len();
            self.0.resize(rhs.0.len(), 0);
            res
        } else {
            rhs.0.len()
        };
        for i in 0..len {
            self.0[i] |= rhs.0[i];
        }
    }
}

impl BitOr for &USizeSet {
    type Output = USizeSet;
    fn bitor(self, rhs: Self) -> Self::Output {
        let mut res = self.clone();
        res |= &rhs;
        res
    }
}

struct USizeSetIterator<'a> {
    ind: usize,
    cur: u64,
    set: &'a USizeSet,
}

impl<'a> Iterator for USizeSetIterator<'a> {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        if self.cur == 0 && self.ind == 0 {
            return None;
        } else if self.cur == 0 {
            self.ind -= 1;
            self.cur = self.set.0[self.ind];
        }
        let lsb = self.cur.trailing_zeros() as usize;
        self.cur ^= 1 << lsb;
        Some(lsb + self.ind * 64)
    }
}

#[derive(Debug)]
struct IndexableSet<T: Clone + Eq+ Ord+ PartialEq+ PartialOrd> {
    set: BTreeMap<Rc<T>, usize>,
    vec: Vec<Rc<T>>,
}

impl<T: Clone + Eq+ Ord+ PartialEq+ PartialOrd> IndexableSet<T> {
    fn len(&self) -> usize {
        self.vec.len()
    }
    fn last(&self) -> Option<Rc<T>> {
        self.vec.last().map(Rc::clone)
    }
    fn get(&self, set: &T) -> Option<usize> {
        self.set.get(set).map(|x| *x)
    }
    fn push(&mut self, t: T) {
        self.vec.push(Rc::new(t));
        self.set.insert(self.last().unwrap(), self.len() - 1);
    }
}

impl<T: Clone + Eq+ Ord+ PartialEq+ PartialOrd> Index<usize> for IndexableSet<T> {
    fn index(&self, index: usize) -> &Self::Output {
        &self.vec[index]
    }
    type Output = Rc<T>;
}

impl<T: Clone + Eq+ Ord+ PartialEq+ PartialOrd, const N: usize> From<[T; N]> for IndexableSet<T> {
    fn from(items: [T; N]) -> Self {
        let items: Vec<_> = items.into_iter().map(Rc::new).collect();
        Self {
            set: BTreeMap::from_iter(items.iter().enumerate().map(|(i, rc)| (Rc::clone(rc), i))),
            vec: items,
        }
    }
}

#[derive(Debug)]
pub struct DFA {
    states: IndexableSet<USizeSet>,
    trans: Vec<[Option<usize>; 256]>,
    fin: Vec<bool>,
}

impl DFA {
    pub fn from_lexemes(_lexemes: Vec<(&str, Lexeme)>) -> Self {
        unimplemented!()
    }

    pub fn from_regex(regex: &str) -> (Self, usize) {
        let (nfa, fin) = NFA::from_regex(regex);
        println!("{:?}", nfa);
        let mut res = Self {
            states: IndexableSet::from([nfa.epsilon_closure(0)]),
            trans: vec![],
            fin: vec![],
        };
        let mut stack = vec![0];
        while let Some(cur_ind) = stack.pop() {
            let cur_state = Rc::clone(&res.states[cur_ind]);
            let mut char_state = [None; 256];
            for (edge, dest) in cur_state
                .iter()
                .flat_map(|node| nfa.edges[node].iter())
                .filter_map(|(edge, dest)| edge.map(|e| (e, dest)))
            {
                let mut new_state_map: BTreeMap<usize, usize> = BTreeMap::new();
                let new_state = nfa.epsilon_closure(*dest);
                let base_state_ind = res.states.get(&new_state).unwrap_or_else(|| {
                    stack.push(res.states.len());
                    res.states.push(new_state);
                    *stack.last().unwrap()
                });
                for i in (edge.0..=edge.1).map(usize::from) {
                    if let Some(old_state) = char_state[i] {
                        char_state[i] = new_state_map.get(&old_state).map(|x| *x).or_else(|| {
                            let new_state = res.states[old_state].as_ref() | res.states[base_state_ind].as_ref();
                            let new_ind = res.states.get(&new_state).unwrap_or_else(|| {
                                stack.push(res.states.len());
                                res.states.push(new_state);
                                res.fin.push(res.states.last().unwrap().get(fin));
                                res.states.len() - 1
                            });
                            new_state_map.insert(old_state, new_ind);
                            Some(new_ind)
                        });
                    } else {
                        char_state[i] = Some(base_state_ind);
                    }
                }
            }
            if res.trans.len() < cur_ind + 1 {
                res.trans.resize(cur_ind + 1, [None; 256]);
            }
            res.trans[cur_ind] = char_state;
        }
        (res, fin)
    }
}

impl NFA {
    pub fn epsilon_closure(&self, node: usize) -> USizeSet {
        let mut stack = vec![node];
        let mut res = USizeSet::default();
        let mut vis = USizeSet::default();
        vis.set(node, true);
        while let Some(cur) = stack.pop() {
            res.set(cur, true);
            stack.extend(
                self.edges[cur]
                    .iter()
                    .filter_map(|(maybe_e, v)| maybe_e.map(|_| 0).xor(Some(*v)))
                    .filter(|v| {
                        let res = !vis.get(*v);
                        vis.set(*v, true);
                        res
                    }),
            );
            self.edges[cur]
                .iter()
                .filter(|(e, _)| e.is_none())
                .for_each(|(_, to)| res.set(*to, true));
        }
        res
    }

    pub fn from_regex(regex: &str) -> (Self, usize) {
        let mut escape_lookup = [None; 256];
        for (key, val) in ESCAPE_ARRAY {
            escape_lookup[key as usize] = Some(val);
        }
        let mut escaped = false;
        let mut groups = vec![];
        let mut sq = None;
        let mut sq_not = false;
        let mut last = (0, 0);
        let mut result = Self {
            edges: vec![vec![]],
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
                    result.edges[last.1].push((None, last.0));
                    s = tail;
                }
                (Some(_), [b'.', ..]) if !escaped => panic!("ERROR: . in []"),
                (None, [b'.', tail @ ..]) if !escaped => {
                    let new_node = result.edges.len();
                    result.edges[last.1].push((Some((u8::MIN, u8::MAX)), new_node));
                    result.edges.push(vec![]);
                    last = (last.1, new_node);
                    s = tail;
                }
                (_, [b'-', tail @ ..]) if tail.len() != 0 => panic!("ERROR: Leading -"),
                (_, [b'-', tail @ ..]) if tail.len() == 0 => panic!("ERROR: Trailing -"),
                (None, [b'-', ..]) => panic!("ERROR: - Not in []."),
                (Some((sq_start, sq_end)), [c0, b'-', c1, tail @ ..]) if *c0 != b'\\' => {
                    let mut c0 = *c0;
                    if escaped {
                        c0 = escape_lookup[c0 as usize].expect("ERROR; Invalid escape code.");
                    }
                    let mut c1 = *c1;
                    let mut tail = tail;
                    if c1 == b'\\' {
                        c1 = tail[0];
                        c1 = escape_lookup[c1 as usize].expect("ERROR: Invalid escape code.");
                        tail = &tail[1..]
                    }
                    let new_node = result.edges.len();
                    let edge_row: &mut Vec<(NFAEdge, usize)> = &mut result.edges[sq_start];
                    if sq_not {
                        if c0 != u8::MIN {
                            edge_row.push((Some((u8::MIN, c0 - 1)), new_node));
                        }
                        if c1 != u8::MAX {
                            edge_row.push((Some((c1 + 1, u8::MAX)), new_node));
                        }
                    }
                    result.edges.push(vec![(None, sq_end)]);
                    s = tail;
                }
                (Some(_), [b'(', ..]) => panic!("ERROR; () in []."),
                (_, [b'(', tail @ ..]) if !escaped => {
                    groups.push((last.1, result.edges.len()));
                    in_group += 1;
                    result.edges.push(vec![]);
                    let new_node = result.edges.len();
                    result.edges[last.1].push((None, new_node));
                    last = (last.1, new_node);
                    result.edges.push(vec![]);
                    s = tail;
                }
                (_, [b')', ..]) if !escaped && in_group == 0 => panic!("ERROR: Trailing )."),
                (_, [b')', tail @ ..]) if !escaped && groups.len() != 0 => {
                    result.edges[last.1].push((None, groups.last().unwrap().1));
                    last = groups.pop().unwrap();
                    s = tail;
                }
                (_, [b'|', ..]) if !escaped && in_group == 0 => panic!("ERROR: | not in ()."),
                (_, [b'|', tail @ ..]) if !escaped && in_group != 0 => {
                    result.edges[last.1].push((None, groups.last().unwrap().1));
                    last = (0, groups.last().unwrap().0);
                    let new_node = result.edges.len();
                    result.edges[last.1].push((None, new_node));
                    last = (last.1, new_node);
                    result.edges.push(vec![]);
                    s = tail;
                }
                (_, [b'[', tail @ ..]) if !escaped => {
                    sq = Some((last.1, result.edges.len()));
                    result.edges.push(vec![]);
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
                        c = escape_lookup[c as usize].unwrap_or(c);
                        escaped = false;
                    }
                    let new_node = result.edges.len();
                    result.edges[sq_start].push((None, new_node));
                    result.edges.push(vec![]);
                    if sq_not {
                        if c != u8::MIN {
                            result
                                .edges
                                .last_mut()
                                .unwrap()
                                .push((Some((u8::MIN, c - 1)), new_node));
                        }
                        if c != u8::MAX {
                            result
                                .edges
                                .last_mut()
                                .unwrap()
                                .push((Some((c + 1, u8::MAX)), new_node));
                        }
                    } else {
                        result
                            .edges
                            .last_mut()
                            .unwrap()
                            .push((Some((c, c)), sq_end));
                    }
                    last = (last.1, new_node);
                    s = tail;
                }
                (None, [c, tail @ ..]) => {
                    let mut c = *c;
                    if escaped {
                        c = escape_lookup[c as usize].unwrap_or(c);
                        escaped = false;
                    }
                    let new_node = result.edges.len();
                    result.edges[last.1].push((Some((c, c)), new_node));
                    result.edges.push(vec![]);
                    s = tail;
                    last = (last.1, new_node);
                }
                _ => {}
            }
        }

        (result, last.1)
    }
}
