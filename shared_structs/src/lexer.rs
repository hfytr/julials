use super::fmt_maybe_arr;
use crate::sets::{IndexableSet, USizeSet};
use proc_macro2::{Punct, Spacing, TokenStream};
use quote::{ToTokens, TokenStreamExt, quote};
use std::{collections::BTreeMap, fmt::Debug, rc::Rc};

type NFAEdge = Option<(u8, u8)>;

pub struct TrieNode {
    pub callback: Option<usize>,
    pub children: [Option<usize>; 256],
}

impl Debug for TrieNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.callback.fmt(f)?;
        f.write_str("\n")?;
        fmt_maybe_arr(f, &self.children)
    }
}

impl TrieNode {
    pub fn from_raw(callback: Option<usize>, children: [Option<usize>; 256]) -> Self {
        Self { callback, children }
    }
}

impl ToTokens for TrieNode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let callback = if let Some(u) = self.callback {
            quote! { Some(#u) }
        } else {
            quote! { None }
        };
        let mut children_inner = TokenStream::new();
        children_inner.append_separated(
            self.children.iter().map(|maybe_child| {
                maybe_child
                    .map(|child| quote! { Some(#child) })
                    .unwrap_or(quote! { None })
            }),
            Punct::new(',', Spacing::Alone),
        );
        tokens.append_all(quote! {
            shared_structs::TrieNode::from_raw(#callback, [#children_inner])
        });
    }
}

#[derive(Debug)]
pub struct Trie(pub Vec<TrieNode>);

impl Trie {
    pub fn insert(&mut self, s: &[u8], x: usize) {
        let mut cur = 0;
        for c in s {
            cur = self.0[cur].children[*c as usize].unwrap_or_else(|| {
                self.0[cur].children[*c as usize] = Some(self.0.len());
                self.0.push(TrieNode {
                    callback: None,
                    children: [None; 256],
                });
                self.0.len() - 1
            });
        }
        self.0[cur].callback = Some(x)
    }

    pub fn query_longest(&self, s: &[u8]) -> Option<(usize, usize)> {
        let mut maybe_trie_match = None;
        let mut cur = 0;
        let mut i = 0;
        while let Some(next) = self.0[cur].children[s[i] as usize] {
            if let Some(fin) = self.0[cur].callback {
                maybe_trie_match = Some(fin);
            }
            cur = next;
            i += 1;
        }
        maybe_trie_match.map(|trie_match| (trie_match, i))
    }
}

impl<const N: usize> From<[TrieNode; N]> for Trie {
    fn from(a: [TrieNode; N]) -> Self {
        Self(a.into_iter().collect())
    }
}

impl ToTokens for Trie {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut result = TokenStream::new();
        result.append_separated(self.0.iter(), Punct::new(',', Spacing::Alone));
        tokens.append_all(quote! {
            shared_structs::Trie::from([#result])
        });
    }
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

pub struct RegexDFA {
    states: IndexableSet<USizeSet>,
    pub trans: Vec<[Option<usize>; 256]>,
    pub fin: Vec<Option<usize>>,
}

impl Debug for RegexDFA {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("RegexDFA(")?;
        self.states.fmt(f)?;
        self.fin.fmt(f)?;
        for tran in self.trans.iter() {
            fmt_maybe_arr(f, tran)?;
            f.write_str(", ")?;
        }
        f.write_str(")")
    }
}

impl RegexDFA {
    pub fn from_regexi(regexi: Vec<(String, usize)>) -> Self {
        let nfa = NFA::from_regexi(regexi);
        let mut res = Self {
            states: IndexableSet::from([nfa.epsilon_closure(0)]),
            trans: vec![],
            fin: vec![None],
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
                    res.fin.push(
                        new_state
                            .iter()
                            .fold(None, |acc, nfa_state| acc.or(nfa.fin[nfa_state])),
                    );
                    res.states.push(new_state).0
                });
                for i in (edge.0..=edge.1).map(usize::from) {
                    if let Some(old_state) = char_state[i] {
                        char_state[i] = new_state_map.get(&old_state).map(|x| *x).or_else(|| {
                            let new_state = res.states[old_state].as_ref()
                                | res.states[base_state_ind].as_ref();
                            let new_ind = res.states.get(&new_state).unwrap_or_else(|| {
                                stack.push(res.states.len());
                                res.fin.push(
                                    new_state
                                        .iter()
                                        .fold(None, |acc, nfa_state| nfa.fin[nfa_state].or(acc)),
                                );
                                res.states.push(new_state).0
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
        res
    }

    pub fn minimize(&mut self) {
        // TODO
    }

    pub fn from_raw(
        states: IndexableSet<USizeSet>,
        trans: Vec<[Option<usize>; 256]>,
        fin: Vec<Option<usize>>,
    ) -> Self {
        Self { states, trans, fin }
    }
}

impl ToTokens for RegexDFA {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let states = &self.states;
        let mut trans_inner = TokenStream::new();
        let quote_option_vec = |v: &[Option<usize>]| {
            let mut res = TokenStream::new();
            res.append_separated(
                v.iter().map(|maybe_vi| {
                    maybe_vi
                        .map(|vi| quote! { Some(#vi) })
                        .unwrap_or(quote! { None })
                }),
                Punct::new(',', Spacing::Alone),
            );
            res
        };
        trans_inner.append_separated(
            self.trans.iter().map(|tran| {
                let inner = quote_option_vec(tran);
                quote! { [#inner] }
            }),
            Punct::new(',', Spacing::Alone),
        );
        let fin_inner = quote_option_vec(&self.fin);
        tokens.append_all(quote! {
            shared_structs::RegexDFA::from_raw(#states, vec![#trans_inner], vec![#fin_inner])
        });
    }
}

#[derive(Debug)]
struct NFA {
    edges: Vec<Vec<(NFAEdge, usize)>>,
    fin: Vec<Option<usize>>,
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

    pub fn from_regexi(regexi: Vec<(String, usize)>) -> Self {
        regexi
            .iter()
            .fold(Self::from_regex("").0, |mut acc, regex| {
                let (mut new_nfa, new_fin) = Self::from_regex(&regex.0);
                let len = acc.edges.len();
                acc.fin.resize(len + new_nfa.edges.len(), None);
                acc.fin[new_fin + len] = Some(regex.1);
                acc.edges[0].push((None, len));
                acc.edges.append(&mut new_nfa.edges);
                acc
            })
    }

    fn from_regex(regex: &str) -> (Self, usize) {
        let mut escape_lookup = [None; 256];
        for (key, val) in ESCAPE_ARRAY {
            escape_lookup[key as usize] = Some(val);
        }
        let mut escaped = false;
        let mut groups = vec![];
        let mut sq = None;
        let mut sq_chars: [u64; 4] = [0; 4];
        let mut sq_not = false;
        let mut last = (0, 0);
        let mut res = Self {
            edges: vec![vec![]],
            fin: vec![],
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
                    res.edges[last.1].push((None, last.0));
                    s = tail;
                }
                (Some(_), [b'.', ..]) if !escaped => panic!("ERROR: . in []"),
                (None, [b'.', tail @ ..]) if !escaped => {
                    let new_node = res.edges.len();
                    res.edges[last.1].push((Some((u8::MIN, u8::MAX)), new_node));
                    res.edges.push(vec![]);
                    last = (last.1, new_node);
                    s = tail;
                }
                (_, [b'-', tail @ ..]) if tail.len() != 0 => panic!("ERROR: Leading -"),
                (_, [b'-', tail @ ..]) if tail.len() == 0 => panic!("ERROR: Trailing -"),
                (None, [b'-', ..]) => panic!("ERROR: - Not in []."),
                (Some(_), [c0, b'-', c1, tail @ ..]) if *c0 != b'\\' => {
                    let mut c0 = *c0;
                    if escaped {
                        c0 = escape_lookup[c0 as usize].expect("ERROR; Invalid escape code.");
                    }
                    let mut c1 = *c1;
                    let mut tail = tail;
                    if c1 == b'\\' {
                        c1 = escape_lookup[tail[0] as usize].expect("ERROR: Invalid escape code.");
                        tail = &tail[1..]
                    }
                    for i in c0..=c1 {
                        sq_chars[i as usize / 64] |= 1 << (i % 64);
                    }
                    s = tail;
                }
                (Some(_), [b'(', ..]) => panic!("ERROR; () in []."),
                (_, [b'(', tail @ ..]) if !escaped => {
                    groups.push((last.1, res.edges.len()));
                    in_group += 1;
                    res.edges.push(vec![]);
                    let new_node = res.edges.len();
                    res.edges[last.1].push((None, new_node));
                    last = (last.1, new_node);
                    res.edges.push(vec![]);
                    s = tail;
                }
                (_, [b')', ..]) if !escaped && in_group == 0 => panic!("ERROR: Trailing )."),
                (_, [b')', tail @ ..]) if !escaped && groups.len() != 0 => {
                    res.edges[last.1].push((None, groups.last().unwrap().1));
                    last = groups.pop().unwrap();
                    in_group -= 1;
                    s = tail;
                }
                (_, [b'|', ..]) if !escaped && in_group == 0 => panic!("ERROR: | not in ()."),
                (_, [b'|', tail @ ..]) if !escaped && in_group != 0 => {
                    res.edges[last.1].push((None, groups.last().unwrap().1));
                    last = (0, groups.last().unwrap().0);
                    let new_node = res.edges.len();
                    res.edges[last.1].push((None, new_node));
                    last = (last.1, new_node);
                    res.edges.push(vec![]);
                    s = tail;
                }
                (_, [b'[', tail @ ..]) if !escaped => {
                    sq = Some((last.1, res.edges.len()));
                    last = (last.1, res.edges.len());
                    sq_chars = [0; 4];
                    res.edges.push(vec![]);
                    s = tail;
                }
                (None, [b']', ..]) if !escaped => panic!("ERROR: Trailing ]"),
                (Some((sq_start, sq_end)), [b']', tail @ ..]) if !escaped => {
                    let mut start = 0;
                    let mut end = 0;
                    let mut in_feasible = false;
                    sq_chars = [!sq_chars[0], !sq_chars[1], !sq_chars[2], !sq_chars[3]];
                    let get_u8_set = |i: u8| sq_chars[i as usize / 64] & (1 << (i % 64)) > 0;
                    for i in 0..=255 {
                        if get_u8_set(i) && in_feasible {
                            end += 1;
                        } else if in_feasible {
                            res.edges[sq_start].push((Some((start, end)), sq_end));
                            in_feasible = false;
                        } else if get_u8_set(i) {
                            in_feasible = true;
                            start = i;
                            end = i;
                        }
                    }
                    if in_feasible {
                        res.edges[sq_start].push((Some((start, end)), sq_end));
                    }
                    sq = None;
                    s = tail;
                }
                (None, [b'^', ..]) => panic!("ERROR: ^ not in []"),
                (Some(_), [b'^', ..]) if sq_not => panic!("ERROR: Multiple ^ in []"),
                (Some(_), [b'^', tail @ ..]) if !sq_not => {
                    sq_not = true;
                    s = tail;
                }
                (Some(_), [c, tail @ ..]) => {
                    sq_chars[*c as usize / 64] |= 1 << (c % 64);
                    s = tail;
                }
                (None, [c, tail @ ..]) => {
                    let mut c = *c;
                    if escaped {
                        c = escape_lookup[c as usize].unwrap_or(c);
                        escaped = false;
                    }
                    let new_node = res.edges.len();
                    res.edges[last.1].push((Some((c, c)), new_node));
                    last = (res.edges.len(), res.edges.len());
                    res.edges.push(vec![]);
                    s = tail;
                }
                _ => {}
            }
        }

        (res, last.1)
    }
}
