#![feature(iter_intersperse)]

mod lexer;
mod parser;
mod sets;

pub use lexer::{RegexDFA, Trie, TrieNode};
pub use sets::{IndexableSet, USizeSet};
use std::fmt::Debug;

fn fmt_maybe_arr(f: &mut std::fmt::Formatter<'_>, a: &[Option<usize>; 256]) -> std::fmt::Result {
    f.write_str("[")?;
    for (i, e) in a
        .iter()
        .enumerate()
        .filter_map(|(i, maybe_e)| maybe_e.map(|e| (i, e)))
    {
        write!(f, "{}: {:?}, ", i, e)?
    }
    f.write_str("]")
}

pub struct Engine<'a, 'b, L: Default, S> {
    trie: Trie,
    dfa: RegexDFA,
    callbacks: Vec<Box<dyn Fn(&mut S, &str) -> L>>,
    state: S,
    s: &'a mut &'b str,
}

impl<L: Debug + Default, S: Debug> Debug for Engine<'_, '_, L, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Engine(")?;
        self.trie.fmt(f)?;
        f.write_str("\n")?;
        self.dfa.fmt(f)?;
        f.write_str("\n")?;
        self.state.fmt(f)?;
        f.write_str("\n")?;
        self.s.fmt(f)?;
        f.write_str(")\n")
    }
}

impl<'a, 'b, L: Default, S> Engine<'a, 'b, L, S> {
    pub fn from_raw(
        dfa: RegexDFA,
        trie: Trie,
        callbacks: Vec<Box<dyn Fn(&mut S, &str) -> L>>,
        state: S,
        s: &'a mut &'b str,
    ) -> Self {
        Self {
            trie,
            callbacks,
            dfa,
            state,
            s,
        }
    }

    pub fn query(&mut self) -> Option<L> {
        if self.s.is_empty() {
            return Some(L::default());
        }
        let (trie_match, trie_len) =
            if let Some((trie_match, trie_fin)) = self.trie.query_longest(&self.s.as_bytes()) {
                (Some(trie_match), trie_fin)
            } else {
                (None, 0)
            };
        let bytes = self.s.as_bytes();
        let mut regex_match = None;
        let mut cur = 0;
        let mut regex_len = 0;
        while let Some(next) = self.dfa.trans[cur][bytes[regex_len] as usize] {
            if let Some(fin) = self.dfa.fin[cur] {
                regex_match = Some(fin);
            }
            cur = next;
            regex_len += 1;
        }
        let (callback, len) = if let Some(trie_fin) = trie_match
            && let Some(regex_fin) = regex_match
        {
            Some((regex_len, regex_fin).max((trie_len, trie_fin)))
        } else {
            regex_match
                .map(|m| (m, regex_len))
                .or(trie_match.map(|m| (m, trie_len)))
        }?;
        let res = (self.callbacks[callback])(&mut self.state, &self.s[0..len]);
        *self.s = &self.s[len..];
        Some(res)
    }
}
