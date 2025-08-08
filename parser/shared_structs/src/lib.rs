mod lexer;
mod parser;
mod sets;

use core::panic;
pub use lexer::{DynTrie, RegexDFA, Trie, TrieNode};
pub use parser::{Conflict, DynParseTable, ParseAction, ParseTable};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::{fmt::Debug, mem::MaybeUninit};

use crate::lexer::RegexTable;

const ERR_INVALID_LEXEME: &'static str = "The lexing engine hit an invalid sequence.";
const ERR_REDUCED_NONTERMINAL_INVALID: &'static str =
    "Non-terminal mapped to a non-goto action in the parse-table.";
const ERR_STATE_STACK_EMPTY: &'static str = "Attempted to pop from an empty state stack.";
const ERR_STATE_STACK_NOT_EMPTY: &'static str =
    "The state stack was not empty when returning start rule.";
const ERR_NODE_STACK_NOT_EMPTY: &'static str =
    "The node stack was not empty when returning start rule.";
const ERR_TERMINAL_GOTO: &'static str = "A terminal mapped to a goto action in the parse table.";
const ERR_SYNTAX_ERR: &'static str = "Syntax error.";

pub const MAX_STATE_STACK: usize = 1024;

fn quote_option<T: ToTokens>(o: &Option<T>) -> TokenStream {
    if let Some(t) = o {
        quote! { Some(#t) }
    } else {
        quote! { None }
    }
}

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

pub struct CappedVec<const N: usize, T: Clone> {
    buf: [MaybeUninit<T>; N],
    len: usize,
}

impl<const N: usize, T: Debug + Clone> Debug for CappedVec<N, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(
                self.buf[..self.len]
                    .iter()
                    .map(|elem| unsafe { elem.assume_init_ref() }),
            )
            .finish()
    }
}

impl<const N: usize, T: Clone> CappedVec<N, T> {
    pub fn new() -> Self {
        let buf: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };
        Self { buf, len: 0 }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn last(&self) -> Option<&T> {
        self.buf
            .get(self.len - 1)
            .map(|t| unsafe { t.assume_init_ref() })
    }

    pub fn pop(&mut self) -> Option<T> {
        (self.len > 0).then(|| {
            let mut dest = MaybeUninit::uninit();
            self.len -= 1;
            std::mem::swap(&mut self.buf[self.len], &mut dest);
            unsafe { dest.assume_init() }
        })
    }

    pub fn push(&mut self, t: T) {
        if self.len == N {
            panic!("CappedVec hit its limit.")
        }
        self.buf[self.len] = MaybeUninit::new(t);
        self.len += 1;
    }
}

pub struct Engine<
    N: Clone,
    S,
    const TERMINALS: usize,
    const NUM_TOKENS: usize,
    const NUM_LITERALS: usize,
    const NUM_LEX_STATES: usize,
    const NUM_PARSE_STATES: usize,
    const NUM_RULES: usize,
> {
    pub parser: ParseTable<NUM_RULES, NUM_PARSE_STATES, NUM_TOKENS>,
    trie: Trie<NUM_LITERALS>,
    dfa: RegexTable<NUM_LEX_STATES>,
    // TODO: make callbacks not return Box. probably require break out lexeme enum
    lexeme_callbacks: [fn(&mut S, &str) -> N; TERMINALS],
    rule_callbacks: [fn(&mut CappedVec<MAX_STATE_STACK, N>) -> N; NUM_RULES],
    state: S,
}

impl<
        N: Debug + Clone,
        S: Debug,
        const TERMINALS: usize,
        const NUM_TOKENS: usize,
        const NUM_LITERALS: usize,
        const NUM_LEX_STATES: usize,
        const NUM_PARSE_STATES: usize,
        const NUM_RULES: usize,
    >
    Engine<N, S, TERMINALS, NUM_TOKENS, NUM_LITERALS, NUM_LEX_STATES, NUM_PARSE_STATES, NUM_RULES>
{
    pub fn from_raw(
        (actions, rule_lens): (
            [[(usize, usize); NUM_TOKENS]; NUM_PARSE_STATES],
            [(usize, usize); NUM_RULES],
        ),
        (trans, fin): (
            [[Option<usize>; 256]; NUM_LEX_STATES],
            [Option<(usize, usize)>; NUM_LEX_STATES],
        ),
        trie_raw: [(Option<(usize, usize)>, [Option<usize>; 256]); NUM_LITERALS],
        lexeme_callbacks: [fn(&mut S, &str) -> N; TERMINALS],
        rule_callbacks: [fn(&mut CappedVec<MAX_STATE_STACK, N>) -> N; NUM_RULES],
        state: S,
    ) -> Result<Self, &'static str> {
        Ok(Self {
            parser: ParseTable::from_raw(actions, rule_lens)?,
            trie: Trie::from_raw(trie_raw),
            dfa: RegexTable { trans, fin },
            lexeme_callbacks,
            rule_callbacks,
            state,
        })
    }

    pub fn parse(&mut self, node: usize, mut s: &str) -> Result<N, &'static str> {
        let mut cur_lexeme = self.lex(&mut s);
        let mut state_stack: CappedVec<MAX_STATE_STACK, usize> = CappedVec::new();
        state_stack.push(node);
        let mut node_stack = CappedVec::new();
        while let Ok((lexeme, lexeme_id)) = cur_lexeme.as_ref() {
            match self.parser.actions[*state_stack.last().unwrap()][*lexeme_id] {
                ParseAction::Shift(state) => {
                    state_stack.push(state);
                    if let Ok((Some(lexeme), _)) = cur_lexeme {
                        node_stack.push(lexeme);
                    }
                    cur_lexeme = self.lex(&mut s);
                }
                ParseAction::Reduce(rule) => {
                    let (rule_len, non_terminal) = self.parser.rule_lens[rule];
                    for _ in 0..rule_len {
                        state_stack.pop().ok_or(ERR_STATE_STACK_EMPTY)?;
                    }
                    let new_node = (self.rule_callbacks[rule])(&mut node_stack);
                    node_stack.push(new_node);
                    state_stack.push(
                        if let ParseAction::Goto(state) = self.parser.actions
                            [*state_stack.last().ok_or(ERR_STATE_STACK_EMPTY)?][non_terminal]
                        {
                            state
                        } else if non_terminal == node && lexeme.is_none() {
                            break;
                        } else {
                            return Result::Err(ERR_REDUCED_NONTERMINAL_INVALID);
                        },
                    )
                }
                ParseAction::Invalid => return Result::Err(ERR_SYNTAX_ERR),
                ParseAction::Goto(_) => return Result::Err(ERR_TERMINAL_GOTO),
            }
        }
        cur_lexeme?;
        if node_stack.len() != 1 {
            return Result::Err(ERR_NODE_STACK_NOT_EMPTY);
        } else if state_stack.len() != 1 {
            return Result::Err(ERR_STATE_STACK_NOT_EMPTY);
        } else {
            return Result::Ok(node_stack.pop().unwrap());
        }
    }

    fn lex(&mut self, s: &mut &str) -> Result<(Option<N>, usize), &'static str> {
        if s.is_empty() {
            return Ok((None, self.parser.actions[0].len() - 1));
        }
        let (trie_match, trie_len) = if let Some((trie_lexeme, trie_node, trie_len)) =
            self.trie.query_longest(&s.as_bytes())
        {
            (Some((trie_lexeme, trie_node)), trie_len)
        } else {
            (None, 0)
        };
        let bytes = s.as_bytes();
        let mut regex_match = None;
        let mut cur = 0;
        let mut regex_len = 0;
        while regex_len < bytes.len()
            && let Some(next) = self.dfa.trans[cur][bytes[regex_len] as usize]
        {
            if let Some(fin) = self.dfa.fin[next] {
                regex_match = Some(fin);
            }
            cur = next;
            regex_len += 1;
        }
        let (len, (lexeme_id, node_id)) = if let Some(trie_fin) = trie_match
            && let Some(regex_fin) = regex_match
        {
            Some((regex_len, regex_fin).max((trie_len, trie_fin)))
        } else {
            regex_match
                .map(|m| (regex_len, m))
                .or(trie_match.map(|m| (trie_len, m)))
        }
        .ok_or(ERR_INVALID_LEXEME)?;
        let lexeme = (self.lexeme_callbacks[lexeme_id])(&mut self.state, &s[0..len]);
        *s = &s[len..];
        Ok((Some(lexeme), node_id))
    }
}
