mod lexer;
mod parser;
mod sets;

use core::panic;
pub use lexer::{DynTrie, RegexDFA, Trie, TrieNode};
pub use parser::{Conflict, DynParseTable, ParseAction, ParseTable};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::{fmt::Debug, marker::PhantomData, mem::MaybeUninit};

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
    const NUM_TERMINALS: usize,
    const NUM_TOKENS: usize,
    const NUM_LITERALS: usize,
    const NUM_UPDATES: usize,
    const NUM_LEX_STATES: usize,
    const NUM_UPDATE_STATES: usize,
    const NUM_PARSE_STATES: usize,
    const NUM_RULES: usize,
> {
    parser: ParseTable<NUM_RULES, NUM_PARSE_STATES, NUM_TOKENS>,
    trie: Trie<NUM_LITERALS>,
    lexer: RegexTable<NUM_LEX_STATES>,
    lex_update: RegexTable<NUM_UPDATE_STATES>,
    lexeme_callbacks: [fn(&mut S, &str) -> N; NUM_TERMINALS],
    update_callbacks: [fn(&mut S, &str); NUM_UPDATES],
    rule_callbacks: [fn(&mut CappedVec<MAX_STATE_STACK, N>) -> N; NUM_RULES],
    is_terminal: [bool; NUM_TOKENS],
    phantom_lex_state: PhantomData<S>,
}

impl<
        N: Clone + Debug,
        S,
        const NUM_TERMINALS: usize,
        const NUM_TOKENS: usize,
        const NUM_LITERALS: usize,
        const NUM_UPDATES: usize,
        const NUM_LEX_STATES: usize,
        const NUM_UPDATE_STATES: usize,
        const NUM_PARSE_STATES: usize,
        const NUM_RULES: usize,
    >
    Engine<
        N,
        S,
        NUM_TERMINALS,
        NUM_TOKENS,
        NUM_LITERALS,
        NUM_UPDATES,
        NUM_LEX_STATES,
        NUM_UPDATE_STATES,
        NUM_PARSE_STATES,
        NUM_RULES,
    >
{
    pub fn from_raw(
        parser: (
            [[(usize, usize); NUM_TOKENS]; NUM_PARSE_STATES],
            [(usize, usize); NUM_RULES],
        ),
        lexer: (
            [[Option<usize>; 256]; NUM_LEX_STATES],
            [Option<(usize, usize)>; NUM_LEX_STATES],
        ),
        lex_update: (
            [[Option<usize>; 256]; NUM_UPDATE_STATES],
            [Option<(usize, usize)>; NUM_UPDATE_STATES],
        ),
        trie: [(Option<(usize, usize)>, [Option<usize>; 256]); NUM_LITERALS],
        lexeme_callbacks: [fn(&mut S, &str) -> N; NUM_TERMINALS],
        update_callbacks: [fn(&mut S, &str); NUM_UPDATES],
        rule_callbacks: [fn(&mut CappedVec<MAX_STATE_STACK, N>) -> N; NUM_RULES],
        is_terminal: [bool; NUM_TOKENS],
    ) -> Result<Self, &'static str> {
        Ok(Self {
            parser: ParseTable::from_raw(parser.0, parser.1)?,
            trie: Trie::from_raw(trie),
            lexer: RegexTable {
                trans: lexer.0,
                fin: lexer.1,
            },
            lex_update: RegexTable {
                trans: lex_update.0,
                fin: lex_update.1,
            },
            lexeme_callbacks,
            update_callbacks,
            rule_callbacks,
            is_terminal,
            phantom_lex_state: PhantomData,
        })
    }

    pub fn parse(&mut self, node: usize, mut s: &str, mut lex_state: S) -> Result<N, &'static str> {
        let mut cur_lexeme = self.lex(&mut s, &mut lex_state);
        if self.is_terminal[node]
            && let Ok((Some(_), lexeme_id)) = cur_lexeme.as_ref()
            && node == *lexeme_id
        {
            return Ok(cur_lexeme.unwrap().0.unwrap());
        } else if self.is_terminal[node] {
            return Err(ERR_SYNTAX_ERR);
        }
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
                    cur_lexeme = self.lex(&mut s, &mut lex_state);
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

    fn lex(&mut self, s: &mut &str, state: &mut S) -> Result<(Option<N>, usize), &'static str> {
        let (update_fin, update_len) = self.lex_update.query_longest(&s.as_bytes());
        if let Some((update_id, _)) = update_fin {
            (self.update_callbacks[update_id])(state, &s[0..update_len]);
            *s = &s[update_len..];
        }
        if s.is_empty() {
            return Ok((None, self.parser.actions[0].len() - 1));
        }
        let bytes = &s.as_bytes();
        let (trie_match, trie_len) = self.trie.query_longest(bytes);
        let (regex_match, regex_len) = self.lexer.query_longest(bytes);
        let (fin, len) = if trie_len > regex_len
            && let Some(fin) = trie_match
        {
            Ok((fin, trie_len))
        } else if regex_len > trie_len
            && let Some(fin) = regex_match
        {
            Ok((fin, regex_len))
        } else if regex_len == trie_len
            && let Some(regex_fin) = regex_match
            && let Some(trie_fin) = trie_match
        {
            if regex_fin.0 < trie_fin.0 {
                Ok((regex_fin, regex_len))
            } else {
                Ok((trie_fin, trie_len))
            }
        } else {
            Err(ERR_INVALID_LEXEME)
        }?;
        let lexeme = (self.lexeme_callbacks[fin.0])(state, &s[0..len]);
        *s = &s[len..];
        Ok((Some(lexeme), fin.1))
    }
}
