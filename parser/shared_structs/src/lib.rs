mod lexer;
mod parser;
mod sets;

use core::panic;
use std::fmt::Debug;
pub use lexer::{DynTrie, RegexDFA, Trie, TrieNode};
pub use parser::{Conflict, DynParseTable, ParseAction, ParseTable};
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};

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

pub struct Lexemes<
    'a,
    'b,
    'c,
    N: Clone,
    S,
    const NUM_TERMINALS: usize,
    const NUM_TOKENS: usize,
    const NUM_LITERALS: usize,
    const NUM_LEX_STATES: usize,
    const NUM_PARSE_STATES: usize,
    const NUM_RULES: usize,
    const NUM_ERROR_CALLBACKS: usize,
> {
    engine: &'a Engine<
        N,
        S,
        NUM_TERMINALS,
        NUM_TOKENS,
        NUM_LITERALS,
        NUM_LEX_STATES,
        NUM_PARSE_STATES,
        NUM_RULES,
        NUM_ERROR_CALLBACKS,
    >,
    pub state: &'b mut S,
    pub s: &'c str,
}

pub struct Engine<
    N: Clone,
    S,
    const NUM_TERMINALS: usize,
    const NUM_TOKENS: usize,
    const NUM_LITERALS: usize,
    const NUM_LEX_STATES: usize,
    const NUM_PARSE_STATES: usize,
    const NUM_RULES: usize,
    const NUM_ERRORS: usize,
> {
    parser: ParseTable<NUM_RULES, NUM_PARSE_STATES, NUM_TOKENS>,
    trie: Trie<NUM_LITERALS>,
    lexer: RegexTable<NUM_LEX_STATES>,
    lexeme_callbacks: [fn(&mut S, &str) -> Option<(N, usize)>; NUM_TERMINALS],
    error_callbacks: [fn(Vec<N>) -> N; NUM_ERRORS],
    rule_callbacks: [fn(&mut Vec<N>) -> N; NUM_RULES],
    is_terminal: [bool; NUM_TOKENS],
}

impl<
    N: Clone + Debug,
    S,
    const NUM_TERMINALS: usize,
    const NUM_TOKENS: usize,
    const NUM_LITERALS: usize,
    const NUM_LEX_STATES: usize,
    const NUM_PARSE_STATES: usize,
    const NUM_RULES: usize,
    const NUM_ERROR_CALLBACKS: usize,
>
    Engine<
        N,
        S,
        NUM_TERMINALS,
        NUM_TOKENS,
        NUM_LITERALS,
        NUM_LEX_STATES,
        NUM_PARSE_STATES,
        NUM_RULES,
        NUM_ERROR_CALLBACKS,
    >
{
    pub fn from_raw(
        parser: (
            [[(usize, usize); NUM_TOKENS]; NUM_PARSE_STATES],
            [(usize, usize); NUM_RULES],
            [Option<(usize, usize)>; NUM_PARSE_STATES],
            [[bool; NUM_TOKENS]; NUM_TOKENS],
        ),
        lexer: (
            [[Option<usize>; 256]; NUM_LEX_STATES],
            [Option<usize>; NUM_LEX_STATES],
        ),
        trie: [(Option<usize>, [Option<usize>; 256]); NUM_LITERALS],
        lexeme_callbacks: [fn(&mut S, &str) -> Option<(N, usize)>; NUM_TERMINALS],
        error_callbacks: [fn(Vec<N>) -> N; NUM_ERROR_CALLBACKS],
        rule_callbacks: [fn(&mut Vec<N>) -> N; NUM_RULES],
        is_terminal: [bool; NUM_TOKENS],
    ) -> Result<Self, &'static str> {
        Ok(Self {
            parser: ParseTable::from_raw(parser.0, parser.1, parser.2, parser.3)?,
            trie: Trie::from_raw(trie),
            lexer: RegexTable {
                trans: lexer.0,
                fin: lexer.1,
            },
            lexeme_callbacks,
            error_callbacks,
            rule_callbacks,
            is_terminal,
        })
    }

    pub fn parse(&mut self, node: impl Into<usize>, mut s: &str, lex_state: &mut S) -> Result<N, &'static str> {
        let node: usize = node.into();
        let mut cur_lexeme = self.lex(&mut s, lex_state);
        if self.is_terminal[node]
            && let Ok((Some(_), lexeme_id)) = cur_lexeme.as_ref()
            && node == *lexeme_id
        {
            return Ok(cur_lexeme.unwrap().0.unwrap());
        } else if self.is_terminal[node] {
            return Err(ERR_SYNTAX_ERR);
        }
        let mut state_stack: Vec<usize> = vec![];
        state_stack.push(node);
        let mut node_stack = vec!{};
        let mut error: Option<usize> = None;
        while let Ok((lexeme, lexeme_id)) = cur_lexeme.as_ref() {
            if let Some(nonterminal) = error {
                if self.parser.reductions[nonterminal][*lexeme_id] {
                    error = None;
                    let cur_state = *state_stack.last().unwrap();
                    if let ParseAction::Goto(state) = self.parser.actions[cur_state][*lexeme_id] {
                        state_stack.push(state);
                    } else {
                        panic!()
                    }
                } else {
                    cur_lexeme = self.lex(&mut s, lex_state);
                }
                continue;
            }
            match self.parser.actions[*state_stack.last().unwrap()][*lexeme_id] {
                ParseAction::Shift(state) => {
                    state_stack.push(state);
                    if let Ok((Some(lexeme), _)) = cur_lexeme {
                        node_stack.push(lexeme);
                    }
                    cur_lexeme = self.lex(&mut s, lex_state);
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
                ParseAction::Invalid => {
                    if error.is_none() {
                        return Err(ERR_SYNTAX_ERR);
                    }
                    let mut nodes = vec![];
                    let mut err_callback = None;
                    while let Some(state) = state_stack.last() && !node_stack.is_empty() {
                        if let Some((error_id, nonterminal)) = self.parser.errors[*state] {
                            error = Some(nonterminal);
                            err_callback = Some(error_id);
                            break;
                        }
                        nodes.push(node_stack.pop().unwrap());
                        state_stack.pop();
                    }
                    node_stack.push((self.error_callbacks[err_callback.unwrap()])(nodes));
                }
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

    pub fn lex(&self, s: &mut &str, state: &mut S) -> Result<(Option<N>, usize), &'static str> {
        let mut cur_lexeme = None; 
        while cur_lexeme.is_none() {
            if s.is_empty() {
                return Ok((None, NUM_TOKENS - 1));
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
                if regex_fin < trie_fin {
                    Ok((regex_fin, regex_len))
                } else {
                    Ok((trie_fin, trie_len))
                }
            } else {
                Err(ERR_INVALID_LEXEME)
            }?;
            cur_lexeme = (self.lexeme_callbacks[fin])(state, &s[0..len]);
            *s = &s[len..];
        }
        let (lexeme, id) = cur_lexeme.unwrap();
        Ok((Some(lexeme), id))
    }

    pub fn lexemes<'a, 'b, 'c>(
        &'a self,
        s: &'c str,
        state: &'b mut S,
    ) -> Lexemes<
        'a,
        'b,
        'c,
        N,
        S,
        NUM_TERMINALS,
        NUM_TOKENS,
        NUM_LITERALS,
        NUM_LEX_STATES,
        NUM_PARSE_STATES,
        NUM_RULES,
        NUM_ERROR_CALLBACKS,
    > {
        Lexemes {
            engine: self,
            s,
            state,
        }
    }
}

impl<
    'a,
    'b,
    'c,
    N: Clone + Debug,
    S,
    const NUM_TERMINALS: usize,
    const NUM_TOKENS: usize,
    const NUM_LITERALS: usize,
    const NUM_LEX_STATES: usize,
    const NUM_PARSE_STATES: usize,
    const NUM_RULES: usize,
    const NUM_ERROR_CALLBACKS: usize,
> Iterator
    for Lexemes<
        'a,
        'b,
        'c,
        N,
        S,
        NUM_TERMINALS,
        NUM_TOKENS,
        NUM_LITERALS,
        NUM_LEX_STATES,
        NUM_PARSE_STATES,
        NUM_RULES,
        NUM_ERROR_CALLBACKS,
    >
{
    type Item = Result<(N, usize), &'static str>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.engine.lex(&mut self.s, self.state) {
            Ok((Some(l), id)) => Some(Ok((l, id))),
            Ok((None, _)) => None,
            Err(e) => Some(Err(e)),
        }
    }
}
