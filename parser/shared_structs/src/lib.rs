mod lexer;
mod parser;
mod sets;

pub use lexer::{RegexDFA, Trie, TrieNode};
pub use parser::{Conflict, ParseAction, ParseTable};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::fmt::Debug;

const ERR_INVALID_LEXEME: &'static str = "The lexing engine hit an invalid sequence.";
const ERR_REDUCED_NONTERMINAL_INVALID: &'static str =
    "Non-terminal mapped to a non-goto action in the parse-table.";
const ERR_NO_MORE_LEXEMES: &'static str = "The lexer was called after returning EOF.";
const ERR_EXTRA_TOKENS: &'static str =
    "Extra tokens were encountered after parsing the start production";
const ERR_STATE_STACK_EMPTY: &'static str = "Attempted to pop from an empty state stack.";
const ERR_STATE_STACK_NOT_EMPTY: &'static str =
    "The state stack was not empty when returning start rule.";
const ERR_NODE_STACK_EMPTY: &'static str = "Attempted to pop from an empty node stack.";
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

pub struct Engine<'a, 'b, N, S> {
    parser: ParseTable,
    trie: Trie,
    dfa: RegexDFA,
    // TODO: make callbacks not return Box. probably require break out lexeme enum
    lexeme_callbacks: Vec<Box<dyn Fn(&mut S, &str) -> Box<N>>>,
    rule_callbacks: Vec<Box<dyn Fn(Vec<Box<N>>) -> Box<N>>>,
    state: S,
    done: bool,
    pub s: &'a mut &'b str,
    // for user dbg
    pub state_stack: Vec<usize>,
    pub node_stack: Vec<Box<N>>,
}

impl<N: Debug, S: Debug> Debug for Engine<'_, '_, N, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Engine(")?;
        self.trie.fmt(f)?;
        f.write_str(",\n")?;
        self.dfa.fmt(f)?;
        f.write_str(",\n")?;
        self.state.fmt(f)?;
        f.write_str("\n")?;
        self.parser.fmt(f)?;
        f.write_str("),\n")?;
        self.s.fmt(f)?;
        f.write_str("),\n")
    }
}

impl<'a, 'b, N: Debug, S> Engine<'a, 'b, N, S> {
    pub fn from_raw(
        (actions, rule_lens): (Vec<Vec<(usize, usize)>>, Vec<(usize, usize)>),
        dfa: (
            Vec<(Vec<u64>, ())>,
            Vec<[Option<usize>; 256]>,
            Vec<Option<(usize, usize)>>,
        ),
        trie_raw: Vec<(Option<(usize, usize)>, [Option<usize>; 256])>,
        lexeme_callbacks: Vec<Box<dyn Fn(&mut S, &str) -> Box<N>>>,
        rule_callbacks: Vec<Box<dyn Fn(Vec<Box<N>>) -> Box<N>>>,
        state: S,
        s: &'a mut &'b str,
    ) -> Result<Self, &'static str> {
        Ok(Self {
            parser: ParseTable::from_raw(actions, rule_lens)?,
            trie: Trie::from_raw(trie_raw),
            dfa: RegexDFA::from_raw(dfa),
            lexeme_callbacks,
            rule_callbacks,
            state,
            done: false,
            s,
            node_stack: vec![],
            state_stack: vec![0],
        })
    }

    pub fn parse(&mut self) -> Result<Box<N>, &'static str> {
        let mut cur_lexeme = self.lex();
        while let Ok((_, lexeme_id)) = cur_lexeme.as_ref() {
            match self.parser.actions[*self.state_stack.last().unwrap()][*lexeme_id] {
                ParseAction::Shift(state) => {
                    self.state_stack.push(state);
                    if let Ok((Some(lexeme), _)) = cur_lexeme {
                        self.node_stack.push(lexeme);
                    }
                    cur_lexeme = self.lex();
                }
                ParseAction::Reduce(rule) => {
                    let (rule_len, non_terminal) = self.parser.rule_lens[rule];
                    for _ in 0..rule_len {
                        self.state_stack.pop().ok_or(ERR_STATE_STACK_EMPTY)?;
                    }
                    let children = self.node_stack.split_off(
                        self.node_stack
                            .len()
                            .checked_sub(rule_len)
                            .ok_or(ERR_NODE_STACK_EMPTY)?,
                    );
                    self.node_stack
                        .push((self.rule_callbacks[rule])(children));
                    if non_terminal == 0 {
                        if self.node_stack.len() != 1 {
                            return Result::Err(ERR_NODE_STACK_NOT_EMPTY);
                        } else if self.state_stack.len() != 1 {
                            return Result::Err(ERR_STATE_STACK_NOT_EMPTY);
                        } else if self.lex().is_ok() {
                            return Result::Err(ERR_EXTRA_TOKENS);
                        } else {
                            return Result::Ok(self.node_stack.pop().unwrap());
                        }
                    }
                    self.state_stack.push(
                        if let ParseAction::Goto(state) = self.parser.actions
                            [*self.state_stack.last().ok_or(ERR_STATE_STACK_EMPTY)?][non_terminal]
                        {
                            state
                        } else {
                            return Result::Err(ERR_REDUCED_NONTERMINAL_INVALID);
                        },
                    )
                }
                ParseAction::Invalid => return Result::Err(ERR_SYNTAX_ERR),
                ParseAction::Goto(_) => return Result::Err(ERR_TERMINAL_GOTO),
            }
        }
        Err(cur_lexeme.unwrap_err())
    }

    pub fn lex(&mut self) -> Result<(Option<Box<N>>, usize), &'static str> {
        if self.s.is_empty() && !self.done {
            self.done = true;
            return Ok((None, self.parser.actions[0].len() - 1));
        } else if self.s.is_empty() {
            return Err(ERR_NO_MORE_LEXEMES);
        }
        let (trie_match, trie_len) = if let Some((trie_lexeme, trie_node, trie_len)) =
            self.trie.query_longest(&self.s.as_bytes())
        {
            (Some((trie_lexeme, trie_node)), trie_len)
        } else {
            (None, 0)
        };
        let bytes = self.s.as_bytes();
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
        let lexeme = (self.lexeme_callbacks[lexeme_id])(&mut self.state, &self.s[0..len]);
        *self.s = &self.s[len..];
        Ok((Some(lexeme), node_id))
    }
}
