use std::{collections::BTreeMap, fmt::Debug};

use proc_macro2::{Punct, Spacing, TokenStream};
use quote::{ToTokens, TokenStreamExt, quote};

use crate::USizeSet;

#[derive(Clone, Copy)]
pub enum ParseAction {
    Shift(usize),
    Goto(usize),
    Reduce(usize),
    Invalid,
}

impl Debug for ParseAction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            ParseAction::Shift(state) => write!(f, "Shift({state})"),
            ParseAction::Goto(state) => write!(f, "Goto({state})"),
            ParseAction::Reduce(prod) => write!(f, "Reduce({prod})"),
            ParseAction::Invalid => write!(f, "Invalid"),
        }
    }
}

impl ToTokens for ParseAction {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(match self {
            ParseAction::Shift(state) => quote! { shared_structs::ParseAction::Shift(#state) },
            ParseAction::Goto(state) => quote! { shared_structs::ParseAction::Goto(#state) },
            ParseAction::Reduce(rule) => quote! { shared_structs::ParseAction::Reduce(#rule) },
            ParseAction::Invalid => quote! { shared_structs::ParseAction::Invalid },
        });
    }
}

#[derive(Debug)]
pub struct ParseTable {
    pub actions: Vec<Vec<ParseAction>>,
    pub rule_lens: Vec<(usize, usize)>,
}

impl ParseTable {
    pub fn from_rules(rules: Vec<Vec<Vec<usize>>>, start: usize) -> Self {
        let dfa = ParseDFA::from_rules(rules, start);
        let mut actions = vec![vec![ParseAction::Invalid; dfa.rules.len()]; dfa.states.len()];
        let state_ids: BTreeMap<(usize, usize, usize), usize> = dfa
            .states
            .iter()
            .enumerate()
            .map(|(i, (s, _))| (*s, i))
            .collect();
        let mut production_ids = vec![];
        let mut rule_lens = vec![];
        let mut i = 0;
        for (prod, prod_rules) in dfa.rules.iter().enumerate() {
            production_ids.push(vec![]);
            for rule in prod_rules.iter() {
                production_ids.last_mut().unwrap().push(i);
                rule_lens.push((rule.len(), prod));
                i += 1;
            }
        }
        for (state, (_, lookaheads)) in dfa.states {
            let id = *state_ids.get(&state).unwrap();
            for (item, next) in dfa.trans.get(&state).unwrap() {
                actions[id][*item] = if dfa.rules[*item].len() == 0 {
                    ParseAction::Shift(*state_ids.get(next).unwrap())
                } else {
                    ParseAction::Goto(*state_ids.get(next).unwrap())
                };
            }
            if state.2 == dfa.rules[state.0][state.1].len() {
                for item in lookaheads
                    .iter()
                    .flat_map(|(k, _)| dfa.lookaheads[*k].iter())
                {
                    let production = production_ids[state.0][state.1];
                    actions[id][item] = ParseAction::Reduce(production);
                }
            }
        }
        Self { actions, rule_lens }
    }

    pub fn from_raw(actions: Vec<Vec<ParseAction>>, rule_lens: Vec<(usize, usize)>) -> Self {
        Self { actions, rule_lens }
    }
}

impl ToTokens for ParseTable {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let mut actions_inner = TokenStream::new();
        actions_inner.append_separated(
            self.actions.iter().map(|state_actions| {
                let mut state_inner = TokenStream::new();
                state_inner.append_separated(state_actions.iter(), Punct::new(',', Spacing::Alone));
                quote! {
                    vec![#state_inner]
                }
            }),
            Punct::new(',', Spacing::Alone),
        );
        let mut rule_lens_inner = TokenStream::new();
        rule_lens_inner.append_separated(
            self.rule_lens
                .iter()
                .map(|(len, nt)| quote! { (#len, #nt) }),
            Punct::new(',', Spacing::Alone),
        );
        tokens.append_all(quote! {
            shared_structs::ParseTable::from_raw(vec![#actions_inner], vec![#rule_lens_inner])
        });
    }
}

#[derive(Debug)]
struct ParseDFA {
    // (seed rule (lhs, rhs, pos)) -> ([derived rules] , [seed lookahead -> derived lookaheads])
    states: BTreeMap<(usize, usize, usize), (Vec<(usize, usize)>, BTreeMap<usize, Vec<usize>>)>,
    lookaheads: Vec<USizeSet>,
    rules: Vec<Vec<Vec<usize>>>,
    // {seed rule -> [token, next_seed]}
    trans: BTreeMap<(usize, usize, usize), Vec<(usize, (usize, usize, usize))>>,
}

impl ParseDFA {
    fn from_rules(mut rules: Vec<Vec<Vec<usize>>>, start: usize) -> Self {
        let start_rule = rules.len() - 1;
        rules[0] = vec![vec![start]];
        let mut terminals = USizeSet::default();
        for (non_terminal, _) in rules.iter().enumerate().filter(|(_, l)| !l.is_empty()) {
            terminals.set(non_terminal, true);
        }
        terminals = !terminals;

        let mut firsts: Vec<(bool, USizeSet)> = vec![(false, USizeSet::default()); rules.len()];
        let mut vis = vec![false; rules.len()];
        let mut stack = vec![start_rule];
        while let Some(cur) = stack.pop() {
            if terminals.get(cur) {
                firsts[cur].1.set(cur, true);
                vis[cur] = true;
            }
            let mut deps = rules[cur]
                .iter()
                .flat_map(|rule| {
                    rule.into_iter().scan(false, |should_continue, component| {
                        let result = should_continue.then_some(*component);
                        *should_continue &= vis[*component] && firsts[*component].0;
                        result
                    })
                })
                .collect::<USizeSet>();
            deps.set(cur, false);
            let mut unfinished_deps = deps.iter().filter(|component| !vis[*component]).peekable();
            if unfinished_deps.peek().is_some() {
                stack.push(cur); // search cur after ful-filling all deps
                stack.extend(unfinished_deps);
            } else {
                for dep in deps.iter() {
                    if let Ok([dst, src]) = firsts.get_disjoint_mut([cur, dep]) {
                        dst.1 |= &src.1;
                    }
                }
                firsts[cur].0 = rules[cur]
                    .iter()
                    .any(|rule| rule.iter().all(|component| firsts[*component].0));
                vis[cur] = true;
            }
        }

        let mut states = BTreeMap::new();
        let mut lookaheads = vec![USizeSet::from([1])]; // EOF only
        let mut stack = vec![((0, 0, 0), 0)];
        let mut trans: BTreeMap<(usize, usize, usize), Vec<(usize, (usize, usize, usize))>> =
            BTreeMap::new();
        let get_lookahead =
            |cur: usize, i: usize, start: usize, cur_lookahead: &USizeSet| -> USizeSet {
                let mut should_continue = true;
                let mut result = USizeSet::from([0]);
                let mut j = start;
                while should_continue && j < rules[cur][i].len() {
                    should_continue = firsts[rules[cur][i][j]].0;
                    result |= &firsts[rules[cur][i][j]].1;
                    j += 1;
                }
                if should_continue {
                    result |= cur_lookahead;
                }
                result
            };
        while let Some((seed, seed_lookahead)) = stack.pop() {
            if trans.contains_key(&seed) {
                continue;
            }
            let mut derived_lookaheads = vec![];
            let mut derived_stack = vec![(
                rules[seed.0][seed.1][seed.2],
                get_lookahead(seed.0, seed.1, seed.2, &lookaheads[seed_lookahead]),
            )];
            let mut vis = vec![false; rules.len()];
            let mut derived_rules = vec![];
            let new_seed = !states.contains_key(&seed);
            while let Some((cur, cur_lookahead)) = derived_stack.pop() {
                vis[cur] = true;
                for (i, rule) in rules[cur].iter().enumerate() {
                    derived_lookaheads.push(lookaheads.len());
                    if new_seed {
                        derived_rules.push((cur, i));
                    }
                    if !vis[rule[0]] && !terminals.get(rule[0]) {
                        derived_stack.push((rule[0], get_lookahead(cur, i, 1, &cur_lookahead)));
                    }
                }
                lookaheads.push(cur_lookahead);
            }
            if new_seed {
                states.insert(seed, (derived_rules, BTreeMap::new()));
            }
            let new_seed = (seed.0, seed.1, seed.2 + 1);
            trans.insert(seed, vec![(rules[seed.0][seed.1][seed.2], new_seed)]);
            if seed.2 != rules[seed.0][seed.1].len() - 1 {
                stack.push((new_seed, seed_lookahead));
            } else {
                states.insert(
                    new_seed,
                    (vec![], BTreeMap::from([(seed_lookahead, vec![])])),
                );
                trans.insert(new_seed, vec![]);
            }
            let (derived_rules, seed_lookaheads) = states.get_mut(&seed).unwrap();
            for (rule, derived_lookahead) in
                derived_rules.into_iter().zip(derived_lookaheads.iter())
            {
                let new_seed = (rule.0, rule.1, 0);
                trans
                    .get_mut(&seed)
                    .unwrap()
                    .push((rules[rule.0][rule.1][0], new_seed));
                stack.push((new_seed, *derived_lookahead))
            }
            seed_lookaheads.insert(seed_lookahead, derived_lookaheads);
        }

        Self {
            rules,
            lookaheads,
            states,
            trans,
        }
    }
}
