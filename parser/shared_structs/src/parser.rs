use std::{collections::BTreeMap, fmt::Debug};

use proc_macro2::{Punct, Spacing, TokenStream};
use quote::{ToTokens, TokenStreamExt, quote};

use crate::sets::USizeSet;

const ERR_INVALID_PA_ID: &'static str = "Parse actions must have kind 0 / 1 / 2 / 3";

pub const PA_ID_SHIFT: usize = 0;
pub const PA_ID_GOTO: usize = 1;
pub const PA_ID_REDUCE: usize = 2;
pub const PA_ID_INVALID: usize = 3;

type SeedRule = (usize, usize, usize);

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
        let (pa_kind, pa_val) = match self {
            ParseAction::Shift(state) => (PA_ID_SHIFT, *state),
            ParseAction::Goto(state) => (PA_ID_GOTO, *state),
            ParseAction::Reduce(rule) => (PA_ID_REDUCE, *rule),
            ParseAction::Invalid => (PA_ID_INVALID, 0),
        };
        tokens.append_all(quote! { (#pa_kind, #pa_val) });
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
        let state_ids: BTreeMap<(&SeedRule, &USizeSet), usize> = dfa
            .states.iter()
            .enumerate()
            .map(|(i, ((seed, lookahead), _))| ((seed, lookahead), i))
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
        for ((seed, lookahead), trans) in dfa.states.iter() {
            let id = *state_ids.get(&(seed, lookahead)).unwrap();
            for (item, next, next_lookahead) in trans {
                actions[id][*item] = if dfa.rules[*item].len() == 0 {
                    ParseAction::Shift(*state_ids.get(&(&next, &next_lookahead)).unwrap())
                } else {
                    ParseAction::Goto(*state_ids.get(&(&next, &next_lookahead)).unwrap())
                };
            }
            if seed.2 == dfa.rules[seed.0][seed.1].len() {
                for item in lookahead.iter() {
                    let production = production_ids[seed.0][seed.1];
                    actions[id][item] = ParseAction::Reduce(production);
                }
            }
        }
        Self { actions, rule_lens }
    }

    pub fn from_raw(
        actions_raw: Vec<Vec<(usize, usize)>>,
        rule_lens: Vec<(usize, usize)>,
    ) -> Result<Self, &'static str> {
        let actions = actions_raw
            .into_iter()
            .map(|state| {
                state
                    .into_iter()
                    .map(|(action_type, value)| match action_type {
                        PA_ID_SHIFT => Ok(ParseAction::Shift(value)),
                        PA_ID_GOTO => Ok(ParseAction::Goto(value)),
                        PA_ID_REDUCE => Ok(ParseAction::Reduce(value)),
                        PA_ID_INVALID => Ok(ParseAction::Invalid),
                        _ => Result::Err(ERR_INVALID_PA_ID),
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self { actions, rule_lens })
    }
}

impl ToTokens for ParseTable {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let mut actions_inner = TokenStream::new();
        actions_inner.append_separated(
            self.actions.iter().map(|state_actions| {
                let mut state_inner = TokenStream::new();
                state_inner.append_separated(state_actions.iter(), Punct::new(',', Spacing::Alone));
                quote! { vec![#state_inner] }
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
        tokens.append_all(quote! { (vec![#actions_inner], vec![#rule_lens_inner]) });
    }
}

type Trans = Vec<(usize, SeedRule, USizeSet)>;

#[derive(Debug)]
struct ParseDFA {
    states: BTreeMap<(SeedRule, USizeSet), Vec<(usize, SeedRule, USizeSet)>>,
    rules: Vec<Vec<Vec<usize>>>,
}

// TODO: allow epsilon rules
fn get_firsts(rules: &Vec<Vec<Vec<usize>>>) -> Vec<USizeSet> {
    fn helper(
        firsts: &mut Vec<USizeSet>,
        vis: &mut Vec<bool>,
        rules: &Vec<Vec<Vec<usize>>>,
        cur: usize,
    ) {
        vis[cur] = true;
        if rules[cur].is_empty() {
            firsts[cur].set(cur, true);
            return;
        }
        for dep in rules[cur]
            .iter()
            .filter_map(|prod| prod.first().map(|x| *x))
        {
            if !vis[dep] {
                helper(firsts, vis, rules, dep);
            }
            let [src, dst] = firsts.get_disjoint_mut([cur, dep]).unwrap();
            *src |= dst;
        }
    }

    let mut firsts: Vec<USizeSet> = vec![USizeSet::default(); rules.len()];
    let mut vis = vec![false; rules.len()];
    for node in 0..rules.len() {
        if !vis[node] {
            helper(&mut firsts, &mut vis, &rules, node);
        }
    }
    firsts
}

fn get_derived_rules(rules: &Vec<Vec<Vec<usize>>>, start: usize) -> Vec<(usize, usize)> {
    fn helper(
        cur: usize,
        result: &mut Vec<(usize, usize)>,
        vis: &mut Vec<bool>,
        rules: &Vec<Vec<Vec<usize>>>,
    ) {
        vis[cur] = true;
        for (i, rule) in rules[cur].iter().enumerate() {
            result.push((cur, i));
            if !vis[rule[0]] && !rules[rule[0]].is_empty() {
                helper(rule[0], result, vis, rules);
            }
        }
    }
    let mut result = vec![];
    let mut vis = vec![false; rules.len()];
    helper(start, &mut result, &mut vis, &rules);
    result
}

fn get_derived_lookaheads(
    seed: SeedRule,
    seed_lookahead: &USizeSet,
    firsts: &Vec<USizeSet>,
    rules: &Vec<Vec<Vec<usize>>>,
) -> Vec<Option<USizeSet>> {
    fn helper(
        cur: usize,
        cur_lookahead: &USizeSet,
        firsts: &Vec<USizeSet>,
        result: &mut Vec<Option<USizeSet>>,
        rules: &Vec<Vec<Vec<usize>>>,
    ) {
        if let Some(ref mut cur_result) = result[cur] {
            *cur_result |= cur_lookahead
        } else {
            result[cur] = Some(cur_lookahead.clone());
        }
        for (i, rule) in rules[cur].iter().enumerate() {
            let next_lookahead = rule
                .get(i + 1)
                .map(|n| &firsts[*n])
                .unwrap_or(cur_lookahead);
            if !result[rule[0]]
                .as_ref()
                .map(|next_result| !next_lookahead.is_subset(next_result))
                .unwrap_or(false)
                && rules[rule[0]].len() > 0
            {
                helper(rule[0], &next_lookahead, firsts, result, rules);
            }
        }
    }
    let mut result = vec![None; rules.len()];
    helper(
        rules[seed.0][seed.1][seed.2],
        seed_lookahead,
        firsts,
        &mut result,
        rules,
    );
    result
}

impl ParseDFA {
    fn from_rules(mut rules: Vec<Vec<Vec<usize>>>, start: usize) -> Self {
        rules[0] = vec![vec![start]];
        let firsts = get_firsts(&rules);

        let mut states: BTreeMap<(SeedRule, USizeSet), Trans> = BTreeMap::new();
        let mut stack = vec![((0, 0, 0), firsts.last().unwrap().clone())];
        let derived_rules = (0..rules.len()).map(|i| get_derived_rules(&rules, i)).collect::<Vec<_>>();
        while let Some((seed, seed_lookahead)) = stack.pop() {
            if seed.2 == rules[seed.0][seed.1].len() {
                states.insert((seed, seed_lookahead), vec![]);
                continue;
            } else if states.get(&(seed, seed_lookahead.clone())).is_some() {
                continue;
            }
            let mut cur_trans = vec![];
            let next_seed = (seed.0, seed.1, seed.2 + 1);
            cur_trans.push((
                rules[seed.0][seed.1][seed.2],
                next_seed,
                seed_lookahead.clone(),
            ));
            stack.push((next_seed, seed_lookahead.clone()));
            let derived_lookaheads = get_derived_lookaheads(seed, &seed_lookahead, &firsts, &rules);
            for rule in derived_rules[rules[seed.0][seed.1][seed.2]].iter() {
                let new_seed = (rule.0, rule.1, 1);
                cur_trans.push((
                    rules[rule.0][rule.1][0],
                    new_seed,
                    derived_lookaheads[rule.0].clone().unwrap(),
                ));
                stack.push((new_seed, derived_lookaheads[rule.0].clone().unwrap()))
            }
            states.insert((seed, seed_lookahead), cur_trans);
        }

        Self { rules, states }
    }
}
