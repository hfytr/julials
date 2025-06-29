use std::{collections::BTreeMap, marker::PhantomPinned, pin::Pin, rc::Rc};

use crate::USizeSet;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct AugmentedRule {
    components: (usize, usize),
    lookahead: USizeSet,
    _pin: PhantomPinned,
}

impl AugmentedRule {
    fn new(components: (usize, usize), lookahead: USizeSet) -> Pin<Rc<Self>> {
        Rc::pin(AugmentedRule {
            components,
            lookahead,
            _pin: PhantomPinned,
        })
    }
}

struct ParseDFA {
    rules: Vec<Vec<Vec<usize>>>,
    states: Vec<Vec<(usize, Pin<Rc<AugmentedRule>>)>>,
    trans: Vec<Vec<usize>>,
}

impl ParseDFA {
    fn from_rules(mut rules: Vec<Vec<Vec<usize>>>, start: usize) -> Self {
        let start_rule = rules.len();
        rules.push(vec![vec![start]]);
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
                        *should_continue &= vis[*component] && firsts[*component].0;
                        should_continue.then_some(*component)
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

        let start_rule = (0, AugmentedRule::new((0, 0), USizeSet::from([0])));
        let mut added_states = BTreeMap::from([(clone_rule(&start_rule), 0)]);
        let mut states = vec![Self::production_to_state(&rules, &firsts, start_rule)];
        let mut stack = vec![0];
        let mut trans = vec![vec![]];
        while let Some(cur) = stack.pop() {
            for i in 0..states[cur].len() {
                let (pos, production) = clone_rule(&states[cur][i]);
                if rules[production.components.0][production.components.1].len() == pos {
                    continue;
                }
                let next = (pos + 1, production);
                trans[cur].push(added_states.get(&next).map(|i| *i).unwrap_or_else(|| {
                    added_states.insert(clone_rule(&next), states.len());
                    states.push(Self::production_to_state(&rules, &firsts, next));
                    states.len() - 1
                }));
                if *trans[cur].last().unwrap() == states.len() - 1 {
                    trans.push(vec![]);
                }
            }
        }

        Self {
            rules,
            states,
            trans,
        }
    }

    fn production_to_state(
        rules: &Vec<Vec<Vec<usize>>>,
        firsts: &Vec<(bool, USizeSet)>,
        rule: (usize, Pin<Rc<AugmentedRule>>),
    ) -> Vec<(usize, Pin<Rc<AugmentedRule>>)> {
        if rule.0 == rules[rule.1.components.0][rule.1.components.1].len() {
            return vec![rule];
        }
        let mut stack = vec![rule];
        let mut result = vec![];
        while let Some(cur) = stack.pop() {
            let cur_rule = &rules[cur.1.components.0][cur.1.components.1];
            for (i, rule) in rules[cur_rule[cur.0]].iter().enumerate() {
                let mut lookahead = USizeSet::default();
                let mut should_continue = true;
                let mut j = cur.0 + 1;
                while should_continue && j < rule.len() {
                    lookahead |= &firsts[rule[j]].1;
                    should_continue = firsts[rule[j]].0;
                    j += 1;
                }
                if should_continue {
                    lookahead |= &cur.1.lookahead;
                }
                stack.push((0, AugmentedRule::new((rule[cur.0], i), lookahead)));
            }
            result.push(cur);
        }
        result
    }
}

// helps when grepping for clones to remove
fn clone_rule<T>(rc: &(usize, Pin<Rc<T>>)) -> (usize, Pin<Rc<T>>) {
    rc.clone()
}
