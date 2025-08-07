use std::{collections::BTreeMap, fmt::Debug};

use quote::ToTokens;
use shared_structs::{Conflict, DynParseTable, DynTrie, RegexDFA, TrieNode};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    Error, ExprClosure, Ident, LitStr, Token,
};

use crate::Context;

const ERR_INCORRECT_TOKEN_SPEC: &'static str = r#"ERROR: Provide each token in the form
    "Literal(<pattern>) or "Regex(<pattern>)"
    where pattern is a single string literal"#;
const ERR_INCORRECT_PROD: &'static str = r#"ERROR: Each parsing rule should be of the form:
    RULE_NAME => Rule(<component 1> <component 2> ... <component n>) <optional callback>"#;
const ERR_MISSING_PROD_TYPE: &'static str =
    "ERROR: Expected Literal | Regex | Rule after => in new production definition.";
const ERR_MISSING_PATT: &'static str =
    "ERROR: Expected parenthesized string pattern after Regex | Literal.";
const ERR_PROD_NO_OUT_TYPE: &'static str =
    "ERROR: Every production must start with <OutputType>::ProductionName =>";
const ERR_NO_CALLBACK: &'static str =
    "ERROR: Literal | Regex require a callback to be specified after the pattern";

pub enum ProductionType {
    Regex(String, ExprClosure),
    Literal(String, ExprClosure),
    Rule(Vec<(Vec<Ident>, Option<ExprClosure>)>),
}

impl Debug for ProductionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn callback_string(callback: &Option<ExprClosure>) -> Option<String> {
            callback.as_ref().map(|c| c.to_token_stream().to_string())
        }
        match self {
            ProductionType::Regex(patt, callback) => write!(
                f,
                "ProductionType::Regex {{\n\tpattern: {patt},\n\tcallback: {}",
                callback.to_token_stream().to_string()
            )?,
            ProductionType::Literal(patt, callback) => write!(
                f,
                "ProductionType::Literal {{\n\tpattern: {patt},\n\tcallback: {}",
                callback.to_token_stream().to_string()
            )?,
            ProductionType::Rule(prods) => {
                write!(f, "ProductionType::Rule([")?;
                for (prod, callback) in prods {
                    write!(f, "\n\tproduction: [")?;
                    for (i, item) in prod.iter().enumerate() {
                        write!(f, "{}", item.to_string())?;
                        if i != prod.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, "] => {:?},", callback_string(callback))?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Production {
    pub name: Ident,
    pub name_raw: String,
    pub prod_type: ProductionType,
}

impl Parse for Production {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse().context(ERR_PROD_NO_OUT_TYPE)?;
        input.parse::<Token![=>]>().context(ERR_PROD_NO_OUT_TYPE)?;
        let prod_type_raw = input.parse::<Ident>().context(ERR_MISSING_PROD_TYPE)?;
        let content;
        parenthesized!(content in input);
        let prod_type_str = prod_type_raw.to_string();
        let prod_type = match prod_type_str.as_str() {
            "Literal" | "Regex" => {
                let patt: LitStr = content.parse().context(ERR_MISSING_PATT)?;
                let callback = content.parse::<ExprClosure>().context(ERR_NO_CALLBACK)?;
                if prod_type_str.as_str() == "Regex" {
                    Ok(ProductionType::Regex(patt.value(), callback))
                } else {
                    Ok(ProductionType::Literal(patt.value(), callback))
                }
            }
            "Rule" => {
                let mut rules = vec![];
                let mut last_was_ident = false;
                loop {
                    if let Result::Ok(ident) = content.parse::<Ident>() {
                        if !last_was_ident {
                            rules.push((vec![], None));
                        }
                        rules.last_mut().unwrap().0.push(ident);
                        last_was_ident = true;
                    } else if content.parse::<Token![,]>().is_ok() {
                        last_was_ident = false;
                    } else if last_was_ident
                        && let Result::Ok(callback) = content.parse::<ExprClosure>()
                    {
                        rules.last_mut().unwrap().1 = Some(callback);
                        last_was_ident = false;
                    } else if !content.is_empty() {
                        return Result::Err(Error::new(content.span(), ERR_INCORRECT_PROD));
                    } else {
                        break;
                    }
                }
                Ok(ProductionType::Rule(rules))
            }
            _ => Err(Error::new(prod_type_raw.span(), ERR_INCORRECT_TOKEN_SPEC)),
        }?;
        Ok(Self {
            name_raw: name.to_string(),
            name,
            prod_type,
        })
    }
}

pub fn process_productions(
    productions: &Vec<Production>,
    start_prod: &String,
) -> (RegexDFA, DynTrie, DynParseTable) {
    let mut trie = DynTrie(vec![TrieNode {
        fin: None,
        children: [None; 256],
    }]);
    let (regexi, production_ids, _) = productions.iter().enumerate().fold(
        (vec![], BTreeMap::new(), 0),
        |(mut regexi, mut production_ids, mut lexeme_i), (production_i, production)| {
            match &production.prod_type {
                ProductionType::Regex(patt, _) => {
                    regexi.push((patt.as_str(), lexeme_i, production_ids.len() + 1));
                    lexeme_i += 1;
                }
                ProductionType::Literal(patt, _) => {
                    trie.insert(patt.as_bytes(), (lexeme_i, production_ids.len() + 1));
                    lexeme_i += 1;
                }
                ProductionType::Rule(_) => {}
            }
            production_ids.insert(&production.name_raw, production_i + 1);
            (regexi, production_ids, lexeme_i)
        },
    );
    let mut any_errors = false;
    let start_prod = production_ids
        .get(start_prod)
        .expect(crate::ERR_NO_START_PROD);
    // there are 2 augmentations: rules[0] = start' -> start, and rules.last is eof
    let mut rules: Vec<Vec<Vec<usize>>> = vec![vec![]; production_ids.len() + 2];
    for (raw_components, rule_name) in productions
        .iter()
        .filter_map(|prod| match &prod.prod_type {
            ProductionType::Rule(raw_components) => Some((raw_components, &prod.name_raw)),
            _ => None,
        })
        .flat_map(|(raw_components, rule_name)| {
            raw_components
                .iter()
                .map(move |raw_components| (raw_components, rule_name))
        })
    {
        let production_id = *production_ids.get(&rule_name).unwrap();
        rules[production_id].push(raw_components.0.iter().map(|raw_component|
            *production_ids.get(&raw_component.to_string()).unwrap_or_else(|| {
                eprintln!(r#"Reference to undefined production "{raw_component}" in definition of production "{rule_name}""#);
                any_errors = true;
                &0
            })
        ).collect());
    }
    if any_errors {
        panic!();
    }
    let dfa = RegexDFA::from_regexi(regexi);
    let eprint_conflict = |node: usize, rule, item: usize, s| {
        let item_name = productions
            .get(item)
            .map(|p| p.name.to_string())
            .unwrap_or(String::from("EOF"));
        eprintln!(
            "ERROR: {s} / Reduce conflict on item {} in rule {rule} of production {}",
            item_name, productions[node].name
        )
    };
    let parser = match DynParseTable::from_rules(rules, *start_prod) {
        Err(conflicts) => {
            for conflict in conflicts {
                match conflict {
                    Conflict::RR(node, rule, item) => eprint_conflict(node, rule, item, "Reduce"),
                    Conflict::SR(node, rule, item) => eprint_conflict(node, rule, item, "Shift"),
                }
            }
            panic!();
        }
        Ok(parser) => parser,
    };
    (dfa, trie, parser)
}
