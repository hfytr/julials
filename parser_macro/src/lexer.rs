use std::collections::BTreeMap;

use shared_structs::{ParseTable, RegexDFA, Trie, TrieNode};
use syn::{Error, ExprClosure, Ident, LitStr, Token, parenthesized, parse::ParseStream};

use crate::Context;

const ERR_INCORRECT_TOKEN_SPEC: &'static str = r#"ERROR: Provide each token in the form
    "Literal(<pattern>) or "Regex(<pattern>)"
    where pattern is a single string literal"#;
const ERR_INCORRECT_PROD: &'static str = r#"ERROR: Each parsing rule should be of the form:
    RULE_NAME => Rule(<component 1> <component 2> ... <component n>) <optional callback>"#;
const ERR_MISSING_PROD_TYPE: &'static str =
    "ERROR: Expected Literal | Regex | Rule after => in new production definition.";
const ERR_MISSING_PROD_DATA_TYPE: &'static str =
    "ERROR: Expected parenthesized rule data type in new production definition";
const ERR_MISSING_PATT: &'static str =
    "ERROR: Expected parenthesized string pattern after Regex | Literal.";
const ERR_MISSING_AMERICAN_ARROW: &'static str =
    "ERROR: Expected => after new production definition.";
const ERR_INVALID_CALLBACK: &'static str = "ERROR: The provided callback is invalid.";
const ERR_MISSING_COMMA_PROD_DT: &'static str = "ERROR: Missing , after production data type";

pub enum ProductionType {
    Regex((LitStr, Option<ExprClosure>)),
    Literal((LitStr, Option<ExprClosure>)),
    Rule(Vec<Vec<(Ident, Option<ExprClosure>)>>),
}

pub struct Production {
    pub name: Ident,
    pub name_raw: String,
    pub data_type: Ident,
    pub prod_type: ProductionType,
}

pub fn parse_production(name: Ident, input: &ParseStream) -> syn::Result<Production> {
    input
        .parse::<Token![=>]>()
        .context(ERR_MISSING_AMERICAN_ARROW)?;
    let prod_type_raw = input.parse::<Ident>().context(ERR_MISSING_PROD_TYPE)?;
    let content;
    parenthesized!(content in input);
    let data_type = content
        .parse::<Ident>()
        .context(ERR_MISSING_PROD_DATA_TYPE)?;
    content
        .parse::<Token![,]>()
        .context(ERR_MISSING_COMMA_PROD_DT)?;
    let prod_type_str = prod_type_raw.to_string();
    let prod_type = match prod_type_str.as_str() {
        "Literal" | "Regex" => {
            let pattern = content.parse().context(ERR_MISSING_PATT)?;
            let callback = (!content.is_empty())
                .then(|| content.parse::<ExprClosure>())
                .transpose()
                .context(ERR_INVALID_CALLBACK)?;
            if prod_type_str.as_str() == "Regex" {
                Ok(ProductionType::Regex((pattern, callback)))
            } else {
                Ok(ProductionType::Literal((pattern, callback)))
            }
        }
        "Rule" => {
            let mut rules = vec![(vec![])];
            let mut last_was_ident = false;
            loop {
                if let Result::Ok(ident) = content.parse::<Ident>() {
                    rules.last_mut().unwrap().push((ident, None));
                    last_was_ident = true;
                } else if last_was_ident && content.fork().parse::<ExprClosure>().is_ok() {
                    // ^ could affect parsing of "|"
                    let callback = content.parse::<ExprClosure>().unwrap();
                    rules.last_mut().unwrap().last_mut().unwrap().1 = Some(callback);
                    last_was_ident = false;
                } else if content.parse::<Token![|]>().is_ok() {
                    rules.push(vec![]);
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
    Ok(Production {
        name_raw: name.to_string(),
        name,
        data_type,
        prod_type,
    })
}

pub fn process_productions(
    productions: &Vec<Production>,
    start_prod: &String,
) -> (RegexDFA, Trie, ParseTable) {
    let mut trie = Trie(vec![TrieNode {
        callback: None,
        children: [None; 256],
    }]);
    let (regexi, production_ids) = productions.iter().fold(
        (vec![], BTreeMap::new()),
        |(mut regexi, mut production_ids), production| {
            let i = regexi.len() + 1; // index 0 reserved for generated start production
            match &production.prod_type {
                ProductionType::Regex((patt, _)) => regexi.push((patt.value(), i)),
                ProductionType::Literal((patt, _)) => trie.insert(patt.value().as_bytes(), i),
                ProductionType::Rule(_) => {}
            }
            production_ids.insert(&production.name_raw, i);
            (regexi, production_ids)
        },
    );
    let mut any_errors = false;
    let start_prod = production_ids
        .get(start_prod)
        .expect(crate::ERR_NO_START_PROD);
    let mut rules: Vec<Vec<Vec<usize>>> = vec![];
    for production in productions.iter() {
        let production_id = *production_ids.get(&production.name_raw).unwrap();
        if rules.len() < production_id + 1 {
            rules.resize(production_id + 1, vec![]);
        }
    }
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
        if rules.len() < production_id + 1 {
            rules.resize(production_id + 1, vec![]);
        }
        rules[production_id].push(raw_components.into_iter().map(|(raw_component, _)|
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
    let mut dfa = RegexDFA::from_regexi(regexi);
    dfa.minimize();
    let parser = ParseTable::from_rules(rules, *start_prod);
    (dfa, trie, parser)
}
