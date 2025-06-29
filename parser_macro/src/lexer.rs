use std::collections::BTreeMap;

use shared_structs::{RegexDFA, Trie, TrieNode};
use syn::{
    Error, ExprClosure, Ident, LitStr, Token, parenthesized, parse::Parse, spanned::Spanned,
};

use crate::ERR_NO_START_PROD;

const ERR_INCORRECT_TOKEN_SPEC: &'static str = r#"Provide each token in the form
    "Literal(<pattern>) or "Regex(<pattern>)"
    where pattern is a single string literal"#;
const ERR_INCORRECT_PRODUCTION_CALLBACK: &'static str =
    r#"Each provided callback must take (or ignore) exactly 2 arguments"#;
const ERR_INCORRECT_PROD_RULE: &'static str = r#"Each parsing rule should be of the form:
    RULE_NAME => Rule(<component 1> <component 2> ... <component n>) <optional callback>"#;

pub enum ProductionType {
    Regex(LitStr),
    Literal(LitStr),
    Rule(Vec<String>),
}

pub struct Production {
    pub name: Ident,
    pub name_raw: String,
    pub prod_type: ProductionType,
    pub callback: Option<ExprClosure>,
}

impl Parse for Production {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![=>]>()?;
        let prod_type_raw = input.parse::<Ident>()?;
        let content;
        parenthesized!(content in input);
        let prod_type = match prod_type_raw.to_string().as_str() {
            "Regex" => Ok(ProductionType::Regex(content.parse()?)),
            "Literal" => Ok(ProductionType::Literal(content.parse()?)),
            "Rule" => {
                let mut idents = vec![];
                while let Ok(ident) = content.parse::<Ident>() {
                    idents.push(ident.to_string());
                }
                if content.is_empty() {
                    Ok(ProductionType::Rule(idents))
                } else {
                    Err(Error::new(content.span(), ERR_INCORRECT_PROD_RULE))
                }
            }
            _ => Err(Error::new(prod_type_raw.span(), ERR_INCORRECT_TOKEN_SPEC)),
        }?;
        let callback: Option<syn::ExprClosure> = input.parse().ok();
        if let Some(callback) = callback.as_ref() {
            if callback.inputs.len() != 2 {
                Err(Error::new(
                    callback.inputs.span(),
                    ERR_INCORRECT_PRODUCTION_CALLBACK,
                ))?;
            }
            // TODO: More checks on the callback. We want to catch these rather than rustc.
        }
        Ok(Self {
            name_raw: name.to_string(),
            name,
            callback,
            prod_type,
        })
    }
}

pub fn process_lexemes(productions: &Vec<Production>, start_prod: &String) -> (RegexDFA, Trie) {
    let mut regexi = vec![];
    let mut production_ids = BTreeMap::new();
    let mut trie = Trie(vec![TrieNode {
        callback: None,
        children: [None; 256],
    }]);
    for (i, prod) in productions.iter().enumerate() {
        match &prod.prod_type {
            ProductionType::Regex(patt) => regexi.push(patt.value()),
            ProductionType::Literal(patt) => trie.insert(patt.value().as_bytes(), i),
            ProductionType::Rule(_) => {}
        }
        production_ids.insert(&prod.name_raw, i);
    }
    let mut any_errors = false;
    let start_prod = production_ids.get(start_prod).expect(ERR_NO_START_PROD);
    let mut rules: Vec<Vec<Vec<usize>>> = vec![];
    for (raw_components, rule_name) in productions.iter().filter_map(|prod| match &prod.prod_type {
        ProductionType::Rule(raw_components) => Some((raw_components, &prod.name_raw)),
        _ => None,
    }) {
        let production_id = *production_ids.get(&rule_name).unwrap();
        if rules.len() < production_id + 1 {
            rules.resize(production_id + 1, vec![]);
        }
        rules[production_id].push(raw_components.into_iter().map(|raw_component|
            *production_ids.get(raw_component).unwrap_or_else(||{
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
    (dfa, trie)
}
