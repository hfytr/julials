mod lexer;

use crate::lexer::{Production, ProductionType, process_productions};
use proc_macro2::{Ident, Punct, Spacing, TokenStream};
use quote::{TokenStreamExt, quote};
use shared_structs::{ParseTable, RegexDFA, Trie};
use std::process::exit;
use syn::{
    Error, PathSegment, Token,
    parse::{Parse, discouraged::Speculative},
};

const ERR_STATE_NOT_SPECIFIED: &'static str =
    "ERROR: You must specify the lexer state with State(...)";
const ERR_MISSING_ELEM: &'static str =
    "ERROR: Expected State | Start | <Rule-Name> at beginning of element.";
const ERR_MISSING_STATE_TYPE: &'static str =
    "ERROR: Expected parenthesized lexer state type after State element.";
const ERR_MISSING_INIT_STATE: &'static str =
    "ERROR: Expected initial lexer state after = in State element.";
const ERR_MISSING_START_PROD: &'static str =
    "ERROR: Expected parenthesized production name after Start element.";
const ERR_NO_START_PROD: &'static str =
    "ERROR: You must specify the starting state with Start(...)";
const ERR_MISSING_OUT_TYPE: &'static str =
    "ERROR: Expected parenthesized output type after Output element.";
const ERR_NO_OUT_TYPE: &'static str = "ERROR: You must specify the output type with Output(...)";
const ERR_LEADING_COMMA: &'static str = "ERROR: Leading comma in macro input.";

extern crate proc_macro;

/// The parser macro takes comma separated arguments of three types:
/// - State: This argument must only be passed once. It is of the form State(<StateTypeName>),
///   and specifies the state which will be maintained during the lexing stage of your parser
/// - Start: This argument must only passed once. It is of the form Start(<ElementName>) and
///   must specify an element of your language previously / later defined.
/// - Elements: This argument must appear at least once. It specifies the various elements of
///   your language, and takes three sub-forms:
///   - Regex: Specifies a regex defined lexeme within your language. It must be of the form:
///     <LexemeName> => Regex(<ProducedNodeType>, "my-regex.*") <callback>
///     where the callback is a closure of form:
///       |state: &mut State, matched_text: &str| -> Node
///   - Literal: Specifies a literal defined lexeme within your language. It is of the same form as
///     a Regex, except it is specified as:
///       <LexemeName> => Literal(...) <-callback>
///   - Output: Specifies the output enum produced by parser. Every single Rule must have a name
///     which corresponds to a member of this enum.
///   - Rule: Specifies a branch node in your AST. It must be of the form:
///       OutputType::<RuleName> => Rule(<NodeInnerType>, <rule_1> | <rule_2> | ... )
///     where each rule is of the form: elem1 elem2 elem3 ... <optional-callback>
///     the optional callback is a closure of the form:
///       |children: Vec<Box<Node>>| -> Node {...}
#[proc_macro]
pub fn parser(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse(input).expect("Proc Macro errored parsing input.");
    match parser2(input) {
        Result::Ok(output) => output.into(),
        Result::Err(err) => {
            eprintln!("{}", err);
            exit(1);
        }
    }
}

trait Context {
    fn context(self, cx: &str) -> Self;
}

impl<T> Context for syn::Result<T> {
    fn context(self, cx: &str) -> Self {
        self.map_err(|e| Error::new(e.span(), format!("{cx}: {e}")))
    }
}

struct MacroBody {
    dfa: RegexDFA,
    trie: Trie,
    // TODO allow full paths as out type
    out_type: PathSegment,
    productions: Vec<Production>,
    state_type: syn::Ident,
    init_state: syn::Expr,
    parser: ParseTable,
}

impl Parse for MacroBody {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut state = None;
        let mut start_prod = None;
        let mut productions: Vec<Production> = vec![];
        let mut out_type = None;
        while !input.is_empty() {
            let fork = input.fork();
            if let Result::Ok(ident) = fork.parse::<Ident>()
                && ["State", "Start", "Output"].contains(&ident.to_string().as_str())
            {
                input.advance_to(&fork);
                let ident_str = ident.to_string();
                if ident_str == "State" {
                    let content;
                    syn::parenthesized!(content in input);
                    let state_type = content.parse().context(ERR_MISSING_STATE_TYPE)?;
                    content
                        .parse::<Token![=]>()
                        .context(ERR_MISSING_INIT_STATE)?;
                    let init_state = content.parse().context(ERR_MISSING_INIT_STATE)?;
                    state = Some((init_state, state_type));
                } else if ident_str == "Start" {
                    let content;
                    syn::parenthesized!(content in input);
                    start_prod = Some(
                        content
                            .parse::<Ident>()
                            .context(ERR_MISSING_START_PROD)?
                            .to_string(),
                    );
                } else if ident_str == "Output" {
                    let content;
                    syn::parenthesized!(content in input);
                    out_type = Some(
                        content
                            .parse::<PathSegment>()
                            .context(ERR_MISSING_OUT_TYPE)?,
                    );
                } else {
                    return Result::Err(Error::new(ident.span(), ERR_MISSING_ELEM));
                }
            } else {
                productions.push(input.parse()?);
            }
            if input.peek(Token![,]) {
                input.parse::<Token![,]>().context(ERR_LEADING_COMMA)?;
            }
        }
        let tot_span = input.span();
        let (init_state, state_type) =
            state.ok_or(Error::new(tot_span, ERR_STATE_NOT_SPECIFIED))?;
        let start_prod = start_prod.ok_or(Error::new(tot_span, ERR_NO_START_PROD))?;
        let out_type = out_type.ok_or(Error::new(tot_span, ERR_NO_OUT_TYPE))?;
        let (dfa, trie, parser) = process_productions(&productions, &start_prod);
        Ok(Self {
            dfa,
            trie,
            out_type,
            state_type,
            init_state,
            productions,
            parser,
        })
    }
}

fn parser2(input: TokenStream) -> Result<TokenStream, Error> {
    let MacroBody {
        dfa,
        trie,
        out_type,
        productions,
        state_type,
        init_state,
        parser,
    } = syn::parse2(input)?;
    let mut lexeme_callbacks_inner = TokenStream::new();
    lexeme_callbacks_inner.append_separated(
        productions
            .iter()
            .filter(|production| !matches!(production.prod_type, ProductionType::Rule(_)))
            .map(|production| {
                if let ProductionType::Literal((_, ref callback)) = production.prod_type {
                    quote! { Box::new(#callback) }
                } else if let ProductionType::Regex((_, ref callback)) = production.prod_type {
                    quote! { Box::new(#callback) }
                } else {
                    let prod_name = &production.name;
                    quote! { Box::new(|_, _| #out_type::#prod_name(#prod_name::default())) }
                }
            }),
        Punct::new(',', Spacing::Alone),
    );
    let mut rule_callbacks_inner = TokenStream::new();
    rule_callbacks_inner.append_separated(
        productions.into_iter().flat_map(|production| {
            if let ProductionType::Rule(rules) = production.prod_type {
                rules
            } else {
                vec![]
            }
            .into_iter()
            .map(|(_, callback)| {
                if let Some(callback) = callback.as_ref() {
                    quote! { Box::new(#callback) }
                } else {
                    quote! { Box::new(|mut children| children.swap_remove(0)) }
                }
            })
        }),
        Punct::new(',', Spacing::Alone),
    );
    Ok(quote! {
        fn create_parsing_engine<'a, 'b>(s: &'a mut &'b str) ->
            Result<parser::Engine<'a, 'b, #out_type, #state_type>, &'static str>
        {
            parser::Engine::from_raw(
                #parser,
                #dfa,
                #trie,
                vec![#lexeme_callbacks_inner],
                vec![#rule_callbacks_inner],
                #init_state,
                s
            )
        }
    })
}
