mod lexer;

use crate::lexer::{process_productions, Production, ProductionType};
use proc_macro2::{Ident, Punct, Spacing, Span, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use shared_structs::{DynParseTable, DynTrie, RegexDFA};
use std::process::exit;
use syn::{
    parse::{discouraged::Speculative, Parse},
    spanned::Spanned,
    Error, ExprClosure, PathSegment, Token,
};

const ERR_STATE_NOT_SPECIFIED: &'static str =
    "ERROR: You must specify the lexer state with State(...)";
const ERR_MISSING_ELEM: &'static str =
    "ERROR: Expected State | <Rule-Name> at beginning of element.";
const ERR_MISSING_STATE_TYPE: &'static str =
    "ERROR: Expected parenthesized lexer state type after State element.";
const ERR_MISSING_INIT_STATE: &'static str =
    "ERROR: Expected initial lexer state after = in State element.";
const ERR_MISSING_OUT_TYPE: &'static str =
    "ERROR: Expected parenthesized output type after Output element.";
const ERR_NO_OUT_TYPE: &'static str = "ERROR: You must specify the output type with Output(...)";
const ERR_LEADING_COMMA: &'static str = "ERROR: Leading comma in macro input.";

extern crate proc_macro;

/// The parser macro takes comma separated arguments of three types:
/// - State: This argument must only be passed once. It is of the form State(<StateTypeName>),
///   and specifies the state which will be maintained during the lexing stage of your parser
/// - Elements: This argument must appear at least once. It specifies the various elements of
///   your language, and takes three sub-forms:
///   - Regex: Specifies a regex defined lexeme within your language. It must be of the form:
///     <LexemeName> => Regex(<ProducedNodeType>, "my-regex.*") <callback>
///     where the callback is a closure of form:
///       |state: &mut State, matched_text: &str| -> Node
///   - Literal: Specifies a literal defined lexeme within your language. It is of the same form as
///     a Regex, except it is specified as:
///       <LexemeName> => Literal(...) <callback>
///   - Output: Specifies the output enum produced by parser. Every single Rule must have a name
///     which corresponds to a member of this enum.
///   - Rule: Specifies a branch node in your AST. It must be of the form:
///       <RuleName> => Rule(<NodeInnerType>, <rule_1>, <rule_2>, ... )
///     where each rule is of the form: elem1 elem2 elem3 ... <optional-callback>
///     the optional callback is a closure of the form:
///       |elem1: Node, elem2: Node, ...| -> Node {...}
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
    trie: DynTrie,
    // TODO allow full paths as out type
    out_type: PathSegment,
    productions: Vec<Production>,
    state_type: syn::Ident,
    parser: DynParseTable,
}

impl Parse for MacroBody {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut state = None;
        let mut productions: Vec<Production> = vec![];
        let mut out_type = None;
        while !input.is_empty() {
            let fork = input.fork();
            if let Result::Ok(ident) = fork.parse::<Ident>()
                && ["State", "Output"].contains(&ident.to_string().as_str())
            {
                input.advance_to(&fork);
                let ident_str = ident.to_string();
                if ident_str == "State" {
                    let content;
                    syn::parenthesized!(content in input);
                    let state_type = content.parse().context(ERR_MISSING_STATE_TYPE)?;
                    state = Some(state_type);
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
        let state_type = state.ok_or(Error::new(tot_span, ERR_STATE_NOT_SPECIFIED))?;
        let out_type = out_type.ok_or(Error::new(tot_span, ERR_NO_OUT_TYPE))?;
        let (dfa, trie, parser) = process_productions(&productions);
        Ok(Self {
            dfa,
            trie,
            out_type,
            state_type,
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
        parser,
    } = syn::parse2(input)?;

    let num_tokens = productions.len() + 1;
    let num_literals = trie.0.len();
    let num_lex_states = dfa.fin.len();
    let num_parse_states = parser.actions.len();
    let num_rules = parser.rule_lens.len();
    let mut is_token = TokenStream::new();
    is_token.append_separated(productions.iter().map(|p| !matches!(p.prod_type, ProductionType::Rule(_))), Punct::new(',', Spacing::Alone));
    is_token.append_all(quote! { , true });

    let make_rule_callback = |maybe_user_callback: Option<&ExprClosure>,
                              num_generated: usize,
                              num_args: usize|
     -> (TokenStream, Ident) {
        let callback_name = Ident::new(&format!("__gen_{}", num_generated), Span::call_site());
        let user_callback = maybe_user_callback
            .map(|c| c.to_token_stream())
            .unwrap_or_else(|| {
                let mut closure_args = quote! { node_1 };
                for _ in 0..(num_args - 1) {
                    closure_args.append_all(quote! {, _});
                }
                quote! { |#closure_args| node_1 }
            });

        let callback_args_rev = (0..num_args)
            .map(|i| Ident::new(&format!("node_{}", num_args - i), user_callback.span()));
        let stack_pops_iter = callback_args_rev
            .clone()
            .map(|s| quote! { let #s = node_stack.pop().unwrap(); });
        let stack_pops = quote! { #(#stack_pops_iter)* };
        let callback_args_iter = callback_args_rev.rev();
        let callback_args = quote! { #(#callback_args_iter),* };
        let cap = shared_structs::MAX_STATE_STACK;
        let callback = quote! {
            fn #callback_name(node_stack: &mut parser::CappedVec<#cap, #out_type>) -> #out_type {
                #stack_pops
                let user_callback = #user_callback;
                user_callback(#callback_args)
            }
        };
        (callback, callback_name)
    };
    let make_lexeme_callback =
        |user_callback: &ExprClosure, num_generated: usize| -> (TokenStream, Ident) {
            let callback_name = Ident::new(&format!("__gen_{}", num_generated), Span::call_site());
            let callback = quote! {
                fn #callback_name(state: &mut #state_type, s: &str) -> #out_type {
                    let user_callback = #user_callback;
                    user_callback(state, s)
                }
            };
            (callback, callback_name)
        };
    let mut lexeme_callback_defs = TokenStream::new();
    let mut lexeme_callback_names = vec![];
    let mut rule_callback_defs = TokenStream::new();
    let mut rule_callback_names = vec![];
    let mut num_terminals = 0usize;
    let mut num_generated = 0;
    let mut cur_rule = 0;

    for production in productions.iter() {
        match &production.prod_type {
            ProductionType::Literal(_, callback) | ProductionType::Regex(_, callback) => {
                let (callback, callback_name) = make_lexeme_callback(callback, num_generated);
                num_terminals += 1;
                num_generated += 1;
                lexeme_callback_defs.append_all(callback);
                lexeme_callback_names.push(callback_name);
            }
            ProductionType::Rule(rules) => {
                for (_, callback) in rules {
                    let (callback, callback_name) = make_rule_callback(
                        callback.as_ref(),
                        num_generated,
                        parser.rule_lens[cur_rule].0,
                    );
                    num_generated += 1;
                    cur_rule += 1;
                    rule_callback_defs.append_all(callback);
                    rule_callback_names.push(callback_name);
                }
            }
        }
    }
    let mut lexeme_callbacks = TokenStream::new();
    lexeme_callbacks.append_separated(lexeme_callback_names, Punct::new(',', Spacing::Alone));
    let mut rule_callbacks = TokenStream::new();
    rule_callbacks.append_separated(rule_callback_names, Punct::new(',', Spacing::Alone));

    let mut rule_callbacks_inner = TokenStream::new();
    let default_rule_callback = quote! { Box::new(|mut children| children.swap_remove(0)) };
    rule_callbacks_inner.append_all(default_rule_callback.clone());
    rule_callbacks_inner.append_all(quote! { , });
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
                    default_rule_callback.clone()
                }
            })
        }),
        Punct::new(',', Spacing::Alone),
    );

    Ok(quote! {
        fn create_parsing_engine() ->
            Result<parser::Engine<#out_type, #state_type, #num_terminals, #num_tokens, #num_literals, #num_lex_states, #num_parse_states, #num_rules>, &'static str>
        {
            #lexeme_callback_defs
            #rule_callback_defs
            parser::Engine::from_raw(
                #parser,
                #dfa,
                #trie,
                [#lexeme_callbacks],
                [#rule_callbacks],
                [#is_token],
            )
        }
    })
}
