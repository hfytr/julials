mod lexer;

use crate::lexer::{process_productions, Production, ProductionType};
use proc_macro2::{Ident, Punct, Spacing, Span, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use shared_structs::{DynParseTable, DynTrie, RegexDFA};
use std::process::exit;
use syn::{
    parse::{discouraged::Speculative, Parse},
    spanned::Spanned,
    Error, ExprClosure, LitStr, Token, Type,
};

const ERR_STATE_NOT_SPECIFIED: &'static str =
    "ERROR: You must specify the lexer state with State(...)";
const ERR_MISSING_ELEM: &'static str =
    "ERROR: Expected State | <Rule-Name> at beginning of element.";
const ERR_MISSING_STATE_TYPE: &'static str =
    "ERROR: Expected parenthesized lexer state type after State element.";
const ERR_MISSING_OUT_TYPE: &'static str =
    "ERROR: Expected parenthesized output type after Output element.";
const ERR_NO_OUT_TYPE: &'static str = "ERROR: You must specify the output type with Output(...)";
const ERR_LEADING_COMMA: &'static str = "ERROR: Leading comma in macro input.";
const ERR_MISSING_UPDATE_REGEX: &'static str =
    "ERROR: Expected string literal in parenthesized Update(...)";
const ERR_MISSING_UPDATE_CALLBACK: &'static str =
    "ERROR: Expected string literal in parenthesized Update(...)";

extern crate proc_macro;

/// The parser macro takes comma separated arguments of three types:
/// - State: This argument must only be passed once. It is of the form State(<StateTypeName>),
///   and specifies the state which will be maintained during the lexing stage of your parser
/// Output: Specifies the output enum produced by parser. Every single Rule must have a name
/// which corresponds to a member of this enum.
/// - Elements: This argument must appear at least once. It specifies the various elements of
///   your language, and takes three sub-forms:
///   - Regex: Specifies a regex defined lexeme within your language. It must be of the form:
///     <LexemeName> => Regex(<ProducedNodeType>, "my-regex.*") <callback>
///     where the callback is a closure of form:
///       |state: &mut State, matched_text: &str| -> Node
///   - Literal: Specifies a literal defined lexeme within your language. It is of the same form as
///     a Regex, except it is specified as:
///       <LexemeName> => Literal(...) <callback>
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
    regex: RegexDFA,
    update: RegexDFA,
    update_callbacks: Vec<ExprClosure>,
    trie: DynTrie,
    // TODO allow full paths as out type
    out_type: TokenStream,
    state_type: TokenStream,
    productions: Vec<Production>,
    parser: DynParseTable,
    num_tokens: usize,
    is_token: Vec<bool>,
}

impl Parse for MacroBody {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut state = None;
        let mut productions: Vec<Production> = vec![];
        let mut out_type = None;
        let mut update_vec = vec![];
        while !input.is_empty() {
            let fork = input.fork();
            if let Result::Ok(ident) = fork.parse::<Ident>()
                && ["State", "Output", "Update"].contains(&ident.to_string().as_str())
            {
                input.advance_to(&fork);
                let ident_str = ident.to_string();
                if ident_str == "State" {
                    let content;
                    syn::parenthesized!(content in input);
                    state = Some(content.parse::<Type>().context(ERR_MISSING_STATE_TYPE)?);
                } else if ident_str == "Output" {
                    let content;
                    syn::parenthesized!(content in input);
                    out_type = Some(content.parse::<Type>().context(ERR_MISSING_OUT_TYPE)?);
                } else if ident_str == "Update" {
                    let content;
                    syn::parenthesized!(content in input);
                    update_vec.push((
                        content
                            .parse::<LitStr>()
                            .context(ERR_MISSING_UPDATE_REGEX)?,
                        content
                            .parse::<ExprClosure>()
                            .context(ERR_MISSING_UPDATE_CALLBACK)?,
                    ));
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
        let out_type = out_type.ok_or(Error::new(tot_span, ERR_NO_OUT_TYPE))?;
        let state_type = state.ok_or(Error::new(tot_span, ERR_STATE_NOT_SPECIFIED))?;
        let update = RegexDFA::from_regexi(
            update_vec
                .iter()
                .enumerate()
                .map(|(i, (s, _))| (s.value(), i, 0)),
        );
        let update_callbacks = update_vec.into_iter().map(|(_, c)| c).collect();
        let (regex, trie, parser, num_tokens, is_token) = process_productions(&productions);

        let result = Ok(Self {
            regex,
            update,
            update_callbacks,
            productions,
            trie,
            out_type: quote! { #out_type },
            state_type: quote! { #state_type },
            parser,
            num_tokens,
            is_token
        });
        result
    }
}

fn parser2(input: TokenStream) -> Result<TokenStream, Error> {
    let MacroBody {
        regex,
        update,
        update_callbacks,
        trie,
        out_type,
        productions,
        state_type,
        parser,
        num_tokens,
        is_token,
    } = syn::parse2(input)?;

    let num_literals = trie.0.len();
    let num_updates = update_callbacks.len();
    let num_lex_states = regex.fin.len();
    let num_update_states = update.fin.len();
    let num_parse_states = parser.actions.len();
    let num_rules = parser.rule_lens.len();
    let mut is_token_toks = TokenStream::new();
    is_token_toks.append_separated(is_token .iter(), Punct::new(',', Spacing::Alone));
    is_token_toks.append_all(quote! { , true });

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
        let callback = quote! {
            fn #callback_name(node_stack: &mut parser::CappedVec<{shared_structs::MAX_STATE_STACK}, #out_type>) -> #out_type {
                #stack_pops
                let user_callback = #user_callback;
                user_callback(#callback_args)
            }
        };
        (callback, callback_name)
    };

    let mut lexeme_callback_defs = TokenStream::new();
    let mut lexeme_callback_names = vec![];
    let mut rule_callback_defs = TokenStream::new();
    let mut rule_callback_names = vec![];
    let mut update_callback_defs = TokenStream::new();
    let mut update_callback_names = vec![];
    let mut num_terminals = 0usize;
    let mut num_generated = 0;
    let mut cur_rule = 0;

    for production in productions.iter() {
        match &production.prod_type {
            ProductionType::Literal(_, user_callback) | ProductionType::Regex(_, user_callback) => {
                let callback_name =
                    Ident::new(&format!("__gen_{}", num_generated), Span::call_site());
                let callback = quote! {
                    fn #callback_name(state: &mut #state_type, s: &str) -> #out_type {
                        let user_callback = #user_callback;
                        user_callback(state, s)
                    }
                };
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
    for update_callback in update_callbacks.iter() {
        let callback_name = Ident::new(&format!("__gen_{}", num_generated), Span::call_site());
        num_generated += 1;
        let callback = quote! {
            fn #callback_name(state: &mut #state_type, text: &str) {
                let update_callback = #update_callback;
                update_callback(state, text);
            }
        };
        update_callback_defs.append_all(callback);
        update_callback_names.push(callback_name);
    }

    let mut lexeme_callbacks = TokenStream::new();
    lexeme_callbacks.append_separated(
        lexeme_callback_names.into_iter().map(|name| {
            quote! {
                #name as fn(&mut #state_type, &str) -> #out_type
            }
        }),
        Punct::new(',', Spacing::Alone),
    );
    let mut rule_callbacks = TokenStream::new();
    rule_callbacks.append_separated(rule_callback_names.into_iter().map(|name| quote! {
        #name as fn(&mut parser::CappedVec<{shared_structs::MAX_STATE_STACK}, #out_type>) -> #out_type
    }), Punct::new(',', Spacing::Alone));
    let mut update_callbacks = TokenStream::new();
    update_callbacks.append_separated(
        update_callback_names.into_iter().map(|name| {
            quote! {
                #name as fn(&mut #state_type, &str)
            }
        }),
        Punct::new(',', Spacing::Alone),
    );

    Ok(quote! {
        fn create_parsing_engine() ->
            Result<parser::Engine<
                #out_type,
                #state_type,
                #num_terminals,
                #num_tokens,
                #num_literals,
                #num_updates,
                #num_lex_states,
                #num_update_states,
                #num_parse_states,
                #num_rules
                >,
                &'static str
            >
        {
            #lexeme_callback_defs
            #rule_callback_defs
            #update_callback_defs
            parser::Engine::from_raw(
                #parser,
                #regex,
                #update,
                #trie,
                [#lexeme_callbacks],
                [#update_callbacks],
                [#rule_callbacks],
                [#is_token_toks],
            )
        }
    })
}
