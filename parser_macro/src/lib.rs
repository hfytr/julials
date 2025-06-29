mod lexer;

use crate::lexer::{Production, process_lexemes};
use proc_macro2::{Ident, Punct, Spacing, TokenStream};
use quote::{TokenStreamExt, quote};
use shared_structs::{RegexDFA, Trie};
use std::process::exit;
use syn::{Error, Token, parse::Parse, punctuated::Punctuated};

const ERR_FIRST_NOT_STATE: &'static str =
    r#"The first item within the parser macro must be of the form "State(<State-Type>)""#;
const ERR_SECOND_NOT_START: &'static str =
    r#"The second item within the parser macro must be of the form "Start(<Production-Name>)""#;
const ERR_NO_START_PROD: &'static str = r#"You must specify a start symbol"#;

extern crate proc_macro;

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

struct MacroBody {
    dfa: RegexDFA,
    trie: Trie,
    start_prod: String,
    lexemes: Vec<Production>,
    state_type: syn::Ident,
    init_state: syn::Expr,
}

impl Parse for MacroBody {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident: syn::Ident = input.parse()?;
        if ident.to_string().as_str() != "State" {
            return Err(Error::new_spanned(ident, ERR_FIRST_NOT_STATE));
        }
        let content;
        syn::parenthesized!(content in input);
        let state_type = content.parse()?;
        input.parse::<Token![=]>()?;
        let init_state = input.parse()?;
        input.parse::<Token![,]>()?;
        let ident: syn::Ident = input.parse()?;
        if ident.to_string().as_str() != "Start" {
            return Err(Error::new_spanned(ident, ERR_SECOND_NOT_START));
        }
        let content;
        syn::parenthesized!(content in input);
        let start_prod = content.parse::<Ident>()?.to_string();
        input.parse::<Token![,]>()?;
        let lexemes: Vec<_> = Punctuated::<_, Token![,]>::parse_terminated(input)?
            .into_iter()
            .collect();
        let (dfa, trie) = process_lexemes(&lexemes, &start_prod);
        Ok(Self {
            dfa,
            trie,
            start_prod,
            lexemes,
            state_type,
            init_state,
        })
    }
}

fn parser2(input: TokenStream) -> Result<TokenStream, Error> {
    let MacroBody {
        dfa,
        trie,
        start_prod,
        lexemes,
        state_type,
        init_state,
    } = syn::parse2(input)?;
    let num_lexemes = lexemes.len() + 1;
    let mut lexeme_arr = TokenStream::new();
    lexeme_arr.append_separated(
        lexemes.iter().map(|lexeme| {
            let lexeme_name = &lexeme.name;
            quote! { Lexeme::#lexeme_name }
        }),
        Punct::new(',', Spacing::Alone),
    );
    let mut lexeme_match_body = TokenStream::new();
    lexeme_match_body.append_separated(
        (1usize..).zip(lexemes.iter()).map(|(i, lexeme)| {
            let name = &lexeme.name;
            quote! { Lexeme::#name => #i, }
        }),
        Punct::new(',', Spacing::Alone),
    );
    let mut lexeme_enum = TokenStream::new();
    lexeme_enum.append_all(lexemes.iter().map(|lexeme| {
        let lexeme_type = lexeme
            .callback
            .as_ref()
            .and_then(|callback| match callback.output {
                syn::ReturnType::Default => None,
                syn::ReturnType::Type(_, ref ret_type) => Some(quote! { (#ret_type) }),
            });
        let lexeme_name = &lexeme.name;
        quote! { #lexeme_name #lexeme_type, }
    }));
    let mut callbacks_inner = TokenStream::new();
    callbacks_inner.append_separated(
        lexemes.iter().map(|lexeme| {
            let lexeme_name = &lexeme.name;
            if let Some(ref callback) = lexeme.callback {
                quote! {
                    std::boxed::Box::new(|state, s| {
                        let callback_raw = #callback;
                        Lexeme::#lexeme_name(callback_raw(state, s))
                    })
                }
            } else {
                quote! { std::boxed::Box::new(|_, s| Lexeme::#lexeme_name) }
            }
        }),
        Punct::new(',', Spacing::Alone),
    );
    Ok(quote! {
        #[derive(Debug)]
        enum Lexeme { EOF, #lexeme_enum }
        impl Default for Lexeme {
            fn default() -> Self {
                Lexeme::EOF
            }
        }
        impl Into<usize> for Lexeme {
            fn into(self) -> usize {
                match self { Lexeme::EOF => 0usize, #lexeme_match_body }
            }
        }
        const ID_TO_LEXEME: [Lexeme; #num_lexemes] = [Lexeme::EOF, #lexeme_arr];
        fn create_parsing_engine<'a, 'b>(s: &'a mut &'b str) -> shared_structs::Engine<'a, 'b, Lexeme, #state_type> {
            shared_structs::Engine::from_raw(
                #dfa,
                #trie,
                vec![#callbacks_inner],
                #init_state,
                s
            )
        }
    })
}
