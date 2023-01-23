use std::ops::Range;

use proc_macro::TokenStream;
use proc_macro2::{Punct, TokenTree};
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, ExprBlock, ExprRange, Ident, LitInt, Token,
};

#[derive(Debug)]
struct Seq {
    ident: Ident,
    range: Range<u16>,
    tokens: TokenStream,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;

        input.parse::<Token![in]>()?;

        let range_start: u16 = input.parse::<LitInt>().and_then(|i| i.base10_parse())?;
        input.parse::<Token![..]>()?;
        let range_end: u16 = input.parse::<LitInt>().and_then(|i| i.base10_parse())?;

        let block: ExprBlock = input.parse()?;
        let tokens: TokenStream = block
            .block
            .stmts
            .into_iter()
            .map(|stmt| TokenStream::from(stmt.into_token_stream()))
            .collect();

        Ok(Seq {
            ident,
            range: range_start..range_end,
            tokens,
        })
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Seq);

    eprintln!("{:?}", input);

    TokenStream::new()
}
