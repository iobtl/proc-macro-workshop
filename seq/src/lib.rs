use std::ops::Range;

use proc_macro::TokenStream;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, ExprBlock, ExprRange, Ident, LitInt, Token,
};

#[derive(Debug)]
struct SeqRange {
    ident: Ident,
    range: Range<u16>,
}

impl Parse for SeqRange {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;

        input.parse::<Token![in]>()?;

        let range_start: u16 = input.parse::<LitInt>().and_then(|i| i.base10_parse())?;
        input.parse::<Token![..]>()?;
        let range_end: u16 = input.parse::<LitInt>().and_then(|i| i.base10_parse())?;

        input.parse::<ExprBlock>()?;

        Ok(SeqRange {
            ident,
            range: range_start..range_end,
        })
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as SeqRange);

    eprintln!("{:?}", input);

    TokenStream::new()
}
