use proc_macro::TokenStream;
use proc_macro2::{Literal, Punct, TokenTree};
use quote::{quote, quote_spanned, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput};
use tap::{Pipe, Tap};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    eprintln!("{:#?}", input);

    let struct_name = input.ident;
    // NOTE: quote! will automatically enclose strings with double quotes in the final token stream
    let struct_name_quote = struct_name.pipe_ref(|n| n.to_string());
    let struct_fields = match input.data {
        Data::Struct(s) => s.fields,
        Data::Enum(_) | Data::Union(_) => todo!(),
    };
    let struct_fields_names = struct_fields.iter().map(|field| {
        let name = &field.ident;
        let name_quote = name.as_ref().map(|n| n.to_string());
        let attrs = &field.attrs;

        if attrs.is_empty() {
            quote! {
                dbg.field(#name_quote, &self.#name);
            }
        } else if attrs.len() == 1 {
            // TODO: clone
            let mut attr_tokens = attrs.get(0).unwrap().tokens.clone().into_iter();

            // Check for '='
            match attr_tokens.next() {
                Some(TokenTree::Punct(punct)) if punct.as_char() == '=' => {}
                Some(tok) => {
                    return quote_spanned! {
                        tok.span() => compile_error!("`debug` attribute should be of the form `debug = \"<format_string>\"`")
                    };
                }
                None => {
                    return quote_spanned! {
                        field.span() => compile_error!("`debug` attribute should be of the form `debug = \"<format_string>\"`")
                    };
                }
            }

            // Then, check for format string
            let format_str = match attr_tokens.next() {
                // NOTE: don't want this to be a string
                Some(TokenTree::Literal(lit)) => { lit }
                Some(tok) => {
                    return quote_spanned! {
                        tok.span() => compile_error!("`debug` attribute should be of the form `debug = \"<format_string>\"`")
                    };
                }
                None => {
                    return quote_spanned! {
                        field.span() => compile_error!("`debug` attribute should be of the form `debug = \"<format_string>\"`")
                    }
                }
            };

            quote! {
                dbg.field(#name_quote, &format_args!(#format_str, &self.#name));
            }
        } else {
            quote_spanned! {
                field.span() => compile_error!("expecting only one inert `debug` attribute")
            }
        }
    });
    let debug_struct = quote! {
        let mut dbg = fmt.debug_struct(#struct_name_quote);
        #(#struct_fields_names)*
        dbg.finish()
    };

    let trait_impl = quote!(
         impl std::fmt::Debug for #struct_name {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #debug_struct
            }
        }
    );

    // trait_impl.into_token_stream().tap(|t| eprintln!("{:#}", t));
    // TokenStream::new()
    trait_impl
        .into_token_stream()
        .tap(|t| eprintln!("{:#}", t))
        .into()
}
