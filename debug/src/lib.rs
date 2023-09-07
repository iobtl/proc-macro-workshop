use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, Data, DeriveInput};
use tap::{Pipe, Tap};

#[proc_macro_derive(CustomDebug)]
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

        quote! {
            dbg.field(#name_quote, &self.#name);
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
