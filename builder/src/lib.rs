use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident, Type};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let fields: Vec<(&Ident, &Type)> = match &input.data {
        Data::Struct(s) => match &s.fields {
            Fields::Named(fields) => fields
                .named
                .iter()
                .filter_map(|f| f.ident.as_ref().map(|ident| (ident, &f.ty)))
                .collect(),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    let builder_struct_name = format_ident!("{}Builder", name);
    let builder_fields_recurse = fields.iter().map(|f| {
        let name = &f.0;
        let ty = &f.1;
        quote! {
            #name: Option<#ty>
        }
    });
    let builder_fields = quote! {
        #(#builder_fields_recurse),*
    };
    let builder_struct = quote! {
        pub struct #builder_struct_name {
            #builder_fields
        }
    };

    // Generated builder impl
    let builder_impl_recurse = fields.iter().map(|f| {
        let name = &f.0;
        quote! {
            #name: None
        }
    });
    let builder_impl_body = quote! {
        #(#builder_impl_recurse),*
    };
    let builder_impl = quote! {
        impl #name {
            pub fn builder() -> #builder_struct_name {
                #builder_struct_name {
                    #builder_impl_body
                }
            }
        }
    };
    let expanded = quote! {
        #builder_impl
        #builder_struct
    };

    // panic!("{}", proc_macro2::TokenStream::to_string(&expanded));

    TokenStream::from(expanded)
}
