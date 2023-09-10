use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Data, DataStruct, DeriveInput, Fields,
    FieldsNamed, GenericArgument, GenericParam, Generics, Ident, Path, PathArguments, Type,
    TypePath,
};
use tap::{Pipe, Tap};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    eprintln!("{:#?}", input);

    let phantom_data_ty_params = extract_phantom_data_params(&input.data);
    let generics = add_trait_bounds(input.generics, &phantom_data_ty_params);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

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
         impl #impl_generics std::fmt::Debug for #struct_name #ty_generics #where_clause {
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

fn extract_phantom_data_params(data: &Data) -> Vec<Ident> {
    let mut params = Vec::new();

    match data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => {
            for named_field in named.iter() {
                if let Type::Path(TypePath {
                    path: Path { segments, .. },
                    ..
                }) = &named_field.ty
                {
                    for segment in segments.iter() {
                        if segment.ident == "PhantomData" {
                            match &segment.arguments {
                                PathArguments::AngleBracketed(args) => {
                                    for arg in args.args.iter() {
                                        match arg {
                                            GenericArgument::Type(ty) => match ty {
                                                Type::Path(TypePath {
                                                    path: Path { segments, .. },
                                                    ..
                                                }) => {
                                                    if let Some(brack_segment) =
                                                        segments.iter().next()
                                                    {
                                                        params.push(brack_segment.ident.clone());
                                                    }
                                                }
                                                _ => unimplemented!(),
                                            },
                                            _ => unimplemented!(),
                                        }
                                    }
                                }
                                _ => unimplemented!(),
                            }
                        }
                    }
                }
            }
        }
        _ => unimplemented!(),
    }

    params
}

fn add_trait_bounds(mut generics: Generics, phantom_data_ty_params: &[Ident]) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(type_params) = param {
            if !phantom_data_ty_params.contains(&type_params.ident) {
                type_params.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }
    }

    generics
}
