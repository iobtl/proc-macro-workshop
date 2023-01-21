use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, Data, DeriveInput, Fields, GenericArgument, Ident, PathArguments, Type,
};

type IsOption = bool;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = input.ident;
    let fields: Vec<(&Ident, &Type, IsOption)> = match &input.data {
        Data::Struct(s) => match &s.fields {
            Fields::Named(fields) => fields
                .named
                .iter()
                .filter_map(|f| {
                    f.ident.as_ref().map(|ident| {
                        let (ty, is_option) = match &f.ty {
                            Type::Path(type_path) => {
                                if type_path.qself.is_none() {
                                    if let Some(segment) = type_path
                                        .path
                                        .segments
                                        .iter()
                                        .find(|segment| segment.ident == "Option")
                                    {
                                        match &segment.arguments {
                                            PathArguments::AngleBracketed(args) => {
                                                // Option must have generic type parameter
                                                // TODO: Recursive Option definition?
                                                match args.args.first() {
                                                    Some(GenericArgument::Type(ty)) => (ty, true),
                                                    _ => unreachable!(),
                                                }
                                            }
                                            _ => unreachable!(),
                                        }
                                    } else {
                                        (&f.ty, false)
                                    }
                                } else {
                                    (&f.ty, false)
                                }
                            }
                            _ => (&f.ty, false),
                        };

                        (ident, ty, is_option)
                    })
                })
                .collect(),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    let builder_struct_name = format_ident!("{}Builder", struct_name);
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

    let builder_fields_none_check_recurse = fields.iter().map(|f| {
        let name = &f.0;
        let error = format!("{} is uninstantiated", name);
        let is_option = f.2;
        quote! {
            if self.#name.is_none() && !#is_option {
                return Err(#error.into());
            }
        }
    });
    let builder_fields_none_check = quote! {
        #(#builder_fields_none_check_recurse)*
    };
    let struct_fields_recurse = fields.iter().map(|f| {
        let name = &f.0;
        let is_option = f.2;

        if is_option {
            quote! {
                #name: self.#name.take()
            }
        } else {
            quote! {
                #name: self.#name.take().unwrap()
            }
        }
    });
    let struct_fields = quote! {
        #(#struct_fields_recurse),*
    };

    let builder_fields_fn_recurse = fields.iter().map(|f| {
        let name = &f.0;
        let ty = &f.1;
        quote! {
            fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });
    let builder_fns = quote! {
       #(#builder_fields_fn_recurse)*
    };
    let builder_struct = quote! {
        pub struct #builder_struct_name {
            #builder_fields
        }

        impl #builder_struct_name {
            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                #builder_fields_none_check

                Ok(#struct_name {
                    #struct_fields
                })
            }

            #builder_fns
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
        impl #struct_name {
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
