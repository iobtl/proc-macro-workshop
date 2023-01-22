use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DataStruct, DeriveInput, Fields,
    FieldsNamed, GenericArgument, Ident, Lit, Meta, MetaList, NestedMeta, PathArguments,
    PathSegment, Type,
};

#[derive(PartialEq, Eq)]
enum ContainerType {
    Option,
    Vec,
}

struct FieldTy<'a> {
    ty: &'a Type,
    container_type: Option<ContainerType>,
}

struct FieldInfo<'a> {
    ident: &'a Ident,
    ty: FieldTy<'a>,
    attribute_name: Option<String>,
}

fn extract_contained_ty(segment: &PathSegment) -> &Type {
    match &segment.arguments {
        PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => {
            match args.first() {
                Some(GenericArgument::Type(ty)) => ty,
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = input.ident;
    let fields: Result<Vec<FieldInfo>, syn::Error> = match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => named
            .iter()
            .filter_map(|f| {
                f.ident.as_ref().map(|ident| {
                    let attribute_name = match f.attrs.iter().find(|attr| {
                        attr.path
                            .segments
                            .iter()
                            .any(|segment| segment.ident == "builder")
                    }) {
                        Some(attr) => {
                            let attr_err = |meta: Option<&MetaList>, msg: &str| {
                                let mut token_stream = proc_macro2::TokenStream::new();
                                if let Some(s) =
                                    attr.path.segments.iter().find(|s| s.ident == "builder")
                                {
                                    s.ident.to_tokens(&mut token_stream);
                                } else {
                                    attr.to_tokens(&mut token_stream);
                                }

                                // If the attribute has an additional (key = value) MetaList
                                if let Some(m) = meta {
                                    m.to_tokens(&mut token_stream);
                                }

                                Err(syn::Error::new_spanned(token_stream, msg))
                            };

                            match attr.parse_meta() {
                                Ok(Meta::List(meta_list)) => meta_list
                                    .nested
                                    .iter()
                                    .map(|m| match m {
                                        NestedMeta::Meta(Meta::NameValue(name_value)) => {
                                            if name_value.path.segments.len() == 1
                                                && name_value
                                                    .path
                                                    .segments
                                                    .first()
                                                    .map(|s| s.ident == "each")
                                                    .unwrap_or(false)
                                            {
                                                match &name_value.lit {
                                                    Lit::Str(s) => Ok(Some(s.value())),
                                                    _ => attr_err(
                                                        Some(&meta_list),
                                                        "argument to `each` must be a string",
                                                    ),
                                                }
                                            } else {
                                                attr_err(
                                                    Some(&meta_list),
                                                    "expected `builder(each = \"...\")`",
                                                )
                                            }
                                        }
                                        _ => attr_err(
                                            Some(&meta_list),
                                            "expected `builder(each = \"...\")`",
                                        ),
                                    })
                                    .next()
                                    .unwrap_or_else(|| {
                                        attr_err(None, "expected `builder(each = \"...\")`")
                                    }),
                                Ok(_) => attr_err(None, "expected `builder(each = \"...\")`"),
                                e => e.map(|_| Some(String::new())),
                            }
                        }
                        _ => Ok(None),
                    }?;
                    let field_ty = match &f.ty {
                        Type::Path(type_path) if type_path.qself.is_none() => type_path
                            .path
                            .segments
                            .iter()
                            .find_map(|segment| {
                                if segment.ident == "Option" {
                                    Some(FieldTy {
                                        ty: extract_contained_ty(segment),
                                        container_type: Some(ContainerType::Option),
                                    })
                                } else if segment.ident == "Vec" {
                                    Some(FieldTy {
                                        ty: extract_contained_ty(segment),
                                        container_type: Some(ContainerType::Vec),
                                    })
                                } else {
                                    None
                                }
                            })
                            .unwrap_or(FieldTy {
                                ty: &f.ty,
                                container_type: None,
                            }),
                        _ => FieldTy {
                            ty: &f.ty,
                            container_type: None,
                        },
                    };

                    Ok(FieldInfo {
                        ident,
                        ty: field_ty,
                        attribute_name,
                    })
                })
            })
            .collect(),
        _ => unimplemented!(),
    };
    let fields = match fields {
        Ok(f) => f,
        Err(e) => return e.into_compile_error().into(),
    };

    let builder_struct_name = format_ident!("{}Builder", struct_name);
    let builder_fields_recurse = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty.ty;

        match &f.ty.container_type {
            Some(ContainerType::Vec) => {
                quote! {
                    #name: std::vec::Vec<#ty>
                }
            }
            _ => {
                quote! {
                    #name: std::option::Option<#ty>
                }
            }
        }
    });
    let builder_fields = quote! {
        #(#builder_fields_recurse),*
    };

    let builder_fields_check_recurse = fields.iter().map(|f| {
        let name = &f.ident;
        let error = format!("{} is uninstantiated", name);

        match f.ty.container_type.as_ref() {
            Some(_) => {
                quote!()
            }
            None => {
                quote! {
                    if self.#name.is_none() {
                        return std::result::Result::Err(#error.into());
                    }
                }
            }
        }
    });
    let builder_fields_check = quote! {
        #(#builder_fields_check_recurse)*
    };
    let struct_fields_recurse = fields.iter().map(|f| {
        let name = &f.ident;
        match f.ty.container_type.as_ref() {
            Some(ContainerType::Option) => {
                quote! {
                    #name: self.#name.take()
                }
            }
            Some(ContainerType::Vec) => {
                quote! {
                    #name: std::mem::take(&mut self.#name)
                }
            }
            _ => {
                quote! {
                    #name: self.#name.take().unwrap()
                }
            }
        }
    });
    let struct_fields = quote! {
        #(#struct_fields_recurse),*
    };

    let builder_fields_fn_recurse = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty.ty;

        // `ty` contains the unwrapped/containing type
        let default_fn = match f.ty.container_type.as_ref() {
            Some(ContainerType::Vec) => quote! {
                fn #name(&mut self, #name: std::vec::Vec<#ty>) -> &mut Self {
                    self.#name = #name;
                    self
                }
            },
            _ => quote! {
                fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            },
        };

        // Assuming that fields have type Vec
        // TODO: emit compile_error otherwise
        if let Some(attr_name) = f.attribute_name.as_ref() {
            // Don't generate all-at-once builder method if given attribute
            // name is same as original struct field name
            if f.ty.container_type == Some(ContainerType::Vec) {
                let single_name = format_ident!("{}", attr_name);
                let single_add_fn = quote! {
                    fn #single_name(&mut self, #single_name: #ty) -> &mut Self {
                        self.#name.push(#single_name);
                        self
                    }
                };

                if name == &attr_name.as_str() {
                    return quote! {
                        #single_add_fn
                    };
                } else {
                    return quote! {
                        #default_fn
                        #single_add_fn
                    };
                }
            }
        }

        default_fn
    });
    let builder_fns = quote! {
       #(#builder_fields_fn_recurse)*
    };
    let builder_struct = quote! {
        pub struct #builder_struct_name {
            #builder_fields
        }

        impl #builder_struct_name {
            pub fn build(&mut self) -> std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
                #builder_fields_check

                Ok(#struct_name {
                    #struct_fields
                })
            }

            #builder_fns
        }
    };

    // Generated builder impl
    let builder_impl_recurse = fields.iter().map(|f| {
        let name = &f.ident;

        quote! {
            #name: std::default::Default::default()
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
