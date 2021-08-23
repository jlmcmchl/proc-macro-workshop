use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, parse_quote, DeriveInput, Generics};

fn ty_inner_type<'a>(wrapper: &str, ty: &'a syn::Type) -> std::option::Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments.iter().last().unwrap().ident != wrapper {
            return std::option::Option::None;
        }

        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.iter().len() != 1 {
                return std::option::Option::None;
            }
            let inner_ty = inner_ty.args.first().unwrap();

            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        }
    }

    std::option::Option::None
}

fn subtype<'a>(wrapper: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = ty
    {
        if !segments.is_empty() && segments[0].ident == wrapper {
            if let syn::PathArguments::AngleBracketed(ref args) = segments[0].arguments {
                if let syn::GenericArgument::Type(ref wrapped_type) = args.args[0] {
                    return Some(&wrapped_type);
                }
            }
        }
    }
    None
}

fn type_ident(ty: &syn::Type) -> syn::Ident {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = ty
    {
        if !segments.is_empty() {
            return segments[0].ident.clone();
        } else {
            panic!("Expected an identity but found none");
        }
    }

    panic!("Expected an identity but found none");
}

fn add_trait_bounds(mut generics: Generics, adds: Vec<&syn::TypeParam>) -> Generics {
    for param in generics.type_params_mut() {
        for req_param in adds.clone() {
            if req_param.ident == param.ident {
                param.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }
    }

    generics
}

fn add_where_clauses(mut generics: Generics, wheres: Vec<&syn::Type>) -> Generics {
    let where_clause = generics.make_where_clause();

    for where_item in wheres {
        let predicate = parse_quote!(#where_item: std::fmt::Debug);

        where_clause.predicates.push(predicate);
    }

    generics
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let name = ast.ident;

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!();
    };

    let phantom_data_types: Vec<&syn::Type> = fields
        .iter()
        .filter_map(|field| subtype("PhantomData", &field.ty))
        .collect();

    let attr_bounds: Vec<syn::WherePredicate> = ast
        .attrs
        .iter()
        .filter_map(|attr| {
            if let Ok(syn::Meta::List(list)) = attr.parse_meta() {
                if list.path.get_ident().unwrap().to_string() != "debug" {
                    return None;
                }

                for meta in list.nested {
                    match meta {
                        syn::NestedMeta::Meta(syn::Meta::NameValue(name_value)) => {
                            assert_eq!(name_value.path.get_ident().unwrap().to_string(), "bound");

                            if let syn::Lit::Str(lit_str) = name_value.lit {
                                return Some(lit_str.parse().unwrap());
                            }

                            return None;
                        }
                        _ => panic!("Expected a NameValue"),
                    }
                }
            }
            None
        })
        .collect();

    let required_types = ast
        .generics
        .type_params()
        .filter(|ty_param| {
            for phantom_ty in phantom_data_types.clone() {
                if type_ident(&phantom_ty) == ty_param.ident {
                    return false;
                }
            }

            ty_param.bounds.is_empty()
        })
        .collect();

    let mut generics = add_trait_bounds(ast.generics.clone(), required_types);

    if attr_bounds.is_empty() {
        let associated_type_params: Vec<&syn::Type> = fields
            .iter()
            .filter_map(|field| ty_inner_type("Vec", &field.ty))
            .collect();

        generics = add_where_clauses(generics, associated_type_params);
    } else {
        let bound = attr_bounds.get(0).unwrap();

        generics.make_where_clause().predicates.push(bound.clone());
    }

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let field_method_calls = fields.iter().map(|field| {
        let name = &field.ident;

        if field.attrs.is_empty() {
            quote! {
                field(stringify!(#name), &self.#name)
            }
        } else if let Ok(syn::Meta::NameValue(name_value)) = field.attrs[0].parse_meta() {
            if name_value.path.get_ident().unwrap().to_string() != "debug" {
                return syn::Error::new_spanned(name_value, "expected `debug = \"...\"`")
                    .to_compile_error();
            }

            match name_value.lit {
                syn::Lit::Str(lit_str) => {
                    let fmt_str = lit_str.value();

                    return quote! {
                        field(stringify!(#name), &format_args!(#fmt_str, &self.#name))
                    };
                }
                _ => {
                    return syn::Error::new_spanned(name_value, "expected `debug = \"...\"`")
                        .to_compile_error()
                }
            }
        } else {
            unimplemented!()
        }
    });

    let expanded = quote! {
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                fmt.debug_struct(stringify!(#name))
                    #(.#field_method_calls)*
                    .finish()
            }
        }
    };

    TokenStream::from(expanded)
}
