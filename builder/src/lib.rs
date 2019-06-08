extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Ident};

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

            if let syn::GenericArgument::Type(ref t) = inner_ty.value() {
                return Some(t);
            }
        }
    }

    std::option::Option::None
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let name = ast.ident;
    let bname = format!("{}Builder", name);
    let bident = Ident::new(&bname, Span::call_site());

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!();
    };

    let optionized = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;

        if ty_inner_type("Option", ty).is_some()
            || (ty_inner_type("Vec", ty).is_some() && !field.attrs.is_empty())
        {
            quote! {
                #name: #ty
            }
        } else {
            quote! {
                #name: std::option::Option<#ty>
            }
        }
    });

    let default = fields.iter().map(|field| {
        let name = &field.ident;
        //let ty = &field.ty;

        if field.attrs.is_empty() {
            quote! { #name: std::option::Option::None }
        } else {
            quote! { #name: Vec::new() }
        }
    });

    let methods = fields.iter().filter_map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if !field.attrs.is_empty() {
            std::option::Option::None
        } else if let std::option::Option::Some(inner_ty) = ty_inner_type("Option", ty) {
            std::option::Option::Some(quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            })
        } else {
            std::option::Option::Some(quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            })
        }
    });

    let extend_methods = fields.iter().filter_map(|field| {
        let name = &field.ident;
        for attr in &field.attrs {
            attr.parse_meta().unwrap();
            if let Ok(list) = attr.parse_meta() {
                if let syn::Meta::List(syn::MetaList { nested, .. }) = list.clone() {
                    if let std::option::Option::Some(pair) = nested.first() {
                        if let syn::NestedMeta::Meta(syn::Meta::NameValue(name_value)) =
                            pair.value()
                        {
                            if name_value.ident != "each" {
                                return std::option::Option::Some(
                                    syn::Error::new_spanned(
                                        list,
                                        "expected `builder(each = \"...\")`",
                                    )
                                    .to_compile_error(),
                                );
                            } else if let syn::Lit::Str(s) = &name_value.lit {
                                let f_name = syn::Ident::new(&s.value(), s.span());
                                let inner_ty = ty_inner_type("Vec", &field.ty).unwrap();

                                return std::option::Option::Some(quote! {
                                    fn #f_name(&mut self, #f_name: #inner_ty) -> &mut Self {
                                        self.#name.push(#f_name);
                                        self
                                    }
                                });
                            }
                        }
                    }
                }
            }
        }

        std::option::Option::None
    });

    let build_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if ty_inner_type("Option", ty).is_some()
            || (ty_inner_type("Vec", ty).is_some() && !field.attrs.is_empty())
        {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let expanded = quote! {
        use std::error::Error;

        impl #name {
            pub fn builder() -> #bident {
                #bident {
                    #(#default,)*
                }
            }
        }

        pub struct #bident {
            #(#optionized,)*
        }

        impl #bident {
            #(#methods)*
            #(#extend_methods)*

            pub fn build(&self) -> std::result::Result<#name, std::boxed::Box<dyn Error>> {
                Ok(#name {
                    #(#build_fields,)*
                })
            }
        }
    };

    TokenStream::from(expanded)
}
