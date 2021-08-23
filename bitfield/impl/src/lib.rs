#![feature(proc_macro_quote)]
#![feature(proc_macro_diagnostic)]
use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::{DeriveInput, Ident, Token, Type, parse_macro_input};

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    // eprintln!("ARGS: {:?}", args);
    // eprintln!("INPUT: {:?}", input);
    let ast = parse_macro_input!(input as DeriveInput);

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!();
    };

    let attrs = ast.attrs;
    let vis = ast.vis;
    let ident = ast.ident;
    let generics = ast.generics;

    let size = fields.iter().map(|field| {
        let ty = field.ty.clone();
        quote!{
            <#ty as bitfield::Specifier>::BITS 
        }
    }).collect::<Vec<_>>();


    let expanded = quote! {
        #[repr(C)]
        #(#attrs)*
        #vis struct #ident #generics {
            data: [u8; (#(#size)+*) / 8usize]
        }
    };

    TokenStream::from(expanded)
}
