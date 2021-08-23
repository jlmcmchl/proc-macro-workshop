#![feature(proc_macro_quote)]
#![feature(proc_macro_diagnostic)]
use proc_macro::TokenStream;
use quote::quote;
use syn::{DeriveInput, parse_macro_input};

fn get_offset(target: &syn::Field, fields: &syn::Fields) -> proc_macro2::TokenStream {
    let base = vec![quote! {
        0
    }];

    let calculated_offset = fields.iter().take_while(|f| {
        (*f).ne(&target)
    }).map(|field| {
        let ty = field.ty.clone();
        quote!{
            <#ty as bitfield::Specifier>::BITS 
        }
    }).chain(base).collect::<Vec<_>>();

    quote! (
        #(#calculated_offset)+*
    )
}

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    // eprintln!("ARGS: {:?}", args);
    // eprintln!("INPUT: {:?}", input);
    let ast = parse_macro_input!(input as DeriveInput);

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: named,
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

    let accessor_methods = fields.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        let fname = syn::Ident::new(&format!("get_{}", ident), proc_macro2::Span::call_site());
        let ty = field.ty.clone();
        let offset = get_offset(&field, &fields);

        quote! {
            pub fn #fname(&self) -> u64 {
                let mask = 2_u64.overflowing_pow(<#ty as bitfield::Specifier>::BITS as u32).0.wrapping_sub(1) << (#offset);
                println!("{:#b}", mask);
                0
            }
        }
    }).collect::<Vec<_>>();

    let settor_methods = fields.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        let fname = syn::Ident::new(&format!("set_{}", ident), proc_macro2::Span::call_site());
        let ty = field.ty.clone();
        let offset = get_offset(&field, &fields);

        quote! {
            pub fn #fname(&mut self, arg: u64) {
                let mask = 2_u64.overflowing_pow(<#ty as bitfield::Specifier>::BITS as u32).0.wrapping_sub(1) << (#offset);
                println!("{:#b}", mask);
                
            }
        }
    }).collect::<Vec<_>>();


    let expanded = quote! {
        #[repr(C)]
        #(#attrs)*
        #[derive(std::default::Default)]
        #vis struct #ident #generics {
            data: [u8; (#(#size)+*) / 8usize]
        }

        impl #ident {
            pub fn new() -> Self {
                std::default::Default::default()
            }

            #(#accessor_methods)*

            #(#settor_methods)*
        }
    };

    TokenStream::from(expanded)
}
