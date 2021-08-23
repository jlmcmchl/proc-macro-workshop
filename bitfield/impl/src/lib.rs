#![feature(proc_macro_quote)]
#![feature(proc_macro_diagnostic)]
use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::{parse_macro_input, Ident, Token, Type};

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    eprintln!("ARGS: {:?}", args);
    eprintln!("INPUT: {:?}", input);

    

    proc_macro::quote! (
        #[repr(C)]
        pub struct MyFourBytes {
            data: [u8; 4],
        }
    )
}

struct Typegen {
    pub trait_: Ident,
    pub field: Ident,
    pub ty: Type,
}

impl Parse for Typegen {
    fn parse(input: ParseStream) -> Result<Self> {
        let trait_ = input.parse()?;
        input.parse::<Token![,]>()?;
        let field = input.parse()?;
        input.parse::<Token![,]>()?;
        let ty = input.parse()?;
        Ok(Typegen { trait_, field, ty })
    }
}

#[proc_macro_attribute]
pub fn typegen(args: TokenStream, input: TokenStream) -> TokenStream {
    let typegen = parse_macro_input!(args as Typegen);

    let impls: TokenStream = (1..=64)
        .map(|i| {
            let trait_ = typegen.trait_.clone();
            let field = typegen.field.clone();
            let ty = typegen.ty.clone();
            let ident = syn::Ident::new(&format!("B{}", i), proc_macro2::Span::call_site());
            let literal = syn::LitInt::new(&format!("{}", i), proc_macro2::Span::call_site());

            quote! {
                pub enum #ident {}

                impl #trait_ for #ident {
                    const #field: #ty = #literal;
                }
            }
        })
        .collect::<proc_macro2::TokenStream>()
        .into();

    proc_macro::quote! (
        $input

        $impls
    )
}
