#![feature(proc_macro_quote)]
#![feature(proc_macro_diagnostic)]
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn get_offset(target: &syn::Field, fields: &syn::Fields) -> proc_macro2::TokenStream {
    let base = vec![quote! {
        0
    }];

    let calculated_offset = fields
        .iter()
        .take_while(|f| (*f).ne(&target))
        .map(|field| {
            let ty = field.ty.clone();
            quote! {
                <#ty as bitfield::Specifier>::BITS
            }
        })
        .chain(base)
        .collect::<Vec<_>>();

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

    let fields = if let syn::Data::Struct(syn::DataStruct { fields: named, .. }) = ast.data {
        named
    } else {
        unimplemented!();
    };

    let attrs = ast.attrs;
    let vis = ast.vis;
    let ident = ast.ident;
    let generics = ast.generics;

    let size = fields
        .iter()
        .map(|field| {
            let ty = field.ty.clone();
            quote! {
                <#ty as bitfield::Specifier>::BITS
            }
        })
        .collect::<Vec<_>>();

    let methods = fields.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        let get_fname = syn::Ident::new(&format!("get_{}", ident), proc_macro2::Span::call_site());
        let set_fname = syn::Ident::new(&format!("set_{}", ident), proc_macro2::Span::call_site());
        let ty = field.ty.clone();
        let offset = get_offset(&field, &fields);

        quote! {
            pub fn #get_fname(&self) -> <#ty as bitfield::Specifier>::SIGNATURE {
                let offset = #offset;
                let len = <#ty as bitfield::Specifier>::BITS;
                let mut res: u64 = 0;

                let mut mask = 2_u64.overflowing_pow(<#ty as bitfield::Specifier>::BITS as u32).0.wrapping_sub(1) << offset;

                let mut ind = 0;
                while mask > 0 {
                    let temp = self.data[ind] & ((mask & 0xff) as u8);

                    res |= (temp as u64) << (8 * ind);
                    // println!("READ {} {:#b} {:#b} {:#b} {:#b}", ind, self.data[ind], temp, mask, res);

                    mask >>= 8;

                    ind += 1;
                }
                // println!("READ {:#b}", res);
                std::convert::TryInto::<<#ty as bitfield::Specifier>::SIGNATURE>::try_into(res >> offset).unwrap()
            }

            pub fn #set_fname(&mut self, arg: <#ty as bitfield::Specifier>::SIGNATURE) {
                let arg = arg as u64;
                let offset = #offset;
                let len = <#ty as bitfield::Specifier>::BITS;
                let mut arg = arg << offset;

                let mut mask = 2_u64.overflowing_pow(<#ty as bitfield::Specifier>::BITS as u32).0.wrapping_sub(1) << offset;

                let mut ind = 0;
                while arg > 0 {
                    // println!("WRIT PRE {:?} {:#b}", self.data, arg);

                    // first - clear any bits set in mask
                    self.data[ind] &= !((mask & 0xff) as u8);
                    // println!("WRIT CLR {:?}", self.data);

                    // second - set bits according to mask
                    self.data[ind] |= (arg & mask & 0xff) as u8;

                    // println!("WRIT SET {:?}", self.data);

                    mask >>= 8;
                    arg >>= 8;

                    ind += 1;
                }
                // println!("WRIT END {:?}", self.data);
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

            #(#methods)*
        }
    };

    TokenStream::from(expanded)
}
