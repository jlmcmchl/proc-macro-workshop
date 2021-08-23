#![feature(proc_macro_span)]
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, visit_mut::VisitMut, ExprMatch, Item, ItemFn};

struct Visitor {
    error: Option<syn::Error>,
}

impl VisitMut for Visitor {
    fn visit_expr_match_mut(&mut self, i: &mut ExprMatch) {
        let sorted_attr = i
            .attrs
            .iter()
            .enumerate()
            .find(|attr| attr.1.path.segments.first().unwrap().ident == "sorted");

        match sorted_attr {
            None => {}
            Some((index, _)) => {
                i.attrs.remove(index);
            }
        }

        match is_sorted(&i.arms) {
            Ok(_) => {}
            Err(error) => self.error = Some(error),
        }
    }
}

fn is_sorted(arms: &[syn::Arm]) -> syn::Result<()> {
    for arm in arms {
        let mut arm_path = None;
        let mut arm_ident = None;
        match get_path(&arm.pat) {
            Ok(path) => arm_path = Some(path),
            Err(error) => match &arm.pat {
                syn::Pat::Ident(ident) => arm_ident = Some(ident),
                syn::Pat::Wild(_) => {
                    return Ok(());
                }
                _ => {
                    return Err(error);
                }
            },
        };

        for check in arms {
            if arm == check {
                break;
            }

            let mut check_path = None;
            let mut check_ident = None;
            match get_path(&check.pat) {
                Ok(path) => check_path = Some(path),
                Err(error) => match &check.pat {
                    syn::Pat::Ident(ident) => check_ident = Some(ident),
                    syn::Pat::Wild(_) => {
                        return Ok(());
                    }
                    _ => {
                        return Err(error);
                    }
                },
            };

            if arm_ident.is_none()
                && check_ident.is_none()
                && arm_path.is_some()
                && check_path.is_some()
                && lt(arm_path.unwrap(), check_path.unwrap())
            {
                let segments = &arm_path.unwrap().segments;
                let span = segments
                    .first()
                    .unwrap()
                    .span()
                    .unwrap()
                    .join(segments.last().unwrap().span().unwrap())
                    .unwrap();

                return Err(syn::Error::new(
                    proc_macro2::Span::from(span),
                    format!(
                        "{} should sort before {}",
                        tts(arm_path.unwrap()),
                        tts(check_path.unwrap())
                    ),
                ));
            } else if arm_ident.is_some() && check_ident.is_some() {
                if arm_ident.unwrap().ident < check_ident.unwrap().ident {
                    return Err(syn::Error::new(
                        arm_ident.span(),
                        format!(
                            "{} should sort before {}",
                            arm_ident.unwrap().ident,
                            check_ident.unwrap().ident
                        ),
                    ));
                }
            } else {
                unimplemented!();
            }
        }
    }
    Ok(())
}

fn get_path<'a>(pat: &'a syn::Pat) -> syn::Result<&'a syn::Path> {
    match pat {
        syn::Pat::Path(pat_path) => Ok(&pat_path.path),
        syn::Pat::Struct(pat_struct) => Ok(&pat_struct.path),
        syn::Pat::TupleStruct(pat_tuple_struct) => Ok(&pat_tuple_struct.path),
        pat => Err(syn::Error::new(
            pat.span(),
            "unsupported by #[remain::sorted]",
        )),
    }
}

fn lt(curr_path: &syn::Path, other_path: &syn::Path) -> bool {
    for (curr, other) in curr_path.segments.iter().zip(other_path.segments.iter()) {
        if curr.ident > other.ident {
            return false;
        } else if curr.ident < other.ident {
            return true;
        }
    }

    false
}

fn tts(path: &syn::Path) -> proc_macro2::TokenStream {
    quote! {#path}
}

fn enum_check(item: &Item) -> syn::Result<&Item> {
    match item {
        syn::Item::Enum(ref item_enum) => {
            for current in item_enum.variants.clone() {
                for check in item_enum.variants.clone() {
                    if current == check {
                        break;
                    }

                    if check.ident > current.ident {
                        return Err(syn::Error::new(
                            current.ident.span(),
                            format!("{} should sort before {}", current.ident, check.ident),
                        ));
                    }
                }
            }
        }
        _ => {
            return Err(syn::Error::new(
                Span::call_site(),
                "expected enum or match expression",
            ));
        }
    }

    Ok(item)
}

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let ast = parse_macro_input!(input as Item);
    let mut error = None;

    if let Err(err) = enum_check(&ast) {
        error = Some(err.to_compile_error());
    }

    let converted = quote! {
        #ast
        #error
    };

    TokenStream::from(converted)
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let mut ast = parse_macro_input!(input as ItemFn);

    let mut visitor = Visitor { error: None };
    visitor.visit_item_fn_mut(&mut ast);
    let error = match visitor.error {
        None => None,
        Some(err) => Some(err.to_compile_error()),
    };

    let converted = quote! {
        #ast
        #error
    };

    TokenStream::from(converted)
}
