use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use syn::{parse_macro_input, Ident, LitInt, Token};

#[derive(Clone)]
struct Seq {
    ident: Ident,
    start_range: LitInt,
    end_range: LitInt,
    body: Vec<proc_macro2::TokenTree>,
    inclusive: Option<Token![=]>,
}

impl Seq {
    fn range(&self) -> std::ops::Range<u64> {
        self.start_range.value()..self.end_range.value()
    }

    fn range_inclusive(&self) -> std::ops::RangeInclusive<u64> {
        self.start_range.value()..=self.end_range.value()
    }
}

impl syn::parse::Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let ident: Ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let start_range: LitInt = input.parse()?;
        input.parse::<Token![..]>()?;
        let inclusive = input.parse::<Token![=]>().ok();
        let end_range: LitInt = input.parse()?;

        let group: proc_macro2::Group = input.parse()?;
        let body = group.stream().into_iter().collect();

        Ok(Seq {
            ident,
            start_range,
            end_range,
            body,
            inclusive,
        })
    }
}

fn recurse_repeat_replace(
    seq: &Seq,
    literal: &proc_macro2::Literal,
    out: &mut Vec<proc_macro2::TokenTree>,
) {
    let mut skip = 0;
    for i in 0..seq.body.len() {
        if skip > 0 {
            skip -= 1;
            continue;
        }

        match &seq.body[i] {
            TokenTree::Group(group) => {
                let mut new_seq = seq.clone();
                let subtree = group.stream().into_iter().collect();
                new_seq.body = subtree;

                let mut tree_vec = Vec::new();
                recurse_repeat_replace(&new_seq, literal, &mut tree_vec);

                out.push(TokenTree::Group(proc_macro2::Group::new(
                    group.delimiter(),
                    proc_macro2::TokenStream::from_iter(tree_vec),
                )));
            }
            TokenTree::Ident(tree_ident) => {
                let mut none = true;
                if tree_ident == &seq.ident {
                    out.push(TokenTree::Literal(literal.clone()));
                    none = false;
                } else if i < seq.body.len() - 2 {
                    if let TokenTree::Punct(next_punct) = &seq.body[i + 1] {
                        if let TokenTree::Ident(next_ident) = &seq.body[i + 2] {
                            if next_punct.as_char() == '#' && next_ident == &seq.ident {
                                out.push(TokenTree::Ident(proc_macro2::Ident::new(
                                    &format!("{}{}", tree_ident, literal),
                                    tree_ident.span(),
                                )));

                                skip = 2;
                                none = false;
                            }
                        }
                    }
                }

                if none {
                    out.push(TokenTree::Ident(tree_ident.clone()))
                }
            }
            TokenTree::Punct(tree_punct) => {
                let mut none = true;
                if i < seq.body.len() - 2 && tree_punct.as_char() == '#' {
                    if let TokenTree::Group(next_group) = &seq.body[i + 1] {
                        if let TokenTree::Punct(next_punct) = &seq.body[i + 2] {
                            if next_punct.as_char() == '*'
                                && next_group.delimiter() == proc_macro2::Delimiter::Parenthesis
                            {
                                if seq.inclusive.is_some() {
                                    for j in seq.range_inclusive() {
                                        let mut new_seq = seq.clone();
                                        let subtree = next_group.stream().into_iter().collect();
                                        new_seq.body = subtree;

                                        let new_literal = proc_macro2::Literal::u64_unsuffixed(j);

                                        let mut tree_vec = Vec::new();
                                        recurse_repeat_replace(
                                            &new_seq,
                                            &new_literal,
                                            &mut tree_vec,
                                        );

                                        out.extend(tree_vec);

                                        skip = 2;
                                        none = false;
                                    }
                                } else {
                                    for j in seq.range() {
                                        let mut new_seq = seq.clone();
                                        let subtree = next_group.stream().into_iter().collect();
                                        new_seq.body = subtree;

                                        let new_literal = proc_macro2::Literal::u64_unsuffixed(j);

                                        let mut tree_vec = Vec::new();
                                        recurse_repeat_replace(
                                            &new_seq,
                                            &new_literal,
                                            &mut tree_vec,
                                        );

                                        out.extend(tree_vec);

                                        skip = 2;
                                        none = false;
                                    }
                                }
                            }
                        }
                    }
                }

                if none {
                    out.push(TokenTree::Punct(tree_punct.clone()));
                }
            }
            TokenTree::Literal(tree_literal) => out.push(TokenTree::Literal(tree_literal.clone())),
        }
    }
}

fn has_repetition_syntax(body: &Vec<TokenTree>) -> bool {
    for i in 0..body.len() {
        match &body[i] {
            TokenTree::Punct(punct) => {
                if i < body.len() - 2 && punct.as_char() == '#' {
                    if let TokenTree::Group(next_group) = &body[i + 1] {
                        if let TokenTree::Punct(next_punct) = &body[i + 2] {
                            if next_punct.as_char() == '*'
                                && next_group.delimiter() == proc_macro2::Delimiter::Parenthesis
                            {
                                return true;
                            }
                        }
                    }
                }
            }
            TokenTree::Group(group) => {
                if has_repetition_syntax(&group.stream().into_iter().collect()) {
                    return true;
                }
            }
            _ => {}
        }
    }

    false
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = parse_macro_input!(input as Seq);

    let mut expanded = Vec::new();
    if has_repetition_syntax(&seq.body) {
        let literal = proc_macro2::Literal::u64_unsuffixed(0);
        recurse_repeat_replace(&seq, &literal, &mut expanded);
    } else if seq.inclusive.is_some() {
        for i in seq.range_inclusive() {
            let literal = proc_macro2::Literal::u64_unsuffixed(i);
            recurse_repeat_replace(&seq, &literal, &mut expanded);
        }
    } else {
        for i in seq.range() {
            let literal = proc_macro2::Literal::u64_unsuffixed(i);
            recurse_repeat_replace(&seq, &literal, &mut expanded);
        }
    }

    TokenStream::from(proc_macro2::TokenStream::from_iter(expanded))
}
