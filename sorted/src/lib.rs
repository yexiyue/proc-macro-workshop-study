#![allow(unused)]

use std::borrow::BorrowMut;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::Result;
use syn::{spanned::Spanned, visit_mut::VisitMut};

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::Item);
    do_expand(&st)
        .unwrap_or_else(|e| {
            let mut err_temp = e.to_compile_error();
            err_temp.extend(st.to_token_stream());
            err_temp
        })
        .into()
}

fn do_expand(st: &syn::Item) -> Result<proc_macro2::TokenStream> {
    if let syn::Item::Enum(e) = st {
        Ok(check_enum_order(e)?)
    } else {
        Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "expected enum or match expression",
        ))
    }
}

fn check_enum_order(st: &syn::ItemEnum) -> Result<proc_macro2::TokenStream> {
    let origin_order = st
        .variants
        .iter()
        .map(|v| (v.ident.to_string(), v))
        .collect::<Vec<_>>();
    let mut sorted = origin_order.clone();
    sorted.sort_by(|a, b| a.0.cmp(&b.0));
    for (a, b) in origin_order.iter().zip(sorted) {
        if a.0 != b.0 {
            return Err(syn::Error::new(
                b.1.ident.span(),
                format!("{} should sort before {}", b.0, a.0),
            ));
        }
    }
    Ok(st.into_token_stream())
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut st = syn::parse_macro_input!(input as syn::ItemFn);
    de_check_expand(&mut st)
        .unwrap_or_else(|e| {
            let mut err_temp = e.to_compile_error();
            err_temp.extend(st.to_token_stream());
            err_temp
        })
        .into()
}

fn de_check_expand(st: &mut syn::ItemFn) -> Result<proc_macro2::TokenStream> {
    let mut visit = MyVisitMut {
        error: Option::None,
    };
    visit.visit_item_fn_mut(st);
    match visit.error {
        Some(e) => Err(e),
        None => Ok(st.into_token_stream()),
    }
}

struct MyVisitMut {
    error: Option<syn::Error>,
}

impl syn::visit_mut::VisitMut for MyVisitMut {
    fn visit_expr_match_mut(&mut self, i: &mut syn::ExprMatch) {
        let mut target_idx = Option::<isize>::None;
        for (idx, attr) in i.attrs.iter_mut().enumerate() {
            if attr.path().is_ident("sorted") {
                target_idx = Some(idx as isize);
                break;
            }
        }
        match target_idx {
            Some(idx) => {
                i.attrs.remove(idx as usize);
                let mut match_arm_names: Vec<(String, &dyn ToTokens)> = vec![];
                for arm in &i.arms {
                    match &arm.pat {
                        syn::Pat::Struct(p) => {
                            match_arm_names.push((get_path_string(&p.path), &p.path));
                        }
                        syn::Pat::Path(p) => {
                            match_arm_names.push((get_path_string(&p.path), &p.path));
                        }
                        syn::Pat::TupleStruct(p) => {
                            match_arm_names.push((get_path_string(&p.path), &p.path));
                        }
                        syn::Pat::Ident(p) => {
                            match_arm_names.push((p.ident.to_string(), &p.ident));
                        }
                        syn::Pat::Wild(p) => {
                            match_arm_names.push(("_".to_string(), &p.underscore_token));
                        }
                        unsupported => {
                            self.error = Some(syn::Error::new_spanned(
                                unsupported,
                                "unsupported by #[sorted]",
                            ));
                            return;
                        }
                    }
                }

                let mut sorted_name = match_arm_names.clone();
                sorted_name.sort_by(|a, b| a.0.cmp(&b.0));

                for (a, b) in match_arm_names.iter().zip(sorted_name) {
                    if a.0 != b.0 {
                        self.error = Some(syn::Error::new_spanned(
                            b.1,
                            format!("{} should sort before {}", b.0, a.0),
                        ));
                        return;
                    }
                }

                syn::visit_mut::visit_expr_match_mut(self, i);
            }
            None => syn::visit_mut::visit_expr_match_mut(self, i),
        }
    }
}

fn get_path_string(p: &syn::Path) -> String {
    let mut buf = vec![];
    for i in p.segments.iter() {
        buf.push(i.ident.to_string());
    }
    buf.join("::")
}
