use std::collections::HashMap;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse2,
    visit::{self, Visit},
};
use syn::{parse_quote, token::Comma, Expr, Field, Result};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    do_expand(&st)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

/// 进行错误包装
fn do_expand(st: &syn::DeriveInput) -> Result<proc_macro2::TokenStream> {
    let mut res = proc_macro2::TokenStream::new();
    let struct_name = &st.ident;
    let fields = get_struct_fields(st)?;

    let debug_inner = generate_debug_inner(&fields)?;

    // 第4关，处理范型参数
    let mut generics = st.generics.clone();
    // 第8关，逃生出口
    if let Some(escape_hatch) = get_escape_hatch(st)? {
        generics.make_where_clause();
        generics
            .where_clause
            .as_mut()
            .unwrap()
            .predicates
            .push(syn::parse_str(&escape_hatch).unwrap());
    } else {
        // 第5关
        let mut phantom_data_params_type = vec![];
        let mut data_type = vec![];

        for field in fields {
            if let Some(tp) = get_phantom_type_inner(field)? {
                phantom_data_params_type.push(tp);
            }
            if let Some(tp) = get_type_name(field)? {
                data_type.push(tp);
            }
        }
        // 第7关
        let generics_associate_types = get_generics_associated_types(st);
        // 添加范型限定
        for param in generics.params.iter_mut() {
            if let syn::GenericParam::Type(tp) = param {
                let type_param_name = &tp.ident.to_string();

                // 如果范型在PhantomData中存在，且没有被结构体字段使用则跳过
                if !data_type.contains(type_param_name)
                    && phantom_data_params_type.contains(type_param_name)
                {
                    continue;
                }

                // 如果关联类型存在，且关联类型在结构体字段中没有被使用则跳过
                if generics_associate_types.contains_key(type_param_name)
                    && !data_type.contains(type_param_name)
                {
                    continue;
                }

                tp.bounds.push(parse_quote!(std::fmt::Debug))
            }
        }
        // 第7关，为关联类型添加where_case
        generics.make_where_clause();
        for (_, associate_type) in generics_associate_types {
            for tp in associate_type {
                generics
                    .where_clause
                    .as_mut()
                    .unwrap()
                    .predicates
                    .push(parse_quote!(#tp: std::fmt::Debug));
            }
        }
    }

    // 使用工具函数获取3个impl代码块
    let (impl_generics, ty_generics, where_case) = generics.split_for_impl();
    let custom_debug = quote! {
        impl #impl_generics std::fmt::Debug for #struct_name #ty_generics #where_case  {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!(#struct_name))
                    #debug_inner
                    .finish()
            }
        }
    };
    res.extend(custom_debug);
    Ok(res)
}

/// 获取struct的字段
fn get_struct_fields(st: &syn::DeriveInput) -> Result<&syn::punctuated::Punctuated<Field, Comma>> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = &st.data
    {
        Ok(named)
    } else {
        Err(syn::Error::new_spanned(st, "only support named struct"))
    }
}

/// 获取字段属性NamedValue
fn get_struct_field_attribute(field: &syn::Field) -> Result<Option<String>> {
    if let Some(syn::Attribute {
        meta: syn::Meta::NameValue(syn::MetaNameValue { path, value, .. }),
        ..
    }) = &field.attrs.first()
    {
        if path.is_ident("debug") {
            if let Expr::Lit(syn::ExprLit { lit, .. }) = value {
                if let syn::Lit::Str(v) = &lit {
                    return Ok(Some(v.value().to_string()));
                } else {
                    return Err(syn::Error::new_spanned(lit, "the value must be a string"));
                }
            }
        } else {
            return Err(syn::Error::new_spanned(
                path,
                "only support debug attribute",
            ));
        }
    }
    Ok(None)
}

/// 生成debug方法内部关键字段
fn generate_debug_inner(
    fields: &syn::punctuated::Punctuated<Field, Comma>,
) -> Result<proc_macro2::TokenStream> {
    let mut res = proc_macro2::TokenStream::new();
    for field in fields {
        let field_name = &field.ident;
        let debug_args = get_struct_field_attribute(field)?;

        if let Some(v) = debug_args {
            res.extend(quote! {
               .field(stringify!(#field_name), &format_args!(#v, self.#field_name))
            });
        } else {
            res.extend(quote! {
                .field(stringify!(#field_name), &self.#field_name)
            });
        }
    }
    Ok(res)
}

///  获取范型参数内部的范型
fn get_phantom_type_inner(field: &syn::Field) -> Result<Option<String>> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = &field.ty
    {
        if let Some(syn::PathSegment { ident, arguments }) = segments.first() {
            if ident == "PhantomData" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    args,
                    ..
                }) = arguments
                {
                    if let Some(syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
                        path: syn::Path { segments, .. },
                        ..
                    }))) = args.first()
                    {
                        if let Some(syn::PathSegment { ident, .. }) = segments.first() {
                            return Ok(Some(ident.to_string()));
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}

/// 获取类型名称
fn get_type_name(field: &syn::Field) -> Result<Option<String>> {
    if let syn::Type::Path(syn::TypePath { path, .. }) = &field.ty {
        if let Some(syn::PathSegment { ident, .. }) = path.segments.last() {
            return Ok(Some(ident.to_string()));
        }
    }
    Ok(None)
}

// 第7关
struct TypePathVisitor {
    generics_type_names: Vec<String>,
    associated_types: HashMap<String, Vec<syn::TypePath>>,
}

impl<'ast> Visit<'ast> for TypePathVisitor {
    fn visit_type_path(&mut self, i: &'ast syn::TypePath) {
        if i.path.segments.len() >= 2 {
            let generic_type_name = i.path.segments.first().unwrap().ident.to_string();
            if self.generics_type_names.contains(&generic_type_name) {
                self.associated_types
                    .entry(generic_type_name)
                    .or_insert(Vec::new())
                    .push(i.clone());
            }
        }
        visit::visit_type_path(self, i);
    }
}

///  获取范型参数关联类型
fn get_generics_associated_types(st: &syn::DeriveInput) -> HashMap<String, Vec<syn::TypePath>> {
    let generics_type_names = st
        .generics
        .params
        .iter()
        .filter_map(|p| {
            if let syn::GenericParam::Type(tp) = p {
                return Some(tp.ident.to_string());
            }
            return None;
        })
        .collect::<Vec<String>>();
    let mut visitor = TypePathVisitor {
        generics_type_names,
        associated_types: HashMap::new(),
    };
    visitor.visit_derive_input(st);
    visitor.associated_types
}

/// 第8关，逃生出口
fn get_escape_hatch(st: &syn::DeriveInput) -> Result<Option<String>> {
    if let Some(syn::Attribute {
        meta: syn::Meta::List(syn::MetaList { path, tokens, .. }),
        ..
    }) = st.attrs.last()
    {
        if path.is_ident("debug") {
            let named_value: syn::MetaNameValue = parse2(tokens.clone())?;
            if named_value.path.is_ident("bound") {
                if let Expr::Lit(syn::ExprLit { lit, .. }) = named_value.value {
                    if let syn::Lit::Str(v) = &lit {
                        return Ok(Some(v.value().to_string()));
                    } else {
                        return Err(syn::Error::new_spanned(lit, "the value must be a string"));
                    }
                }
            } else {
                return Err(syn::Error::new_spanned(
                    tokens,
                    "only support bound attribute",
                ));
            }
        } else {
            return Err(syn::Error::new_spanned(
                path,
                "only support debug attribute",
            ));
        }
    }
    Ok(None)
}
