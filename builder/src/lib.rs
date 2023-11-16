use proc_macro::TokenStream;
use quote::quote;
// syn包相当于把tokenStream转换成更有语义话的语法树
// 导入Spanned trait才能调用span() 方法 或者直接使用proc_macro2中的 proc_macro2::Span::call_site()
use syn::{
    punctuated::Punctuated, spanned::Spanned, token::Comma, Data, DataStruct, DeriveInput, Field,
    Fields, FieldsNamed, Result,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    // 把派生宏描述的代码转换成DeriverInput
    let derive_input = syn::parse_macro_input!(input as syn::DeriveInput);
    do_expand(&derive_input)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

/// 展开代码
fn do_expand(st: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let struct_name_literal = st.ident.to_string();
    let builder_name_literal = format!("{}Builder", struct_name_literal);
    let builder_name_ident = syn::Ident::new(&builder_name_literal, st.span());

    // quote宏中不支持点读取属性
    let struct_ident = &st.ident;

    let builder_fields = generate_builder_struct_fields(st)?;

    let init_builder_fields = generate_builder_struct_fields_init(st)?;

    let builder_fus = generate_builder_setter_function(st)?;

    // build方法
    let build_method = generate_builder_build(st)?;
    let build_return = generate_builder_build_return(st)?;
    // 使用quote生成代码 (注意要使用花括号，如果使用圆括号可能导致生成的代码不正确)
    let res = quote! {
        pub struct #builder_name_ident{
            #builder_fields
        }

        impl #struct_ident {
           pub fn builder()->#builder_name_ident{
                #builder_name_ident {
                    #(#init_builder_fields),*
                }
           }
        }
        impl #builder_name_ident{
            #builder_fus

            pub fn build(&self)->std::result::Result<#struct_ident,std::boxed::Box<dyn std::error::Error>>{
                #build_method
                Ok(#struct_ident{
                    #build_return
                })
            }
        }
    };

    Ok(res)
}

type StructFields = Punctuated<Field, Comma>;

/// 获取结构体内部的字段
fn get_struct_fields(st: &DeriveInput) -> Result<&StructFields> {
    if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { named, .. }),
        ..
    }) = &st.data
    {
        return Ok(named);
    }
    Err(syn::Error::new_spanned(&st, "only support named struct"))
}

/// 生成Builder结构体中的字段
fn generate_builder_struct_fields(st: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let fields = get_struct_fields(st)?;
    // 方案1 通过循环和变量生成结果
    // let mut res = quote! {};
    // for field in fields {
    //     let field_name = &field.ident;
    //     let field_type = &field.ty;
    //     res = quote! {
    //         #res
    //         pub #field_name: Option<#field_type>
    //     };
    // }

    // 方案2 先转换成数组结构，通过声明宏语法来生成代码
    // // 第6关对类型如果是Option的特殊处理
    // let ident: Vec<_> = fields.iter().map(|field| &field.ident).collect();
    // // 如果是Option包裹🉐的则把里面的Option提取出来，然后再处理
    // let types: Vec<_> = fields
    //     .iter()
    //     .map(|field| get_optional_inner_type(&field.ty, "Option").unwrap_or(&field.ty))
    //     .collect();
    // // 使用声明宏的循环语法，但是$要换成#号
    // Ok(quote! {
    //     #( #ident: std::option::Option<#types> ) , *
    // })

    // 第7关重构
    let mut res = proc_macro2::TokenStream::new();
    for field in fields {
        let field_name = &field.ident;
        let field_type = &field.ty;
        if get_optional_inner_type(&field.ty, "Option").is_some() {
            res.extend(quote! {
                #field_name: #field_type,
            });
            // 第7关 如果是用builder(each=“”)属性修饰的则视为Vec不用包Option
        } else if get_builder_each_attribute(&field)?.is_some() {
            res.extend(quote! {
                #field_name: #field_type,
            });
        } else {
            res.extend(quote! {
                #field_name: std::option::Option<#field_type>,
            });
        }
    }
    Ok(res)
}

/// 生成builder方法中Builder初始化字段
fn generate_builder_struct_fields_init(st: &DeriveInput) -> Result<Vec<proc_macro2::TokenStream>> {
    let fields = get_struct_fields(st)?;

    let res = fields
        .iter()
        .map(|field| {
            let field_name = &field.ident;
            if get_builder_each_attribute(field).unwrap().is_some() {
                quote! {
                    #field_name: std::vec::Vec::new()
                }
            } else {
                quote! {
                    #field_name: std::option::Option::None
                }
            }
        })
        .collect();

    // 与上面实现结果一样
    // let mut res=vec![];
    // for field in fields {
    //     let field_name = &field.ident;
    //     res.push(quote! {
    //         #field_name: std::option::Option::None
    //     });
    // }
    Ok(res)
}

/// 为Builder实现setter方法
fn generate_builder_setter_function(st: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let fields = get_struct_fields(st)?;
    // let new_indent=vec![];
    // let idents: Vec<_> = fields
    //     .iter()
    //     .map(|field| {
    //         // match get_builder_name_values(&field) {
    //         //     Some(each)=>{},
    //         //     None=>
    //         // }
    //         &field.ident
    //     })
    //     .collect();
    // 第6关 如果类型是option包裹的，则要提取出来
    // let types: Vec<_> = fields
    //     .iter()
    //     .map(|field| get_optional_inner_type(&field.ty, "Option").unwrap_or(&field.ty))
    //     .collect();

    // let mut res = proc_macro2::TokenStream::new();
    // for (ident, type_) in idents.iter().zip(types.iter()) {
    //     res.extend(quote! {
    //         fn #ident(&mut self, #ident: #type_) -> &mut Self {
    //             self.#ident = std::option::Option::Some(#ident);
    //             self
    //         }
    //     });
    // }
    // Ok(res)

    // 第7关重构（如果是each修饰的要生成一个或两个方法，并且each修饰的要获取内部方法）
    let mut res = proc_macro2::TokenStream::new();
    for field in fields {
        if let Some(ref each) = get_builder_each_attribute(&field)? {
            let field_name = &field.ident;
            if each != field.ident.as_ref().unwrap() {
                let type_ = &field.ty;
                res.extend(quote! {
                    fn #field_name(&mut self, #field_name: #type_) -> &mut Self {
                        self.#field_name=#field_name;
                        self
                    }
                });
            }
            let type_ = get_optional_inner_type(&field.ty, "Vec").ok_or(syn::Error::new(
                field.span(),
                "`each` field must be a Vec Type",
            ))?;
            res.extend(quote! {
                fn #each(&mut self, #each: #type_) -> &mut Self {
                    self.#field_name.push(#each);
                    self
                }
            });
        } else {
            let field_name = &field.ident;
            let type_ = get_optional_inner_type(&field.ty, "Option").unwrap_or(&field.ty);
            res.extend(quote! {
                fn #field_name(&mut self, #field_name: #type_) -> &mut Self {
                    self.#field_name=Some(#field_name);
                    self
                }
            });
        }
    }
    Ok(res)
}

/// 实现builder中的build方法内的内容
fn generate_builder_build(st: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let fields = get_struct_fields(st)?;
    // 第6关 如果本来就是Option字段则不进行校验
    let ident: Vec<_> = fields
        .iter()
        .filter(|field| {
            get_optional_inner_type(&field.ty, "Option").is_none()
                && get_optional_inner_type(&field.ty, "Vec").is_none()
        })
        .map(|field| &field.ident)
        .collect();
    let verify_fields = quote! {
        #(if self.#ident.is_none(){
            let msg=format!("{} is empty",stringify!(#ident));
            return std::result::Result::Err(msg.into());
        })*
    };
    Ok(verify_fields)
}

/// 实现build方法返回的结构体的内部内容
fn generate_builder_build_return(st: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let fields = get_struct_fields(st)?;
    let mut res = proc_macro2::TokenStream::new();
    for field in fields {
        let field_name = &field.ident;
        if get_optional_inner_type(&field.ty, "Option").is_some() {
            res.extend(quote! {
                #field_name: self.#field_name.clone(),
            });
            // 第7关 如果是用builder(each=“”)属性修饰的则视为Vec不用包Option
        } else if get_builder_each_attribute(&field)?.is_some() {
            res.extend(quote! {
                #field_name: self.#field_name.clone(),
            });
        } else {
            res.extend(quote! {
                #field_name: self.#field_name.clone().unwrap(),
            });
        }
    }
    Ok(res)
}

/// 获取结构体中的Option类型的参数 (返回的类型不一定有，所以用Option包一层，用Result来处理的话不太合适，显得太严格了)
fn get_optional_inner_type<'a>(t: &'a syn::Type, outer_ident_name: &str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = &t
    {
        // 这里segment是一个列表，我们只需要识别最后一个是不是Option就可以了
        // 遇到列表就得另起一段进行匹配，因为列表无法进行匹配
        if let Some(syn::PathSegment { ident, arguments }) = segments.last() {
            if ident == outer_ident_name {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    args,
                    ..
                }) = &arguments
                {
                    if let Some(syn::GenericArgument::Type(inner_type)) = args.first() {
                        return Some(inner_type);
                    }
                }
            }
        }
    }
    None
}

/// 第7关 获取用户属性
fn get_builder_each_attribute(field: &syn::Field) -> Result<Option<syn::Ident>> {
    for attr in &field.attrs {
        if let syn::Meta::List(syn::MetaList { path, tokens, .. }) = &attr.meta {
            if path.is_ident("builder") {
                let token = tokens.clone();
                let name_value = syn::parse2::<syn::MetaNameValue>(token)?;
                if name_value.path.is_ident("each") {
                    if let syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Str(v),
                        ..
                    }) = name_value.value
                    {
                        return Ok(Some(syn::Ident::new(&v.value(), field.span())));
                    }
                } else {
                    // 第8关 返回错误提示
                    return Err(syn::Error::new_spanned(
                        &attr.meta,
                        r#"expected `builder(each = "...")`"#,
                    ));
                }
            }
        }
    }
    Ok(None)
}
