#![allow(unused)]

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let st = parse_macro_input!(input as SeqParser);
    // 第5关使用tokenBuffer处理
    let buf = syn::buffer::TokenBuffer::new2(st.body.clone());

    let (res, found) = st.find_block_to_expand_and_do_expand(buf.begin());
    if (found) {
        return res.into();
    }

    // 如果未匹配到回退到之前的方法
    let mut res = proc_macro2::TokenStream::new();
    for i in st.start..st.end {
        res.extend(st.do_expand(&st.body, i));
    }
    res.into()
}

#[derive(Debug)]
struct SeqParser {
    variable_ident: syn::Ident,
    start: isize,
    end: isize,
    body: proc_macro2::TokenStream,
}

impl syn::parse::Parse for SeqParser {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let variable_ident = input.parse::<syn::Ident>()?;
        input.parse::<syn::Token![in]>()?;
        let start = input.parse::<syn::LitInt>()?;
        input.parse::<syn::Token![..]>()?;

        // 第7关使用peek预看
        let mut inc = false;
        if input.peek(syn::Token![=]) {
            input.parse::<syn::Token![=]>()?;
            inc = true;
        }
        let end = input.parse::<syn::LitInt>()?;
        let body_buf;
        
        syn::braced!(body_buf in input);
        let mut res = Self {
            variable_ident,
            start: start.base10_parse()?,
            end: end.base10_parse()?,
            body: body_buf.parse()?,
        };
        // 第7关
        if inc {
            res.end += 1;
        }
        Ok(res)
    }
}

impl SeqParser {
    // 第三关使用TokenTree处理
    fn do_expand(&self, st: &proc_macro2::TokenStream, n: isize) -> proc_macro2::TokenStream {
        let buf = st.clone().into_iter().collect::<Vec<_>>();
        let mut res = proc_macro2::TokenStream::new();
        let mut i = 0;
        while i < buf.len() {
            let tree_node = &buf[i];

            match tree_node {
                proc_macro2::TokenTree::Group(group) => {
                    let new_stream = self.do_expand(&group.stream(), n);
                    let wrap_in_group = proc_macro2::Group::new(group.delimiter(), new_stream);
                    res.extend(quote! {#wrap_in_group});
                }
                proc_macro2::TokenTree::Ident(prefix) => {
                    if i + 2 < buf.len() {
                        //向后预读两个元素
                        if let proc_macro2::TokenTree::Punct(punct) = &buf[i + 1] {
                            if punct.as_char() == '~' {
                                if let proc_macro2::TokenTree::Ident(suffix) = &buf[i + 2] {
                                    // 判断位置是否连续，且标识符是否是用户的标识符
                                    if suffix == &self.variable_ident
                                        && prefix.span().end() == punct.span().start()
                                        && punct.span().end() == suffix.span().start()
                                    {
                                        let new_ident_literal = format!("{}{}", prefix, n);
                                        let new_ident = proc_macro2::Ident::new(
                                            &new_ident_literal,
                                            prefix.span(),
                                        );
                                        res.extend(quote! {#new_ident});
                                        i += 3; //这里消耗了三个Token
                                        continue;
                                    }
                                }
                            }
                        }
                    }

                    if prefix == &self.variable_ident {
                        let new_ident = proc_macro2::Literal::i64_unsuffixed(n as i64);
                        res.extend(quote! {#new_ident});
                        i += 1;
                        continue;
                    }
                    res.extend(quote! {#tree_node});
                }
                _ => res.extend(quote! {#tree_node}),
            }
            i += 1;
        }
        res
    }

    fn find_block_to_expand_and_do_expand(
        &self,
        c: syn::buffer::Cursor,
    ) -> (proc_macro2::TokenStream, bool) {
        let mut res = proc_macro2::TokenStream::new();
        let mut found = false;
        let mut cursor = c;
        while !cursor.eof() {
            // 匹配 #(...)*
            if let Some((punct_prefix, cursor_1)) = cursor.punct() {
                if punct_prefix.as_char() == '#' {
                    if let Some((group_cursor, _, cursor_2)) =
                        cursor_1.group(proc_macro2::Delimiter::Parenthesis)
                    {
                        if let Some((punct_suffix, cursor_3)) = cursor_2.punct() {
                            if punct_suffix.as_char() == '*' {
                                // 走到这里说明匹配成功
                                // 然后展开内部代码
                                for i in self.start..self.end {
                                    res.extend(self.do_expand(&group_cursor.token_stream(), i));
                                }
                                cursor = cursor_3;
                                found = true;
                                continue;
                            }
                        }
                    }
                }
            }

            // 没有匹配上需要手动处理
            if let Some((group_cursor, _, next_cursor)) =
                cursor.group(proc_macro2::Delimiter::Brace)
            {
                let (t, f) = self.find_block_to_expand_and_do_expand(group_cursor);
                found = f;
                res.extend(quote!({#t}));
                cursor = next_cursor;
                continue;
            } else if let Some((group_cursor, _, next_cursor)) =
                cursor.group(proc_macro2::Delimiter::Bracket)
            {
                let (t, f) = self.find_block_to_expand_and_do_expand(group_cursor);
                found = f;
                res.extend(quote!([#t]));
                cursor = next_cursor;
                continue;
            } else if let Some((group_cursor, _, next_cursor)) =
                cursor.group(proc_macro2::Delimiter::Parenthesis)
            {
                let (t, f) = self.find_block_to_expand_and_do_expand(group_cursor);
                found = f;
                res.extend(quote!((#t)));
                cursor = next_cursor;
                continue;
            } else if let Some((token, next_cursor)) = cursor.ident() {
                res.extend(quote!(#token));
                cursor = next_cursor;
                continue;
            } else if let Some((token, next_cursor)) = cursor.punct() {
                res.extend(quote!(#token));
                cursor = next_cursor;
                continue;
            } else if let Some((token, next_cursor)) = cursor.literal() {
                res.extend(quote!(#token));
                cursor = next_cursor;
                continue;
            } else if let Some((token, next_cursor)) = cursor.lifetime() {
                res.extend(quote!(#token));
                cursor = next_cursor;
                continue;
            }
        }
        (res, found)
    }
}
