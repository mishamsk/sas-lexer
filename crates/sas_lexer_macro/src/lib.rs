extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, DeriveInput, Expr, ExprLit, Ident, Lit, LitInt,
    LitStr, Meta, MetaNameValue, Token,
};

/// Macro to generate a keyword map for an enum. Takes all variants that start with "Kw"
/// but not "Kwm" and generates a map from the uppercase keyword text to the variant.
///
/// # Panics
/// Panics if the input is not an enum.
#[proc_macro_derive(KeywordMap, attributes(keyword, kw_map_name))]
pub fn generate_keyword_map(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let variants = if let syn::Data::Enum(data_enum) = input.data {
        data_enum.variants
    } else {
        panic!("generate_keyword_map can only be used on enums");
    };

    // Get the name of the enum
    let name = &input.ident;

    // Get the name of the map
    let kw_map_name = input
        .attrs
        .iter()
        .find_map(|attr| {
            if attr.path().is_ident("kw_map_name") {
                if let Meta::NameValue(MetaNameValue {
                    value:
                        Expr::Lit(ExprLit {
                            lit: Lit::Str(litstr),
                            ..
                        }),
                    ..
                }) = &attr.meta
                {
                    Some(Ident::new(litstr.value().as_str(), Span::call_site()))
                } else {
                    panic!("kw_map_name attribute must be a string literal");
                }
            } else {
                None
            }
        })
        .unwrap_or(Ident::new("KEYWORDS", Span::call_site()));

    let keywords_and_variants = variants
        .iter()
        .filter(|variant| {
            variant.ident.to_string().starts_with("Kw")
                && !variant.ident.to_string().starts_with("Kwm")
        })
        .flat_map(|variant| {
            let variant_ident = &variant.ident;
            let ident = variant.ident.to_string();

            let keywords = variant.attrs.iter().find_map(|attr| {
                if attr.path().is_ident("keyword") {
                    attr.parse_args_with(Punctuated::<LitStr, Token![,]>::parse_terminated)
                        .ok()
                } else {
                    None
                }
            });

            #[allow(clippy::indexing_slicing)]
            let default_keyword = &ident[2..].to_ascii_uppercase();

            keywords
                .map_or_else(
                    || vec![default_keyword.clone()],
                    |kws| {
                        kws.into_iter()
                            .map(|kw| kw.value().to_ascii_uppercase())
                            .collect()
                    },
                )
                .into_iter()
                .map(move |kw: String| (kw, variant_ident))
        });

    let max_keyword_len = keywords_and_variants
        .clone()
        .fold(0, |acc, (kw, _)| acc.max(kw.len()));

    let variant_code = keywords_and_variants.map(move |(keyword, variant_ident)| {
        quote! {
            #keyword => #name::#variant_ident,
        }
    });

    assert!(
        variant_code.clone().count() != 0,
        "No variants found that start with 'Kw'"
    );

    let const_name = format!("MAX_{kw_map_name}_LEN");
    let const_ident = Ident::new(&const_name, kw_map_name.span());

    let expanded = quote! {
        pub(crate) const #const_ident: usize = #max_keyword_len;

        pub(crate) static #kw_map_name: phf::Map<&'static str, #name> = phf_map! {
            #(#variant_code)*
        };
    };

    TokenStream::from(expanded)
}

/// Macro to generate a SAS macro keyword map for an enum. Takes all variants that start with "Kwm"
/// and generates a map from the uppercase keyword (without the leading %) text to the variant.
///
/// # Panics
/// Panics if the input is not an enum.
#[proc_macro_derive(MacroKeywordMap, attributes(keyword, kwm_map_name))]
pub fn generate_macro_keyword_map(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let variants = if let syn::Data::Enum(data_enum) = input.data {
        data_enum.variants
    } else {
        panic!("generate_macro_keyword_map can only be used on enums");
    };

    // Get the name of the enum
    let name = &input.ident;

    // Get the name of the map
    let kwm_map_name = input
        .attrs
        .iter()
        .find_map(|attr| {
            if attr.path().is_ident("kwm_map_name") {
                if let Meta::NameValue(MetaNameValue {
                    value:
                        Expr::Lit(ExprLit {
                            lit: Lit::Str(litstr),
                            ..
                        }),
                    ..
                }) = &attr.meta
                {
                    Some(Ident::new(litstr.value().as_str(), Span::call_site()))
                } else {
                    panic!("kwm_map_name attribute must be a string literal");
                }
            } else {
                None
            }
        })
        .unwrap_or(Ident::new("MACRO_KEYWORDS", Span::call_site()));

    let keywords_and_variants = variants
        .iter()
        .filter(|variant| variant.ident.to_string().starts_with("Kwm"))
        .flat_map(|variant| {
            let variant_ident = &variant.ident;
            let ident = variant.ident.to_string();

            let keywords = variant.attrs.iter().find_map(|attr| {
                if attr.path().is_ident("keyword") {
                    attr.parse_args_with(Punctuated::<LitStr, Token![,]>::parse_terminated)
                        .ok()
                } else {
                    None
                }
            });

            #[allow(clippy::indexing_slicing)]
            let default_keyword = &ident[3..].to_ascii_uppercase();

            keywords
                .map_or_else(
                    || vec![default_keyword.clone()],
                    |kws| {
                        kws.into_iter()
                            .map(|kw| kw.value().to_ascii_uppercase())
                            .collect()
                    },
                )
                .into_iter()
                .map(move |kw: String| (kw, variant_ident))
        });

    let max_keyword_len = keywords_and_variants
        .clone()
        .fold(0, |acc, (kw, _)| acc.max(kw.len()));

    let variant_code = keywords_and_variants.map(move |(keyword, variant_ident)| {
        quote! {
            #keyword => #name::#variant_ident,
        }
    });

    assert!(
        variant_code.clone().count() != 0,
        "No variants found that start with 'Kwm'"
    );

    let const_name = format!("MAX_{kwm_map_name}_LEN");
    let const_ident = Ident::new(&const_name, kwm_map_name.span());

    let expanded = quote! {
        pub(crate) const #const_ident: usize = #max_keyword_len;

        pub(crate) static #kwm_map_name: phf::Map<&'static str, #name> = phf_map! {
            #(#variant_code)*
        };
    };

    TokenStream::from(expanded)
}

/// Custom derive macro to generate a subset of an enum.
/// The subset enum is generated with the same variants as the original enum
/// but only the ones between the start and end variants (inclusive).
///
/// The subset enum is generated with the following impls:
/// - Debug, Clone, Copy, PartialEq, Eq, EnumIter
///
/// Also, the following impls are generated:
/// - From<SubsetEnum> for FullEnum
/// - TryFrom<FullEnum> for SubsetEnum
///
/// Also currently assumes `repr(u16)`.
///
/// # Panics
/// Panics if the input is not an enum and other conditions are not met.
#[allow(clippy::doc_markdown)]
#[proc_macro_derive(TokenTypeSubset, attributes(subset))]
pub fn derive_token_type_subset(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    // Get the name of the enum
    let full_enum_name = &input.ident;

    // Get the variants of the enum
    let variants = if let syn::Data::Enum(data_enum) = input.data {
        data_enum.variants
    } else {
        panic!("TokenTypeSubset can only be used on enums");
    };

    // Find the name of the subset enum, and start & end variants of the subset
    // subset attr should be defined on the enum and has the following syntax:
    // #[subset(name = "SubsetEnumName", start = "StartVariant", end = "EndVariant")]
    let mut subset_enum_name = None;
    let mut start_variant = None;
    let mut end_variant = None;

    for attr in &input.attrs {
        if attr.path().is_ident("subset") {
            if let Meta::List(meta_list) = &attr.meta {
                for nested in meta_list
                    .parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)
                    .expect("Expected comma separated attributes: name, start, end")
                {
                    if let Meta::NameValue(MetaNameValue {
                        path: opt,
                        value: Expr::Path(syn::ExprPath { path: value, .. }),
                        ..
                    }) = nested
                    {
                        let ident = value
                            .get_ident()
                            .expect("Expected identifier as the value of name, start, end")
                            .clone();

                        if opt.is_ident("name") {
                            subset_enum_name = Some(ident);
                        } else if opt.is_ident("start") {
                            start_variant = Some(ident.to_string());
                        } else if opt.is_ident("end") {
                            end_variant = Some(ident.to_string());
                        }
                    }
                }
            }
        }
    }

    let Some(subset_enum_name) = subset_enum_name else {
        panic!("Missing subset name");
    };

    let Some(start_variant) = start_variant else {
        panic!("Missing start variant");
    };

    let Some(end_variant) = end_variant else {
        panic!("Missing end variant");
    };

    let mut in_range = false;
    let mut subset_variants = Vec::new();
    let mut subset_variant_int_values = Vec::new();

    for (i, variant) in variants.iter().enumerate() {
        let variant_name = variant.ident.to_string();
        let var_ident = variant.ident.clone();

        if variant_name == start_variant {
            in_range = true;
        }
        if in_range {
            let var_int_val = Lit::Int(LitInt::new(i.to_string().as_str(), var_ident.span()));
            subset_variants.push(var_ident);
            subset_variant_int_values.push(var_int_val);
        }
        if variant_name == end_variant {
            break;
        }
    }

    let expanded = quote! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter)]
        #[repr(u16)]
        pub enum #subset_enum_name {
            #(#subset_variants = #subset_variant_int_values),*
        }

        impl From<#subset_enum_name> for #full_enum_name {
            fn from(subset: #subset_enum_name) -> Self {
                match subset {
                    #(#subset_enum_name::#subset_variants => #full_enum_name::#subset_variants),*
                }
            }
        }

        impl std::convert::TryFrom<#full_enum_name> for #subset_enum_name {
            type Error = ();

            fn try_from(value: #full_enum_name) -> Result<Self, Self::Error> {
                match value {
                    #(#full_enum_name::#subset_variants => Ok(#subset_enum_name::#subset_variants),)*
                    _ => Err(()),
                }
            }
        }
    };

    TokenStream::from(expanded)
}
