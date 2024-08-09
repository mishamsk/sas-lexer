extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, Data, DeriveInput, Expr, ExprLit, Ident, Lit,
    LitStr, Meta, MetaNameValue, Token,
};

/// # Panics
/// Panics if the input is not an enum.
#[proc_macro_derive(ToU16)]
pub fn to_u16_conversions_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let variants = if let Data::Enum(data_enum) = &input.data {
        data_enum.variants.iter().collect::<Vec<_>>()
    } else {
        panic!("ToU16 can only be derived for enums");
    };

    let from_type_for_u16 = variants.iter().enumerate().map(|(i, variant)| {
        let variant_ident = &variant.ident;
        quote! {
            #name::#variant_ident => #i as u16,
        }
    });

    let expanded = quote! {
        impl From<#name> for u16 {
            fn from(token_type: #name) -> Self {
                match token_type {
                    #(#from_type_for_u16)*
                }
            }
        }
    };

    TokenStream::from(expanded)
}

/// # Panics
/// Panics if the input is not an enum.
#[proc_macro_derive(FromU16)]
pub fn from_u16_conversions_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let variants = if let Data::Enum(data_enum) = &input.data {
        data_enum.variants.iter().collect::<Vec<_>>()
    } else {
        panic!("FromU16 can only be derived for enums");
    };

    let from_u16_for_type = variants.iter().enumerate().map(|(i, variant)| {
        let variant_ident = &variant.ident;
        let i = u16::try_from(i).unwrap_or_else(|_| {
            panic!(
                "This macro doesn't support more than {} variants!",
                u16::MAX
            );
        });
        quote! {
            #i => Some(#name::#variant_ident),
        }
    });

    let expanded = quote! {
        impl #name {
            fn from_u16(value: u16) -> Option<Self> {
                match value {
                    #(#from_u16_for_type)*
                    _ => None,
                }
            }
        }
    };

    TokenStream::from(expanded)
}

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

    let variant_code = variants
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
            let keywords = keywords.map_or_else(
                || vec![default_keyword.clone()],
                |kws| {
                    kws.into_iter()
                        .map(|kw| kw.value().to_ascii_uppercase())
                        .collect()
                },
            );

            keywords.into_iter().map(move |keyword| {
                quote! {
                    #keyword => #name::#variant_ident,
                }
            })
        });

    assert!(variant_code.clone().count() != 0, "No variants found that start with 'Kw'");

    let expanded = quote! {
        pub(crate) static #kw_map_name: phf::Map<&'static str, #name> = phf_map! {
            #(#variant_code)*
        };
    };

    TokenStream::from(expanded)
}

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

    let variant_code = variants
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
            let keywords = keywords.map_or_else(
                || vec![default_keyword.clone()],
                |kws| {
                    kws.into_iter()
                        .map(|kw| kw.value().to_ascii_uppercase())
                        .collect()
                },
            );

            keywords.into_iter().map(move |keyword| {
                quote! {
                    #keyword => #name::#variant_ident,
                }
            })
        });

    assert!(
        variant_code.clone().count() != 0,
        "No variants found that start with 'Kwm'"
    );

    let expanded = quote! {
        pub(crate) static #kwm_map_name: phf::Map<&'static str, #name> = phf_map! {
            #(#variant_code)*
        };
    };

    TokenStream::from(expanded)
}
