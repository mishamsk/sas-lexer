extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput};

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
        let i = i as u16;
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
