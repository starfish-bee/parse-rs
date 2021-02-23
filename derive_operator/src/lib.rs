extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::Error, parse_macro_input, spanned::Spanned, Data, DeriveInput, Ident, Lit, LitStr, Meta,
    NestedMeta, Variant,
};

#[proc_macro_derive(Operator, attributes(ident, assoc))]
pub fn derive_operator(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);
    match impl_derive_operator(input) {
        Ok(stream) => stream,
        Err(e) => TokenStream::from(e.into_compile_error()),
    }
}

fn impl_derive_operator(input: DeriveInput) -> Result<TokenStream, Error> {
    let (ident, variants) = get_enum(input)?;

    let defs: Vec<_> = match variants
        .into_iter()
        .map(|var| OperatorDef::from_variant(var))
        .collect()
    {
        Ok(defs) => defs,
        Err(e) => return Err(e),
    };

    let name_var = defs.iter().map(|def| {
        let ident = &def.ident;
        let field = &def.field;
        quote!((#ident, Self::#field))
    });

    // the string representation is the same as the str used to parse the operator
    let string_repr = defs.iter().map(|def| {
        let ident = &def.ident;
        let field = &def.field;
        quote!(Self::#field => #ident.to_string())
    });

    // precedence starts at 1, is always odd, and increases with field order
    // associtivity is indicated by whether the left of right value has +1
    let prec = defs.iter().enumerate().filter_map(|(i, def)| {
        let assoc = match &def.infix {
            Some(a) => a,
            None => return None,
        };
        let field = &def.field;
        let base = 2 * i + 1;
        let mut lhs = base;
        let mut rhs = base;
        match assoc {
            Assoc::Left => rhs += 1,
            Assoc::Right => lhs += 1,
        }
        Some(quote!(Self::#field => Some((#lhs, #rhs))))
    });

    let toks = quote! {
        impl parser::Operator for #ident {
            fn parse(inp: &str) -> Option<(&str, Self)> {
                let matches = [#(#name_var),*];

                matches
                    .iter()
                    .find_map(|(op, var)| inp.strip_prefix(op).map(|out| (out, *var)))
            }
            fn infix_precedence(&self) -> Option<(usize, usize)> {
                match self {
                    #(#prec,)*
                    _ => None,
                }
            }
            fn to_string(&self) -> String {
                match self {
                    #(#string_repr),*
                }
            }
        }
    };
    Ok(TokenStream::from(toks))
}

// macro is only valid for enums
fn get_enum(input: DeriveInput) -> Result<(Ident, Vec<Variant>), Error> {
    let ident = input.ident;
    match input.data {
        Data::Enum(en) => Ok((ident, en.variants.into_iter().collect())),
        Data::Struct(st) => Err(Error::new(
            st.struct_token.span,
            "'Operator' derive macro is only valid for 'enum' types",
        )),
        Data::Union(un) => Err(Error::new(
            un.union_token.span,
            "'Operator' derive macro is only valid for 'enum' types",
        )),
    }
}

// for each field, extract the field name, ident to be used for parsing, and operator associativity
struct OperatorDef {
    field: Ident,
    ident: LitStr,
    infix: Option<Assoc>,
    _prefix: bool,
    _postfix: bool,
}

enum Assoc {
    Left,
    Right,
}

impl OperatorDef {
    fn from_variant(var: Variant) -> Result<Self, Error> {
        let mut ident: Option<LitStr> = None;
        let mut assoc: Option<Assoc> = None;
        for attr in var.attrs.iter() {
            let meta = attr.parse_meta();
            // using if let pattern to reduce nesting
            if let Err(e) = meta {
                return Err(e);
            }

            // a list attribute is currently the only type supported
            match attr.parse_meta()? {
                Meta::Path(path) => {
                    return Err(Error::new(
                        path.span(),
                        "expected list attribute 'ident('x')'",
                    ))
                }
                Meta::NameValue(attr) => {
                    return Err(Error::new(
                        attr.span(),
                        "expected list attribute 'ident('x')'",
                    ))
                }
                Meta::List(list) => {
                    if list.path.is_ident("ident") {
                        if let Some(NestedMeta::Lit(Lit::Str(litstr))) = list.nested.first() {
                            ident = Some(litstr.clone())
                        } else {
                            return Err(Error::new(list.nested.span(), "expected string literal"));
                        }
                    } else if list.path.is_ident("assoc") {
                        if let Some(NestedMeta::Lit(Lit::Str(litstr))) = list.nested.first() {
                            match &litstr.value()[..] {
                                "left" => assoc = Some(Assoc::Left),
                                "right" => assoc = Some(Assoc::Right),
                                _ => {
                                    return Err(Error::new(
                                        litstr.span(),
                                        "expected 'left' or 'right'",
                                    ))
                                }
                            }
                        } else {
                            return Err(Error::new(
                                list.nested.span(),
                                "expected string literal 'left' or 'right'",
                            ));
                        }
                    } else {
                        return Err(Error::new(list.path.span(), "unrecognised attribute"));
                    }
                }
            }
        }

        // ident value not being set here imples no ident attribute was present
        let ident = match ident {
            Some(i) => i,
            None => {
                return Err(Error::new(
                    var.span(),
                    "expected list attribute 'ident('x')'",
                ))
            }
        };
        // assoc attribute is optional- default behaviour is left-associativity
        let assoc = match assoc {
            Some(a) => a,
            None => Assoc::Left,
        };

        Ok(Self {
            field: var.ident,
            ident,
            infix: Some(assoc),
            _prefix: false,
            _postfix: false,
        })
    }
}
