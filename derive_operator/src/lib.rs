extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::Error, parse_macro_input, spanned::Spanned, Data, DeriveInput, Ident, Lit, LitStr, Meta,
    Variant,
};

struct OperatorDef {
    field: Ident,
    ident: LitStr,
    assoc: Assoc,
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
            if let Err(e) = meta {
                return Err(e);
            }

            match attr.parse_meta()? {
                Meta::Path(path) => {
                    return Err(Error::new(
                        path.span(),
                        "expected name-value pair attribute 'ident = 'x''",
                    ))
                }
                Meta::List(list) => {
                    return Err(Error::new(
                        list.span(),
                        "expected name-value pair attribute 'ident = 'x''",
                    ))
                }
                Meta::NameValue(attr) => {
                    if attr.path.is_ident("ident") {
                        if let Lit::Str(litstr) = attr.lit {
                            ident = Some(litstr)
                        } else {
                            return Err(Error::new(attr.lit.span(), "expected string literal"));
                        }
                    } else if attr.path.is_ident("assoc") {
                        if let Lit::Str(litstr) = attr.lit {
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
                                attr.lit.span(),
                                "expected string literal 'left' or 'right'",
                            ));
                        }
                    }
                }
            }
        }

        let ident = match ident {
            Some(i) => i,
            None => {
                return Err(Error::new(
                    var.span(),
                    "expected name-value pair 'ident = 'x''",
                ))
            }
        };
        let assoc = match assoc {
            Some(a) => a,
            None => Assoc::Left,
        };

        Ok(Self {
            field: var.ident,
            ident,
            assoc,
        })
    }
}

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

    let prec = defs.iter().enumerate().map(|(i, def)| {
        let field = &def.field;
        let base = 2 * i + 1;
        let mut lhs = base;
        let mut rhs = base;
        match def.assoc {
            Assoc::Left => rhs += 1,
            Assoc::Right => lhs += 1,
        }
        quote!(Self::#field => (#lhs, #rhs))
    });

    let toks = quote! {
        impl parser::Operator for #ident {
            fn parse(inp: &str) -> Option<(&str, Self, usize)> {
                let matches = [#(#name_var),*];

                matches
                    .iter()
                    .find_map(|(op, var)| inp.strip_prefix(op).map(|out| (out, *var, op.len())))
            }
            fn precedence(&self) -> (usize, usize) {
                match self {
                    #(#prec),*
                }
            }
        }
    };
    Ok(TokenStream::from(toks))
}

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
