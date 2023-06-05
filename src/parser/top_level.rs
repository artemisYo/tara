use super::{block::Block, Error, PRes};
use crate::expect;
use crate::tokenizer::{Token, Tokenstack};

#[derive(Debug)]
enum TopLevel {
    Func {
        path: FuncIdent,
        generics: Vec<GenericArg>,
        params: Vec<(Token, Type)>,
        rety: Option<Box<Type>>,
        body: Box<Block>,
    },
    Proto {
        generics: Vec<GenericArg>,
        fields: Vec<(Token, Type)>,
        name: Token,
        implementor: Box<GenericArg>,
    },
    Impl {
        generics: Vec<GenericArg>,
        fields: Vec<(Token, FuncIdent)>,
        protocol: Token,
        implementor: Box<Type>,
    },
    TypeDef {
        generics: Vec<GenericArg>,
        name: Token,
        body: Box<TypeBody>,
    },
}
impl TopLevel {
    fn parse(mut input: Tokenstack) -> PRes<Self> {
        match input.peek() {
            Token::FnKey => parse_fn(input),
            Token::ProtoKey => parse_proto(input),
            Token::ImplKey => parse_impl(input),
            Token::TypeKey => parse_typedef(input),
            _ => Err(Error::Multiple {
                expected: vec![
                    Token::FnKey,
                    Token::ProtoKey,
                    Token::ImplKey,
                    Token::TypeKey,
                ],
                found: input.pop(),
                origin: input,
            }),
        }
    }
}
struct FuncIdent(Vec<Token>);
impl FuncIdent {
    fn parse(mut input: Tokenstack) -> PRes<Self> {
        todo!()
    }
}
// #[derive(Debug)]
// pub struct Program(Vec<Box<dyn TopLevel>>);
// impl Program {
//     pub fn parse(mut input: Tokenstack) -> PRes<Self> {
//         let mut acc = vec![];
//         loop {
//             if input.is_empty() {
//                 break;
//             }
//             match parse_top_level(input) {
//                 Ok((s, a)) => {
//                     input = s;
//                     acc.push(a);
//                 }
//                 Err(e) => {
//                     println!("{:?}", e);
//                     break;
//                 }
//             }
//         }
//         Ok((input, Self(acc)))
//     }
// }
fn parse_fn(mut input: Tokenstack) -> PRes<TopLevel> {
    let mut params = vec![];
    // "fn"
    expect!(input, Token::FnKey);
    // FuncIdent
    let (s, path) = FuncIdent::parse(input)?;
    input = s;
    // GenParams?
    let (s, generics) = parse_genargs(input)?;
    input = s;
    // "("
    expect!(input, Token::OpenParen);
    // ((Ident ":" Type ",")* Ident ":" Type)?
    while let Token::Ident(_) = input.peek() {
        let n = input.pop();
        expect!(input, Token::Colon);
        let (s, t) = Type::parse(input)?;
        params.push((n, t));
        input = s;
        if input.peek() == &Token::Comma {
            input.pop();
        } else {
            break;
        }
    }
    // ")"
    expect!(input, Token::CloseParen);
    let rety = if input.peek() == &Token::Colon {
        input.pop();
        let (s, ty) = Type::parse(input)?;
        input = s;
        Some(Box::new(ty))
    } else {
        None
    };
    let (input, body) = Block::parse(input)?;
    Ok((
        input,
        TopLevel::Func {
            path,
            generics,
            params,
            rety,
            body,
        },
    ))
}

fn parse_proto(mut input: Tokenstack) -> PRes<TopLevel> {
    // "proto"
    expect!(input, Token::ProtoKey);
    // Ident
    let name = input.pop();
    if !name.is_ident() {
        return Err(Error::Expected {
            expected: Token::Ident("".to_string()),
            found: name,
            origin: input,
        });
    }
    // GenParams?
    let (s, generics) = parse_genargs(input)?;
    input = s;
    // "on"
    expect!(input, Token::OnKey);
    // GenArg
    let (mut input, implementor) = GenericArg::parse(input)?;
    let implementor = Box::new(implementor);
    // "->"
    expect!(input, Token::Arrow);
    // ((Ident ":" Type ",")* (Ident ":" Type))?
    let mut fields = vec![];
    while input.peek().is_ident() {
        let n = input.pop();
        expect!(input, Token::Colon);
        let (s, ty) = Type::parse(input)?;
        fields.push((n, ty));
        input = s;
        match input.peek() {
            Token::Comma => {
                input.pop();
            }
            _ => break,
        }
    }
    // ";"
    expect!(input, Token::SemiCol);
    Ok((
        input,
        TopLevel::Proto {
            generics,
            fields,
            name,
            implementor,
        },
    ))
}

fn parse_impl(mut input: Tokenstack) -> PRes<TopLevel> {
    // "impl"
    expect!(input, Token::ImplKey);
    // Ident
    let protocol = input.pop();
    if !protocol.is_ident() {
        return Err(Error::Expected {
            expected: Token::Ident("".to_string()),
            found: protocol,
            origin: input,
        });
    }
    // GenParams?
    let (s, generics) = parse_genargs(input)?;
    input = s;
    // "on"
    expect!(input, Token::OnKey);
    // Type
    let (mut input, implementor) = Type::parse(input)?;
    let implementor = Box::new(implementor);
    // "->"
    expect!(input, Token::Arrow);
    // (Ident "=" (Ident ".")* Ident ",")* ~
    // (Ident "=" (Ident ".")* Ident)?
    let mut fields = vec![];
    while input.peek().is_ident() {
        let n = input.pop();
        expect!(input, Token::Equals);
        let (s, path) = FuncIdent::parse(input)?;
        input = s;
        fields.push((n, path));
        match input.peek() {
            Token::Comma => {
                input.pop();
            }
            _ => break,
        }
    }
    // ";"
    expect!(input, Token::SemiCol);
    Ok((
        input,
        TopLevel::Impl {
            generics,
            fields,
            protocol,
            implementor,
        },
    ))
}

fn parse_typedef(mut input: Tokenstack) -> PRes<TopLevel> {
    // "type"
    expect!(input, Token::TypeKey);
    // Ident
    let name = match input.pop() {
        Token::Ident(n) => Token::Ident(n),
        _ => todo!(),
    };
    // GenParams?
    let (s, generics) = parse_genargs(input)?;
    input = s;
    // "="
    expect!(input, Token::Equals);
    // TypeBody
    let body = Box::new(match TypeBody::parse(input) {
        Ok((s, a)) => {
            input = s;
            Ok(a)
        }
        e => {
            println!("{:?}", e);
            todo!()
        }
    });
    Ok((
        input,
        TopLevel::TypeDef {
            generics,
            name,
            body,
        },
    ))
}

#[derive(Debug)]
enum TypeBody {
    // (name : Type ,)* name : Type
    Struct(Vec<(Token, Type)>),
    // case name '(' ((Type ,)* Type) / ((name : Type ,)* name : Type) ')'
    Enum(Vec<(Token, Option<Box<EnumField>>)>),
    // '(' (Type ,)* Type ')'
    Tuple(Vec<Type>),
    // '(' ')'
    Singlet,
}
impl TypeBody {
    fn parse(mut input: Tokenstack) -> PRes<Self> {
        match input.peek() {
            Token::Ident(_) => parse_struct_def(input),
            Token::CaseKey => parse_enum_def(input),
            Token::OpenParen => {
                if input.lookahead(1) == &Token::CloseParen {
                    input.pop();
                    input.pop();
                    Ok((input, Self::Singlet))
                } else {
                    parse_tuple_def(input)
                }
            }
            _ => Err(Error::Multiple {
                expected: vec![
                    Token::Ident("".to_string()),
                    Token::CaseKey,
                    Token::OpenParen,
                ],
                found: input.pop(),
                origin: input,
            }),
        }
    }
}

fn parse_struct_def(mut input: Tokenstack) -> PRes<TypeBody> {
    let mut fields = vec![];
    while input.peek().is_ident() {
        // Ident
        let n = input.pop();
        // ":"
        expect!(input, Token::Colon);
        // Type
        let (s, ty) = Type::parse(input)?;
        input = s;
        fields.push((n, ty));
        match input.peek() {
            Token::Comma => {
                input.pop();
            }
            _ => break,
        }
    }
    if fields.is_empty() {
        return Err(Error::Expected {
            expected: Token::Ident("".to_string()),
            found: input.pop(),
            origin: input,
        });
    }
    // ";"
    expect!(input, Token::SemiCol);
    Ok((input, TypeBody::Struct(fields)))
}
fn parse_enum_def(mut input: Tokenstack) -> PRes<TypeBody> {
    let mut cases = vec![];
    while input.peek() == &Token::CaseKey {
        // case
        input.pop();
        // Ident
        let n = input.pop();
        if !n.is_ident() {
            return Err(Error::Expected {
                expected: Token::Ident("".to_string()),
                found: n,
                origin: input,
            });
        }
        // EnumField
        let d = match EnumField::parse(input) {
            Ok((s, a)) => {
                input = s;
                Some(Box::new(a))
            }
            Err(_e) => {
                //println!("{_e:?}");
                None
            }
        };
        cases.push((n, d));
        match input.peek() {
            Token::Comma => {
                input.pop();
            }
            _ => break,
        }
    }
    if cases.is_empty() {
        return Err(Error::Expected {
            expected: Token::CaseKey,
            found: input.pop(),
            origin: input,
        });
    }
    // ";"
    expect!(input, Token::SemiCol);
    Ok((input, TypeBody::Enum(cases)))
}

#[derive(Debug)]
pub struct EnumField {
    names: Option<Vec<Token>>,
    tys: Vec<Type>,
}
impl EnumField {
    fn parse(mut input: Tokenstack) -> PRes<Self> {
        // (
        expect!(input, Token::OpenParen);
        let mut tys = vec![];
        let names = if input.lookahead(1) == &Token::Colon {
            let mut names = vec![];
            while input.peek().is_ident() {
                // Ident
                let n = input.pop();
                names.push(n);
                // :
                expect!(input, Token::Colon);
                // Type
                let (s, ty) = Type::parse(input)?;
                tys.push(ty);
                input = s;
                match input.peek() {
                    Token::Comma => {
                        input.pop();
                    }
                    _ => break,
                }
            }
            Some(names)
        } else {
            // Type
            while let Ok((s, a)) = Type::parse(input) {
                input = s;
                tys.push(a);
                match input.peek() {
                    Token::Comma => {
                        input.pop();
                    }
                    _ => break,
                }
            }
            None
        };
        // )
        expect!(input, Token::CloseParen);
        Ok((input, Self { names, tys }))
    }
}
fn parse_tuple_def(mut input: Tokenstack) -> PRes<TypeBody> {
    // (
    expect!(input, Token::OpenParen);
    let mut acc = vec![];
    // Type
    while let Ok((s, a)) = Type::parse(input) {
        input = s;
        acc.push(a);
        match input.peek() {
            Token::Comma => {
                input.pop();
            }
            _ => break,
        }
    }
    // )
    expect!(input, Token::CloseParen);
    Ok((input, TypeBody::Tuple(acc)))
}

#[derive(Debug)]
pub enum Type {
    Plain {
        name: Token,
        params: Vec<Type>,
        array: Vec<Option<usize>>,
        refs: Vec<bool>,
    },
    FnType {
        args: Vec<Type>,
        ret_type: Option<Box<Type>>,
    },
}
impl Type {
    fn parse(mut input: Tokenstack) -> PRes<Self> {
        match input.peek() {
            Token::FnKey => Self::parse_fnty(input),
            _ => Self::parse_plain(input),
        }
    }
    fn parse_fnty(mut input: Tokenstack) -> PRes<Self> {
        // "fn" "("
        expect!(input, Token::FnKey);
        expect!(input, Token::OpenParen);
        let mut args = vec![];
        // ((Type ",")* Type)?
        while let Ok((s, a)) = Type::parse(input) {
            input = s;
            args.push(a);
            match input.peek() {
                Token::Comma => {
                    input.pop();
                }
                _ => break,
            }
        }
        // ")"
        expect!(input, Token::CloseParen);
        // (":" Type)?
        let ret_type = if input.peek() == &Token::Colon {
            input.pop();
            let (s, a) = Type::parse(input)?;
            input = s;
            Some(Box::new(a))
        } else {
            None
        };
        Ok((input, Self::FnType { args, ret_type }))
    }
    fn parse_plain(mut input: Tokenstack) -> PRes<Self> {
        let mut refs = vec![];
        let mut array = vec![];
        let mut params = vec![];
        // ("*" "mut"?)*
        while input.peek() == &Token::Star {
            input.pop();
            if input.peek() == &Token::MutKey {
                input.pop();
                refs.push(true);
            } else {
                refs.push(false);
            }
        }
        // "["^n
        while input.peek() == &Token::OpenBracket {
            input.pop();
            array.push(None);
        }
        // Ident
        let name = input.pop();
        if !name.is_ident() {
            return Err(Error::Expected {
                expected: Token::Ident("".to_string()),
                found: name,
                origin: input,
            });
        }
        // ("[" (Type ",")* Type "]")?
        if input.peek() == &Token::OpenBracket {
            input.pop();
            while let Ok((s, a)) = Type::parse(input) {
                input = s;
                params.push(a);
                match input.peek() {
                    Token::Comma => {
                        input.pop();
                    }
                    _ => break,
                }
            }
            expect!(input, Token::CloseBracket);
        }
        for n in 0..array.len() {
            if input.peek() == &Token::SemiCol {
                input.pop();
                let a = input.pop();
                if !a.is_num() {
                    return Err(Error::Expected {
                        expected: Token::Num("".to_string()),
                        found: a,
                        origin: input,
                    });
                }
                array[n] = Some(a.as_usize());
            }
            expect!(input, Token::CloseBracket);
        }
        Ok((
            input,
            Self::Plain {
                name,
                params,
                array,
                refs,
            },
        ))
    }
}

fn parse_genargs(mut input: Tokenstack) -> PRes<Vec<GenericArg>> {
    // "["
    expect!(input, Token::OpenBracket);
    // (GenArg ",")* GenArg
    let mut args = vec![];
    while let Ok((s, a)) = GenericArg::parse(input) {
        input = s;
        args.push(a);
        match input.peek() {
            Token::Comma => {
                input.pop();
            }
            _ => break,
        }
    }
    // "]"
    expect!(input, Token::CloseBracket);
    Ok((input, args))
}

#[derive(Debug)]
struct GenericArg {
    name: Token,
    constraints: Vec<Token>,
}
impl GenericArg {
    fn parse(mut input: Tokenstack) -> PRes<Self> {
        // Ident
        let name = match input.pop() {
            Token::Ident(n) => Token::Ident(n),
            _ => todo!(),
        };
        // (":" (Ident "+")* Ident)?
        let mut constraints = vec![];
        if input.peek() == &Token::Colon {
            input.pop();
            while input.peek().is_ident() {
                let n = input.pop();
                constraints.push(n);
                match input.peek() {
                    Token::Plus => {
                        input.pop();
                    }
                    _ => break,
                }
            }
        }
        Ok((input, Self { name, constraints }))
    }
}
