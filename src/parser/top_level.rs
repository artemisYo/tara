use super::Error;
use super::PRes;
use crate::expect;
use crate::tokenizer::{Token, Tokenstack};

trait TopLevel: std::fmt::Debug {}
const TOPLEVEL_PARSERS: &[fn(Tokenstack) -> PRes<Box<dyn TopLevel>>] = &[
    Function::parse_top_level,
    ProtoDef::parse_top_level,
    ProtoImpl::parse_top_level,
    TypeDef::parse_top_level,
];
fn parse_top_level(mut input: Tokenstack) -> PRes<Box<dyn TopLevel>> {
    for p in TOPLEVEL_PARSERS {
        match p(input) {
            Err(e) => {
                println!("{:?}", e);
            }
            o => return o,
        }
    }
    Err(Error::Empty)
}
#[derive(Debug)]
pub struct Program(Vec<Box<dyn TopLevel>>);
impl Program {
    pub fn parse(mut input: Tokenstack) -> PRes<Self> {
        let mut acc = vec![];
        loop {
            if input.is_empty() {
                break;
            }
            match parse_top_level(input) {
                Ok((s, a)) => {
                    input = s;
                    acc.push(a);
                }
                Err(e) => {
                    println!("{:?}", e);
                    break;
                }
            }
        }
        Ok((input, Self(acc)))
    }
}
#[derive(Debug)]
pub struct Function {
    path: Vec<Token>,
    generics: Vec<GenericArg>,
    params: Vec<(Token, Type)>,
    ret_type: Box<Option<Type>>,
    body: Box<dyn super::block::Block>,
}
impl TopLevel for Function {}
impl Function {
    fn parse_top_level(input: Tokenstack) -> PRes<Box<dyn TopLevel>> {
        Self::parse(input).map(|(s, n)| (s, Box::new(n) as Box<dyn TopLevel>))
    }
    fn parse(mut input: Tokenstack) -> PRes<Self> {
        let mut path = vec![];
        let mut params = vec![];
        // "fn"
        expect!(input, Token::FnKey);
        // (Ident ".")* Ident
        while input.peek().is_ident() {
            let n = input.pop();
            path.push(n);
            match input.peek() {
                Token::Accessor => {
                    input.pop();
                }
                _ => {
                    break;
                }
            }
        }
        if path.is_empty() {
            return Err(Error::Expected {
                expected: Token::Ident("".to_string()),
                found: input.pop(),
                origin: input,
            });
        }
        // GenParams?
        let generics = match GenParams::parse(input) {
            Ok((s, a)) => {
                input = s;
                a.args
            }
            Err(_) => vec![],
        };
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
        let ret_type = Box::new(if input.peek() == &Token::Colon {
            input.pop();
            let (s, ty) = Type::parse(input)?;
            input = s;
            Some(ty)
        } else {
            None
        });
        let (input, body) = super::block::parse_block(input)?;
        Ok((
            input,
            Self {
                path,
                generics,
                params,
                ret_type,
                body,
            },
        ))
    }
}

#[derive(Debug)]
pub struct ProtoDef {
    name: Token,
    implementor: Box<GenericArg>,
    generics: Vec<GenericArg>,
    fields: Vec<(Token, Type)>,
}
impl TopLevel for ProtoDef {}
impl ProtoDef {
    fn parse_top_level(input: Tokenstack) -> PRes<Box<dyn TopLevel>> {
        Self::parse(input).map(|(s, n)| (s, Box::new(n) as Box<dyn TopLevel>))
    }
    fn parse(mut input: Tokenstack) -> PRes<Self> {
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
        let generics = match GenParams::parse(input) {
            Ok((s, a)) => {
                input = s;
                a.args
            }
            Err(_) => vec![],
        };
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
            Self {
                name,
                implementor,
                generics,
                fields,
            },
        ))
    }
}

#[derive(Debug)]
pub struct ProtoImpl {
    protocol: Token,
    implementor: Box<Type>,
    generics: Vec<GenericArg>,
    fields: Vec<(Token, Vec<Token>)>,
}
impl TopLevel for ProtoImpl {}
impl ProtoImpl {
    fn parse_top_level(input: Tokenstack) -> PRes<Box<dyn TopLevel>> {
        Self::parse(input).map(|(s, n)| (s, Box::new(n) as Box<dyn TopLevel>))
    }
    fn parse(mut input: Tokenstack) -> PRes<Self> {
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
        let generics = match GenParams::parse(input) {
            Ok((s, a)) => {
                input = s;
                a.args
            }
            Err(_) => vec![],
        };
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
            let mut path = vec![];
            // (Ident ".")* Ident
            while let Token::Ident(p) = input.pop() {
                path.push(Token::Ident(p));
                match input.peek() {
                    Token::Period => {
                        input.pop();
                    }
                    _ => break,
                }
            }
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
            Self {
                protocol,
                implementor,
                generics,
                fields,
            },
        ))
    }
}

#[derive(Debug)]
pub struct TypeDef {
    name: Token,
    generics: Vec<GenericArg>,
    def: Box<Result<StructBody, EnumBody>>,
}
impl TopLevel for TypeDef {}
impl TypeDef {
    fn parse_top_level(input: Tokenstack) -> PRes<Box<dyn TopLevel>> {
        Self::parse(input).map(|(s, n)| (s, Box::new(n) as Box<dyn TopLevel>))
    }
    fn parse(mut input: Tokenstack) -> PRes<Self> {
        // "type"
        expect!(input, Token::TypeKey);
        // Ident
        let name = match input.pop() {
            Token::Ident(n) => Token::Ident(n),
            _ => todo!(),
        };
        // GenParams?
        let generics = match GenParams::parse(input) {
            Ok((s, a)) => {
                input = s;
                a.args
            }
            Err(_) => vec![],
        };
        // "="
        expect!(input, Token::Equals);
        // (StructBody | EnumBody)
        let def = Box::new(match StructBody::parse(input) {
            Ok((s, a)) => {
                input = s;
                Ok(a)
            }
            _ => match EnumBody::parse(input) {
                Ok((s, a)) => {
                    input = s;
                    Err(a)
                }
                _ => todo!(),
            },
        });
        Ok((
            input,
            Self {
                name,
                generics,
                def,
            },
        ))
    }
}

#[derive(Debug)]
pub struct StructBody {
    //           name     : Type
    fields: Vec<(Token, Type)>,
}
impl StructBody {
    fn parse(mut input: Tokenstack) -> PRes<Self> {
        let mut fields = vec![];
        while input.peek().is_ident() {
            let n = input.pop();
            expect!(input, Token::Colon);
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
        Ok((input, Self { fields }))
    }
}
#[derive(Debug)]
pub struct EnumBody {
    //       discriminant (   EVField   )
    cases: Vec<(Token, Box<Option<EVField>>)>,
}
impl EnumBody {
    fn parse(mut input: Tokenstack) -> PRes<Self> {
        let mut cases = vec![];
        while input.peek() == &Token::CaseKey {
            input.pop();
            let n = input.pop();
            if !n.is_ident() {
                return Err(Error::Expected {
                    expected: Token::Ident("".to_string()),
                    found: n,
                    origin: input,
                });
            }
            let d = match EVField::parse(input) {
                Ok((s, a)) => {
                    input = s;
                    Some(a)
                }
                Err(_) => None,
            };
            cases.push((n, Box::new(d)));
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
        expect!(input, Token::SemiCol);
        Ok((input, Self { cases }))
    }
}
#[derive(Debug)]
pub struct EVField {
    names: Option<Vec<Token>>,
    tys: Vec<Type>,
}
impl EVField {
    fn parse(mut input: Tokenstack) -> PRes<Self> {
        expect!(input, Token::OpenParen);
        let mut tys = vec![];
        let names = if input.lookahead(1) == &Token::Colon {
            let mut names = vec![];
            while input.peek().is_ident() {
                let n = input.pop();
                names.push(n);
                expect!(input, Token::Colon);
                let (s, ty) = Type::parse(input)?;
                tys.push(ty);
                match input.peek() {
                    Token::Comma => {
                        input.pop();
                    }
                    _ => break,
                }
                input = s;
            }
            Some(names)
        } else {
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
        expect!(input, Token::CloseParen);
        Ok((input, Self { names, tys }))
    }
}

#[derive(Debug)]
pub struct Type {
    name: Token,
    params: Vec<Type>,
    array: Vec<Option<usize>>,
    refs: Vec<bool>,
}
impl Type {
    fn parse(mut input: Tokenstack) -> PRes<Self> {
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
            Self {
                name,
                params,
                array,
                refs,
            },
        ))
    }
}

#[derive(Debug)]
struct GenParams {
    args: Vec<GenericArg>,
}
impl GenParams {
    fn parse(mut input: Tokenstack) -> PRes<Self> {
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
        Ok((input, Self { args }))
    }
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
