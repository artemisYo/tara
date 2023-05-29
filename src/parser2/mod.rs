trait Parseable<'b>: Sized {
    fn parse<'a: 'b>(_: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError>;
}

trait Origin: std::fmt::Debug + 'static {}
macro_rules! origin {
    ($name:ident) => {
        #[derive(Debug)]
        struct $name;
        impl Origin for $name {}
        #[allow(non_upper_case_globals)]
        const origin: &dyn Origin = &$name;
    };
}

#[derive(Debug)]
enum ParseError<'a> {
    Expected(StrState<'a>, &'static str),
    PlainText(StrState<'a>, &'static str),
    Trace(Box<Self>, &'static dyn Origin),
    Multiple(Vec<Self>, &'static dyn Origin),
}
impl ParseError<'_> {
    fn trace(self, origin: &'static dyn Origin) -> Self {
        Self::Trace(Box::new(self), origin)
    }
}

macro_rules! trace {
    ($parse:expr, $origin:ident) => {
        ($parse).map_err(|e| e.trace($origin))?
    };
}

#[derive(Clone, Copy, Debug)]
pub struct StrState<'a> {
    string: &'a str,
    index: usize,
    pos: (usize, usize),
}
impl<'a> StrState<'a> {
    pub fn new(string: &'a str) -> Self {
        Self {
            string,
            index: 0,
            pos: (0, 0),
        }
    }
    fn advance(&mut self, x: usize) {
        if self.get().chars().count() < x {
            panic!("Tried to advance past string length!");
        }
        self.string[self.index..]
            .chars()
            .take(x)
            .map(|c| (if c == '\n' { 1 } else { 0 }, c.len_utf8()))
            .for_each(|(n, i)| {
                self.pos.0 += n;
                self.index += i;
            });
        self.pos.1 = self.index - self.string[..self.index].rfind("\n").unwrap_or(0);
    }
    fn strip_prefix<'b>(&mut self, prefix: &'b str) -> Result<(), &'b str> {
        if self.get().starts_with(prefix) {
            self.advance(prefix.chars().count());
            Ok(())
        } else {
            Err(prefix)
        }
    }
    fn strip_whitespace(&mut self) {
        self.advance(self.get().chars().take_while(|c| c.is_whitespace()).count());
    }
    fn strip_prefix_then_whitespace<'b>(&mut self, prefix: &'b str) -> Result<(), &'b str> {
        self.strip_prefix(prefix)?;
        self.strip_whitespace();
        Ok(())
    }
    fn is_empty(&self) -> bool {
        self.string.len() >= self.index
    }
    fn get(&self) -> &str {
        &self.string[self.index..]
    }
}

pub struct Program<'a>(Vec<Function<'a>>);
impl<'b> Parseable<'b> for Program<'b> {
    fn parse<'a: 'b>(mut input: StrState<'a>) -> Result<(StrState, Self), ParseError> {
        origin!(Program);
        let mut acc = vec![];
        while !input.is_empty() {
            match Function::parse(input) {
                Ok((s, f)) => {
                    input = s;
                    acc.push(f);
                }
                Err(e) => {
                    return Err(e.trace(origin));
                }
            }
        }
        Ok((input, Self(acc)))
    }
}
pub struct Function<'a> {
    name: Name<'a>,
    params: Vec<Parameter<'a>>,
    ret_type: Option<TypeSig<'a>>,
    body: Block<'a>,
}
impl<'b> Parseable<'b> for Function<'b> {
    fn parse<'a: 'b>(mut input: StrState<'a>) -> Result<(StrState, Self), ParseError> {
        origin!(Function);
        input
            .strip_prefix_then_whitespace("fn")
            .map_err(|s| ParseError::Expected(input, s).trace(origin))?;
        let (mut input, name) = Name::parse(input)?;
        input
            .strip_prefix_then_whitespace("(")
            .map_err(|s| ParseError::Expected(input, s).trace(origin))?;
        let (mut input, params) = List::parse::<Parameter>(input)
            .ok()
            .unwrap_or_else(|| (input, vec![]));
        input
            .strip_prefix_then_whitespace(")")
            .map_err(|s| ParseError::Expected(input, s).trace(origin))?;
        let ret_type = TypeSig::parse(input).ok().map(|(s, a)| {
            input = s;
            a
        });
        let (input, body) = Block::parse(input)?;
        Ok((
            input,
            Self {
                name,
                params,
                ret_type,
                body,
            },
        ))
    }
}
pub struct Parameter<'a>(Name<'a>, TypeSig<'a>);
impl<'b> Parseable<'b> for Parameter<'b> {
    fn parse<'a: 'b>(input: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        origin!(Parameter);
        let (input, name) = trace!(Name::parse(input), origin);
        let (input, type_sig) = trace!(TypeSig::parse(input), origin);
        Ok((input, Self(name, type_sig)))
    }
}
pub struct TypeSig<'a>(Type<'a>);
impl<'b> Parseable<'b> for TypeSig<'b> {
    fn parse<'a: 'b>(mut input: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        origin!(TypeSig);
        input
            .strip_prefix_then_whitespace(":")
            .map_err(|s| ParseError::Expected(input, s).trace(origin))?;
        let (input, r#type) = trace!(Type::parse(input), origin);
        Ok((input, Self(r#type)))
    }
}
pub struct Type<'a> {
    name: Name<'a>,
    refs: usize,
    array_depth: Vec<Option<NumLit>>,
    params: Vec<TypeParam<'a>>,
}
impl<'b> Parseable<'b> for Type<'b> {
    fn parse<'a: 'b>(mut input: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        origin!(Type);
        let mut refs = 0;
        let mut array_depth = vec![];
        while let Ok(_) = input.strip_prefix_then_whitespace("*") {
            refs += 1;
        }
        while let Ok(_) = input.strip_prefix_then_whitespace("[") {
            array_depth.push(None);
        }
        let (mut input, name) = trace!(Name::parse(input), origin);
        let (mut input, params) = trace!(
            if input.strip_prefix_then_whitespace("[").is_ok() {
                let (mut i, l) = trace!(List::parse::<TypeParam>(input), origin);
                i.strip_prefix_then_whitespace("]")
                    .map_err(|s| ParseError::Expected(i, s).trace(origin))?;
                Result::<(StrState, Vec<_>), ParseError>::Ok((i, l))
            } else {
                Ok((input, vec![]))
            },
            origin
        );
        for i in 0..array_depth.len() {
            if input.strip_prefix_then_whitespace(";").is_ok() {
                let (s, n) = trace!(NumLit::parse(input), origin);
                array_depth[i] = Some(n);
                input = s;
            }
            input
                .strip_prefix_then_whitespace("]")
                .map_err(|s| ParseError::Expected(input, s).trace(origin))?;
        }
        Ok((
            input,
            Self {
                name,
                refs,
                array_depth,
                params,
            },
        ))
    }
}
pub struct TypeParam<'a>(Type<'a>);
impl<'b> Parseable<'b> for TypeParam<'b> {
    fn parse<'a: 'b>(input: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        origin!(TypeParam);
        let (input, r#type) = trace!(Type::parse(input), origin);
        Ok((input, Self(r#type)))
    }
}
pub struct Block<'a> {
    body: Option<ExprList<'a>>,
    returns: bool,
}
impl<'b> Parseable<'b> for Block<'b> {
    fn parse<'a: 'b>(mut input: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        origin!(Block);
        input
            .strip_prefix_then_whitespace("->")
            .map_err(|s| ParseError::Expected(input, s).trace(origin))?;
        let body = ExprList::parse(input).ok().map(|(i, l)| {
            input = i;
            l
        });
        let returns = if input.strip_prefix_then_whitespace(".").is_ok() {
            true
        } else if input.strip_prefix_then_whitespace(";").is_ok() {
            false
        } else {
            return Err(ParseError::Multiple(
                vec![
                    ParseError::Expected(input, "."),
                    ParseError::Expected(input, ";"),
                ],
                origin,
            ));
        };
        Ok((input, Self { body, returns }))
    }
}
pub enum Expressions<'a> {
    PipeLine(PipeLine),
    BlockExpr(BlockExpr),
    NormExpr(NormExpr<'a>),
}
impl<'b> Parseable<'b> for Expressions<'b> {
    fn parse<'a: 'b>(input: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        origin!(Expressions);
        let mut err = vec![];
        match PipeLine::parse(input) {
            Ok((s, a)) => {
                return Ok((s, Self::PipeLine(a)));
            }
            Err(e) => {
                err.push(e);
            }
        }
        match BlockExpr::parse(input) {
            Ok((s, a)) => {
                return Ok((s, Self::BlockExpr(a)));
            }
            Err(e) => {
                err.push(e);
            }
        }
        match NormExpr::parse(input) {
            Ok((s, a)) => {
                return Ok((s, Self::NormExpr(a)));
            }
            Err(e) => {
                err.push(e);
            }
        }
        Err(ParseError::Multiple(err, origin))
    }
}
pub struct ExprList<'a>(Vec<Expressions<'a>>);
impl<'b> Parseable<'b> for ExprList<'b> {
    fn parse<'a: 'b>(mut input: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        origin!(ExprList);
        let mut l = vec![];
        while let Ok((s, a)) = if let Ok((mut s, a)) = PipeLine::parse(input) {
            s.strip_prefix_then_whitespace(",")
                .and(Ok((s, Expressions::PipeLine(a))))
        } else if let Ok((mut s, a)) = BlockExpr::parse(input) {
            s.strip_prefix_then_whitespace(",")
                .and(Ok((s, Expressions::BlockExpr(a))))
        } else if let Ok((mut s, a)) = NormExpr::parse(input) {
            s.strip_prefix_then_whitespace(",")
                .and(Ok((s, Expressions::NormExpr(a))))
        } else {
            Err("")
        } {
            input = s;
            l.push(a);
        }
        let (input, a) = trace!(Expressions::parse(input), origin);
        l.push(a);
        Ok((input, Self(l)))
    }
}
pub enum NormExpr<'a> {
    FuncCall(FuncCall<'a>),
    Arithmetic(Arithmetic),
    DataAssign(DataAssign<'a>),
    DataDecl(DataDecl),
    NumRange(NumRange),
    Literal(Literal),
    Name(Name<'a>),
}
impl<'b> Parseable<'b> for NormExpr<'b> {
    fn parse<'a: 'b>(input: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        origin!(NormExpr);
        let mut err = vec![];
        match FuncCall::parse(input) {
            Ok((s, a)) => {
                return Ok((s, Self::FuncCall(a)));
            }
            Err(e) => {
                err.push(e);
            }
        }
        match Arithmetic::parse(input) {
            Ok((s, a)) => {
                return Ok((s, Self::Arithmetic(a)));
            }
            Err(e) => {
                err.push(e);
            }
        }
        match DataAssign::parse(input) {
            Ok((s, a)) => {
                return Ok((s, Self::DataAssign(a)));
            }
            Err(e) => {
                err.push(e);
            }
        }
        match DataDecl::parse(input) {
            Ok((s, a)) => {
                return Ok((s, Self::DataDecl(a)));
            }
            Err(e) => {
                err.push(e);
            }
        }
        match NumRange::parse(input) {
            Ok((s, a)) => {
                return Ok((s, Self::NumRange(a)));
            }
            Err(e) => {
                err.push(e);
            }
        }
        match Literal::parse(input) {
            Ok((s, a)) => {
                return Ok((s, Self::Literal(a)));
            }
            Err(e) => {
                err.push(e);
            }
        }
        match Name::parse(input) {
            Ok((s, a)) => {
                return Ok((s, Self::Name(a)));
            }
            Err(e) => {
                err.push(e);
            }
        }
        Err(ParseError::Multiple(err, origin))
    }
}
pub struct FuncCall<'a> {
    prefixes: Vec<Name<'a>>,
    name: Name<'a>,
    args: Vec<Expressions<'a>>,
}
impl<'b> Parseable<'b> for FuncCall<'b> {
    fn parse<'a: 'b>(mut input: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        origin!(FuncCall);
        let mut prefixes = vec![];
        while let Ok((s, a)) = if let Ok((mut s, a)) = Name::parse(input) {
            s.strip_prefix(".")
                .map_err(|e| ParseError::Expected(s, e).trace(origin))?;
            Ok((s, a))
        } else {
            Err(())
        } {
            input = s;
            prefixes.push(a);
        }
        let (mut input, name) = trace!(Name::parse(input), origin);
        input
            .strip_prefix("(")
            .map_err(|s| ParseError::Expected(input, s).trace(origin))?;
        let (mut input, args) =
            List::parse::<Expressions>(input).unwrap_or_else(|_| (input, vec![]));
        input
            .strip_prefix(")")
            .map_err(|s| ParseError::Expected(input, s).trace(origin))?;
        Ok((
            input,
            Self {
                prefixes,
                name,
                args,
            },
        ))
    }
}
pub struct Arithmetic();
impl<'b> Parseable<'b> for Arithmetic {
    fn parse<'a: 'b>(_: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        todo!()
    }
}
pub struct DataAssign<'a> {
    name: Name<'a>,
    source: Box<Expressions<'a>>,
}
impl<'b> Parseable<'b> for DataAssign<'b> {
    fn parse<'a: 'b>(input: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        origin!(DataAssign);
        let (mut input, name) = trace!(Name::parse(input), origin);
        input
            .strip_prefix_then_whitespace("=")
            .map_err(|s| ParseError::Expected(input, s).trace(origin))?;
        let (input, source) = trace!(Expressions::parse(input), origin);
        Ok((
            input,
            Self {
                name,
                source: Box::new(source),
            },
        ))
    }
}
pub struct DataDecl<'a> {
    name: Name<'a>,
    type_sig: Option<TypeSig<'a>>,
    source: Box<Expressions<'a>>,
    is_mutable: bool,
}
impl<'b> Parseable<'b> for DataDecl<'b> {
    fn parse<'a: 'b>(mut input: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        origin!(DataDecl);
        let mut is_mutable = false;
        if let Err(s) = input.strip_prefix_then_whitespace("let") {
            match input.strip_prefix_then_whitespace("mut") {
                Ok(_) => {
                    is_mutable = true;
                }
                Err(t) => {
                    return Err(ParseError::Multiple(
                        vec![
                            ParseError::Expected(input, s),
                            ParseError::Expected(input, t),
                        ],
                        origin,
                    ))
                }
            }
        }
        let (input, name) = trace!(Name::parse(input), origin);
        let (mut input, type_sig) = match TypeSig::parse(input) {
            Ok((s, a)) => (s, Some(a)),
            Err(_) => (input, None),
        };
        input
            .strip_prefix_then_whitespace("=")
            .map_err(|s| ParseError::Expected(input, s).trace(origin))?;
        let (input, source) = trace!(Expressions::parse(input), origin);
        Ok((
            input,
            Self {
                name,
                type_sig,
                source: Box::new(source),
                is_mutable,
            },
        ))
    }
}
pub struct NumRange();
impl<'b> Parseable<'b> for NumRange {
    fn parse<'a: 'b>(_: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        todo!()
    }
}
pub struct Literal();
impl<'b> Parseable<'b> for Literal {
    fn parse<'a: 'b>(_: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        todo!()
    }
}
pub struct BlockExpr();
impl<'b> Parseable<'b> for BlockExpr {
    fn parse<'a: 'b>(_: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        todo!()
    }
}
pub struct PipeLine();
impl<'b> Parseable<'b> for PipeLine {
    fn parse<'a: 'b>(_: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        todo!()
    }
}
pub struct Name<'a>(&'a str);
impl<'b> Parseable<'b> for Name<'b> {
    fn parse<'a: 'b>(mut input: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        origin!(Name);
        let x = input
            .get()
            .chars()
            .take_while(|c| c.is_alphabetic())
            .map(|c| c.len_utf8());
        let name = &input.string[input.index..input.index + x.clone().sum::<usize>()];
        if name.is_empty() {
            return Err(ParseError::PlainText(
                input,
                "Expected any alphabetic character for identifier!",
            )
            .trace(origin));
        }
        input.advance(x.count());
        input.strip_whitespace();
        Ok((input, Self(name)))
    }
}
pub struct NumLit();
impl<'b> Parseable<'b> for NumLit {
    fn parse<'a: 'b>(_: StrState<'a>) -> Result<(StrState<'a>, Self), ParseError> {
        todo!()
    }
}
struct List;
impl List {
    fn parse<'a, 'b: 'a, T: Parseable<'a>>(
        mut input: StrState<'b>,
    ) -> Result<(StrState, Vec<T>), ParseError> {
        let mut acc = vec![];
        while let Ok((s, a)) = match T::parse(input) {
            Ok((mut s, a)) => s.strip_prefix_then_whitespace(",").and(Ok((s, a))),
            _ => Err(""),
        } {
            input = s;
            acc.push(a);
        }
        let (mut input, a) = T::parse(input)?;
        acc.push(a);
        input.strip_prefix_then_whitespace(",").ok();
        Ok((input, acc))
    }
}
