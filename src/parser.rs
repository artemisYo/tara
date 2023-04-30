use crate::comb_parser::{choose, maybe, seq, star, ParseError, ParseResult};

type Parser<PID> = crate::comb_parser::Parser<PID, PResult, PError>;
type Context<PID> = crate::comb_parser::Context<PID, PResult, PError>;

#[derive(Debug, Clone, Copy)]
pub enum Tag {
    Default,
    Fn,
    Block,
    Expr,
    ExprList,
    FuncCall,
    Program,
}

#[derive(Debug, Clone, Copy)]
pub enum Keyword {
    Fn,
    End,
}
impl Keyword {
    fn as_str(&self) -> &'static str {
        match self {
            Self::Fn => "fn",
            Self::End => "end",
        }
    }
}
#[derive(Debug, Clone)]
pub enum PResult {
    Empty(Tag),
    Ref(String),
    Keyword(Keyword),
    Label(Tag, Box<Self>),
    Congregate(Tag, Box<[Self]>),
}
impl ParseResult<Tag> for PResult {
    fn congregate(t: Tag, s: Vec<Self>) -> Self {
        return Self::Congregate(t, s.into_boxed_slice());
    }

    fn label(t: Tag, s: Self) -> Self {
        return Self::Label(t, Box::new(s));
    }

    fn empty(t: Tag) -> Self {
        return Self::Empty(t);
    }
}

#[derive(Debug, Clone)]
pub enum PError {
    Ident,
    Op,
    Keyword(Keyword),
    Label(Tag, Box<Self>),
    Congregate(Tag, Box<[Self]>),
}

impl ParseError<Tag> for PError {
    fn congregate(t: Tag, s: Vec<Self>) -> Self {
        return Self::Congregate(t, s.into_boxed_slice());
    }
    fn label(t: Tag, s: Self) -> Self {
        return Self::Label(t, Box::new(s));
    }
}

fn parse_keyword<PID>(keyword: Keyword, delims: &'static [&str]) -> Parser<PID> {
    Box::new(move |_: &Context<PID>, input: &str| {
        let keyword_len = keyword.as_str().len();
        if input.starts_with(keyword.as_str())
            && delims.iter().any(|d| input[keyword_len..].starts_with(d))
        {
            return Ok((&input[keyword_len..], PResult::Keyword(keyword)));
        } else {
            return Err((input, PError::Keyword(keyword)));
        }
    })
}

fn parse_ident<PID>(delims: &'static [&'static str]) -> Parser<PID> {
    Box::new(move |_: &Context<PID>, input: &str| {
        let mut head = 0;
        while !delims.iter().any(|d| input[head..].starts_with(d)) {
            head += 1;
        }
        if head > 0 {
            return Ok((&input[head..], PResult::Ref(input[..head].to_owned())));
        } else {
            return Err((input, PError::Ident));
        }
    })
}

fn parse_op<PID>(op: &'static str) -> Parser<PID> {
    Box::new(move |_: &Context<PID>, input: &str| {
        if input.starts_with(op) {
            return Ok((&input[op.len()..], PResult::Ref(op.to_owned())));
        } else {
            return Err((input, PError::Op));
        }
    })
}

pub fn parse<'a>(input: &'a str) -> Result<(&'a str, PResult), (&'a str, PError)> {
    const DELIMS: &[&'static str] = &[" ", "\n", "\t"];
    #[derive(PartialEq, Eq, Hash)]
    enum Parsers {
        Whitespace,
        ExprParser,
        BlockParser,
        FnParser,
        FileParser,
    }
    let mut parsers: Context<Parsers> = Context::new();
    parsers.insert(
        Parsers::Whitespace,
        star(
            Tag::Default,
            Ok(choose(
                Tag::Default,
                vec![Ok(parse_op(" ")), Ok(parse_op("\t")), Ok(parse_op("\n"))],
            )),
        ),
    );
    parsers.insert(
        Parsers::ExprParser,
        choose(
            Tag::Expr,
            vec![
                Ok(seq(
                    Tag::FuncCall,
                    vec![
                        Ok(parse_ident(DELIMS)),
                        Ok(parse_op("(")),
                        Err(Parsers::ExprParser),
                        Ok(parse_op(")")),
                    ],
                )),
                Ok(parse_ident(DELIMS)),
            ],
        ),
    );

    parsers.insert(
        Parsers::BlockParser,
        seq(
            Tag::Block,
            vec![
                Ok(parse_op("->")),
                Ok(maybe(
                    Tag::ExprList,
                    Ok(seq(
                        Tag::Default,
                        vec![
                            Ok(star(
                                Tag::Default,
                                Ok(seq(
                                    Tag::Default,
                                    vec![Err(Parsers::ExprParser), Ok(parse_op(","))],
                                )),
                            )),
                            Err(Parsers::ExprParser),
                            Ok(maybe(Tag::Default, Ok(parse_op(",")))),
                        ],
                    )),
                )),
                Ok(parse_keyword(Keyword::End, DELIMS)),
            ],
        ),
    );

    parsers.insert(
        Parsers::FnParser,
        seq(
            Tag::Fn,
            vec![
                Ok(parse_keyword(Keyword::Fn, DELIMS)),
                Err(Parsers::Whitespace),
                Ok(parse_ident(DELIMS)),
                Err(Parsers::Whitespace),
                Err(Parsers::BlockParser),
            ],
        ),
    );

    parsers.insert(
        Parsers::FileParser,
        choose(Tag::Program, vec![Err(Parsers::FnParser)]),
    );
    parsers.get(&Parsers::FileParser).unwrap()(&parsers, input)
}
