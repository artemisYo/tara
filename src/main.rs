mod static_helper;
use static_helper::InitOnce;

mod tokens;
use tokens::*;

fn is_delimited(post_token: &str) -> bool {
    post_token.is_empty()
        || Delimiter::parse(post_token).is_ok()
        || Grouping::parse_open(post_token).is_ok()
        || Grouping::parse_close(post_token).is_ok()
        || SKIP.contains(&post_token.chars().nth(0).unwrap())
}
fn closing_grouping_follows(input: &str) -> bool {
    Grouping::parse_close(input).is_ok()
}
fn opening_grouping_follows(input: &str) -> bool {
    Grouping::parse_open(input).is_ok()
}

impl Keyword {
    fn parse(input: &str) -> Result<(usize, Self), ()> {
        static TABLE: InitOnce<Vec<(&'static str, Keyword)>> = InitOnce::new(&|| {
            return KEYWORDS.iter().map(|t| (t.as_string(), *t)).collect();
        });
        TABLE
            .get()
            .as_ref()
            .unwrap()
            .iter()
            .find(|(s, _)| input.starts_with(s) && is_delimited(&input[s.len()..]))
            .map(|(s, t)| (s.len(), *t))
            .ok_or(())
    }
    fn as_string(&self) -> &'static str {
        match self {
            Self::Fn => "fn",
            Self::Return => "return",
            Self::Count => {
                panic!("The `Keyword` enum variant `Count` should never be constructed!")
            }
        }
    }
}

impl Delimiter {
    fn parse(input: &str) -> Result<(usize, Self), ()> {
        static TABLE: InitOnce<Vec<(char, Delimiter)>> = InitOnce::new(&|| {
            return DELIMITERS.iter().map(|t| (t.as_char(), *t)).collect();
        });
        TABLE
            .get()
            .as_ref()
            .unwrap()
            .iter()
            .find(|(s, _)| input.starts_with(*s))
            .map(|(_, t)| (1, *t))
            .ok_or(())
    }
    fn as_char(&self) -> char {
        match self {
            Self::Semicolon => ';',
            Self::Colon => ':',
            Self::Comma => ',',
            Self::Dot => '.',
            Self::LessThan => '<',
            Self::MoreThan => '>',
            Self::Equal => '=',
            Self::Plus => '+',
            Self::Minus => '-',
            Self::Asterisk => '*',
            Self::Slash => '/',
            Self::Backslash => '\\',
            Self::Pipe => '|',
            Self::Ampersand => '&',
            Self::Caret => '^',
            Self::ExclMark => '!',
            Self::QstMark => '?',
            Self::Count => {
                panic!("The `Delimiter` enum variant `Count` should never be constructed!")
            }
        }
    }
}
impl Grouping {
    fn parse_open(input: &str) -> Result<(usize, Self), ()> {
        static TABLE: InitOnce<Vec<(char, Grouping)>> = InitOnce::new(&|| {
            return GROUPINGS.iter().map(|t| (t.open_char(), *t)).collect();
        });
        TABLE
            .get()
            .as_ref()
            .unwrap()
            .iter()
            .find(|(s, _)| input.starts_with(*s))
            .map(|(_, t)| (1, *t))
            .ok_or(())
    }
    fn parse_close(input: &str) -> Result<(usize, Self), ()> {
        static TABLE: InitOnce<Vec<(char, Grouping)>> = InitOnce::new(&|| {
            return GROUPINGS.iter().map(|t| (t.close_char(), *t)).collect();
        });
        TABLE
            .get()
            .as_ref()
            .unwrap()
            .iter()
            .find(|(s, _)| input.starts_with(*s))
            .map(|(_, t)| (1, *t))
            .ok_or(())
    }
    fn open_char(&self) -> char {
        match self {
            Self::Braces => '{',
            Self::Paren => '(',
            Self::Bracket => '[',
            Self::Count => {
                panic!("The `Grouping` enum variant `Count` should never be constructed!")
            }
        }
    }
    fn close_char(&self) -> char {
        match self {
            Self::Braces => '}',
            Self::Paren => ')',
            Self::Bracket => ']',
            Self::Count => {
                panic!("The `Grouping` enum variant `Count` should never be constructed!")
            }
        }
    }
}

impl<'a> Block<'a> {
    fn parse(input: &'a str) -> Result<(usize, Self), ()> {
        let (_, g) = Grouping::parse_open(input)?;
        let (i, s) = TokenStream::parse(&input[1..])?;
        if Grouping::parse_close(&input[1 + i..])?.1 != g {
            return Err(());
        };
        return Ok((
            2 + i,
            Self {
                grouping: g,
                stream: s,
            },
        ));
    }
}

impl<'a> Literal<'a> {
    fn parse_string(input: &'a str) -> Result<(usize, Self), ()> {
        if input.is_empty() {
            return Err(());
        }
        let l = input[1..].find('\"').map(|i| i + 1).unwrap_or(input.len());
        return Ok((l, Self::String(&input[..l])));
    }
    fn parse_num(input: &'a str) -> Result<(usize, Self), ()> {
        if input.is_empty() {
            return Err(());
        }
        let l = input.chars().take_while(|c| c.is_numeric()).count();
        return Ok((l, Self::Number(&input[..l])));
    }
    fn parse_name(input: &'a str) -> Result<(usize, Self), ()> {
        if input.is_empty() {
            return Err(());
        }
        let l = (0..).take_while(|i| !is_delimited(&input[*i..])).count();
        return Ok((l, Self::Name(&input[..l])));
    }
}

impl<'a> TokenStream<'a> {
    fn parse(input: &'a str) -> Result<(usize, Self), ()> {
        if input.is_empty() {
            return Err(());
        }
        let mut index = 0;
        let mut out = Vec::new();
        while index < input.len() {
            match input[index..].chars().nth(0).unwrap() {
                c if SKIP.contains(&c) => {
                    index += 1;
                }
                _ if closing_grouping_follows(&input[index..]) => {
                    dbg!("closing_grouping");
                    return Ok((index, Self(out)));
                }
                _ if opening_grouping_follows(&input[index..]) => {
                    dbg!("opening_grouping");
                    let (i, n) = Block::parse(&input[index..])?;
                    out.push((index, Token::Block(n)));
                    index += i;
                }
                c if c.is_numeric() => {
                    let (i, n) = Literal::parse_num(&input[index..])?;
                    out.push((index, Token::Literal(n)));
                    index += i;
                }
                '\"' => {
                    let (i, n) = Literal::parse_string(&input[index..])?;
                    out.push((index, Token::Literal(n)));
                    index += i;
                }
                _ => {
                    // this clojure hack is here so as to be able to break
                    // from match arm instead of having to use elses
                    (|| {
                        if let Ok((i, n)) = Delimiter::parse(&input[index..]) {
                            dbg!("delimiter");
                            out.push((index, Token::Delimiter(n)));
                            index += i;
                            return Ok(());
                        }
                        if let Ok((i, n)) = Keyword::parse(&input[index..]) {
                            dbg!("keyword");
                            out.push((index, Token::Keyword(n)));
                            index += i;
                            return Ok(());
                        }
                        if let Ok((i, n)) = Literal::parse_name(&input[index..]) {
                            dbg!("name");
                            out.push((index, Token::Literal(n)));
                            index += i;
                            return Ok(());
                        }
                        return Err(());
                    })()?;
                }
            }
        }
        return Ok((index, Self(out)));
    }
}

fn main() {
    println!(
        "{:?}",
        TokenStream::parse("fn fib(n: int): int {\n\treturn fib(n-1) + fib(n-2);\n}")
    );
}
