#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    Ident = 0,
    //--Whitespace start
    Space = 1,
    Tab,
    LF,
    //--Whitespace end
    //--Delimiters start
    OpPar,
    ClPar,
    OpBrace,
    ClBrace,
    OpBrack,
    ClBrack,
    OpKet,
    ClKet,
    //--Delimiters end
    Operator,
    //--Keywords start
    Fn,
    //--Keywords end
}

#[derive(Debug)]
pub struct Token<'a> {
    kind: TokenType,
    location: &'a str,
}
#[derive(Debug)]
pub struct ErrorToken<'a> {
    str_location: &'a str,
    func_location: &'static str,
}

fn tokenize_punct<'a>(input: &'a str) -> Result<(Token<'a>, usize), ErrorToken> {
    const PUNCTUATION: &[(char, TokenType)] = &[
        (' ', TokenType::Space),
        ('\t', TokenType::Tab),
        ('\n', TokenType::LF),
        ('(', TokenType::OpPar),
        (')', TokenType::ClPar),
        ('{', TokenType::OpBrace),
        ('}', TokenType::ClBrace),
        ('[', TokenType::OpBrack),
        (']', TokenType::ClBrack),
        ('<', TokenType::OpKet),
        ('>', TokenType::ClKet),
    ];
    let (_, k) = PUNCTUATION
        .iter()
        .map(|(c, k)| (input.starts_with(*c), k))
        .find(|(c, _)| *c)
        .ok_or(ErrorToken {
            str_location: input,
            func_location: "tokenize_punct: L96",
        })?;
    let t = Token {
        kind: *k,
        location: input.get(0..1).ok_or(ErrorToken {
            str_location: input,
            func_location: "tokenize_punct: L102",
        })?,
    };
    return Ok((t, 1));
}

fn tokenize_keyword<'a>(input: &'a str) -> Result<(Token<'a>, usize), ErrorToken> {
    const KEYWORDS: &[(&'static str, TokenType)] = &[("fn", TokenType::Fn)];
    let (_, l, k) = KEYWORDS
        .iter()
        .map(|(s, k)| (input.starts_with(s), s.len(), k))
        .find(|(s, _, _)| *s)
        .ok_or(ErrorToken {
            str_location: input,
            func_location: "tokenize_keyword: L115",
        })?;
    let t = Token {
        kind: *k,
        location: input.get(0..l).ok_or(ErrorToken {
            str_location: input,
            func_location: "tokenize_keyword: L121",
        })?,
    };
    return Ok((t, l));
}

fn tokenize_op<'a>(input: &'a str) -> Result<(Token<'a>, usize), ErrorToken> {
    const OPS: &[char] = &[
        '=', '+', '-', '&', '|', '*', '^', '!', '/', ',', '.', ':', '@', '#', '$', '%', '~', '`',
    ];
    let (i, _) =
        input
            .chars()
            .take_while(|c| OPS.contains(&c))
            .fold((0, 'a'), |(mut i, mut l), c| {
                if i == 0 {
                    l = c;
                    i += 1;
                    (i, l)
                } else if l == c {
                    i += 1;
                    (i, l)
                } else {
                    (i, l)
                }
            });
    if i == 0 {
        return Err(ErrorToken {
            str_location: input,
            func_location: "tokenize_op: L111",
        });
    }
    let t = Token {
        kind: TokenType::Operator,
        location: &input[..i],
    };
    return Ok((t, i));
}

fn tokenize_ident<'a>(input: &'a str) -> Result<(Token<'a>, usize), ErrorToken> {
    let mut i = 0;
    for j in 0..input.len() {
        if tokenize_op(input.get(j..).ok_or(ErrorToken {
            str_location: input,
            func_location: "tokenize_ident: L153",
        })?)
        .is_ok()
            || tokenize_punct(input.get(j..).ok_or(ErrorToken {
                str_location: input,
                func_location: "tokenize_ident: L158",
            })?)
            .is_ok()
        {
            i = j;
            break;
        }
    }
    let t = Token {
        kind: TokenType::Ident,
        location: input.get(0..i).ok_or(ErrorToken {
            str_location: input,
            func_location: "tokenize_ident: L169",
        })?,
    };
    return Ok((t, i));
}

pub fn tokenize<'a>(mut input: &'a str) -> Result<Vec<Token<'a>>, ErrorToken> {
    const IGNORE: &[TokenType] = &[TokenType::Space, TokenType::Tab, TokenType::LF];
    let mut out = Vec::new();
    let tokenizers: [fn(&str) -> Result<(Token, usize), ErrorToken>; 4] = [
        tokenize_keyword,
        tokenize_punct,
        tokenize_op,
        tokenize_ident,
    ];
    while input.len() > 0 {
        let (t, l) = tokenizers
            .iter()
            .find_map(|f| f(input).map_or(None, |r| Some(r)))
            .ok_or(ErrorToken {
                str_location: input,
                func_location: "tokenize_run: L199",
            })?;
        input = input.get(l..).ok_or(ErrorToken {
            str_location: input,
            func_location: "tokenize_run: L203",
        })?;
        if !IGNORE.contains(&t.kind) {
            out.push(t);
        }
    }
    return Ok(out);
}

#[derive(Debug)]
pub enum TokenTree<'a> {
    Bare(Token<'a>),
    Group {
        delimiters: (Token<'a>, Token<'a>),
        children: Box<[Self]>,
    },
}
impl<'a> TokenTree<'a> {
    fn unwrap_bare(self) -> Token<'a> {
        match self {
            Self::Bare(t) => t,
            e => panic!("tried to unwrap TokenTree::Bare(Token), but found {:?}", e),
        }
    }
}

// TODO:
// figure out how to differentiate on operators and delimiters:
// 'fn main() -> 1 + 1.' vs 'some_struct.field'
pub fn preparse<'a>(input: Vec<Token<'a>>) -> Vec<TokenTree<'a>> {
    const DELIMITERS: &[(&'static str, &'static str)] =
        &[("{", "}"), ("(", ")"), ("[", "]"), ("->", ".")];
    fn delim_check_open(input: &Token) -> bool {
        DELIMITERS
            .iter()
            .find_map(|(d, _)| {
                if d == &input.location {
                    Some(true)
                } else {
                    None
                }
            })
            .unwrap_or(false)
    }
    fn delim_check_closed(input: &Token) -> Option<&'static str> {
        DELIMITERS
            .iter()
            .find_map(|(p, d)| if d == &input.location { Some(*p) } else { None })
    }
    let mut unclosed = Vec::new();
    let mut stack = Vec::new();
    for t in input {
        if delim_check_open(&t) {
            unclosed.push(stack.len());
            stack.push(TokenTree::Bare(t));
        } else if let Some(p) = delim_check_closed(&t) {
            if let Some(i) = unclosed.pop() {
                // TODO: handle failure on this side
                // as you could have something like
                // " some code ( value, tuple }"
                let children = stack.drain(i + 1..).collect::<Vec<_>>().into_boxed_slice();
                let opening_del = stack.pop().unwrap().unwrap_bare();
                if opening_del.location == p {
                    let delimiters = (opening_del, t);
                    stack.push(TokenTree::Group {
                        delimiters,
                        children,
                    });
                } else {
                    todo!("Mismatched delimiters found!");
                }
            } else {
                todo!("Seemingly unclosed delimiter found!");
            }
        } else {
            stack.push(TokenTree::Bare(t));
        }
    }
    return stack;
}
