//type Set<V> = std::collections::BTreeSet<V>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum TokenType {
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
struct Token<'a> {
    kind: TokenType,
    location: &'a str,
}
#[derive(Debug)]
struct ErrorToken<'a> {
    str_location: &'a str,
    func_location: &'static str,
}

const KEYWORDS: &[(&'static str, TokenType)] = &[("fn", TokenType::Fn)];
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
const IGNORE: &[TokenType] = &[TokenType::Space, TokenType::Tab, TokenType::LF];
const PREDEF_OPS: &[&'static str] = &["->", ":", ".", ","];
static mut OPS: Vec<String> = Vec::new();

fn ops_init() {
    // TODO: thread-safe ops
    // this is fine without any checks for now
    // as the we do not run anything concurrently
    // it is to be reconsidered if multiple file
    // inputs are processed in parallel
    // since sharing defined ops will require
    // change to this
    unsafe {
        if OPS.len() == 0 {
            for o in PREDEF_OPS.iter() {
                OPS.push(o.to_string());
            }
        }
    }
}
fn ops_register(op: &str) {
    // TODO: thread-safe ops
    unsafe {
        OPS.push(op.to_string());
    }
}
fn tokenize_opreg(input: &str) -> Option<usize> {
    if input.starts_with("operator") {
        // TODO: find a better way to handle this
        let x = input.find(['.', '\n']).unwrap();
        let op = input.get(8..x)?.trim();
        ops_register(op);
        return Some(x + 1);
    }
    return None;
}

fn tokenize_punct<'a>(input: &'a str) -> Result<(Token<'a>, usize), ErrorToken> {
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
    // TODO: thread-safe ops
    let (_, s) = unsafe {
        OPS.iter()
            .map(|s| (input.starts_with(s), s.len()))
            .find(|(s, _)| *s)
    }
    .ok_or(ErrorToken {
        str_location: input,
        func_location: "tokenize_op: L136",
    })?;
    let t = Token {
        kind: TokenType::Operator,
        location: input.get(0..s).ok_or(ErrorToken {
            str_location: input,
            func_location: "tokenize_op: L142",
        })?,
    };
    return Ok((t, s));
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

fn tokenize_run<'a>(mut input: &'a str) -> Result<Vec<Token<'a>>, ErrorToken> {
    let mut out = Vec::new();
    ops_init();
    let tokenizers: [fn(&str) -> Result<(Token, usize), ErrorToken>; 4] = [
        tokenize_keyword,
        tokenize_punct,
        tokenize_op,
        tokenize_ident,
    ];
    while input.len() > 0 {
        if let Some(i) = tokenize_opreg(input) {
            input = input.get(i..).ok_or(ErrorToken {
                str_location: input,
                func_location: "tokenize_run: L187",
            })?;
        }
        let (t, l) = tokenizers
            .iter()
            .filter_map(|f| f(input).map_or(None, |r| Some(r)))
            .next()
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

fn main() {
    let s = "operator +.\nfn main() -> int {\n\tthis + fuck\n}";
    println!("{s}");
    println!("----");
    println!("{:?}", tokenize_run(s));
}
