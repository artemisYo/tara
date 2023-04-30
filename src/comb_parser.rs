use std::{
    collections::HashMap,
    hash::Hash,
    ops::{Deref, DerefMut},
};

pub trait ParseResult<Tag: Copy + 'static>: Sized + 'static {
    fn congregate(_: Tag, _: Vec<Self>) -> Self;
    fn label(_: Tag, _: Self) -> Self;
    fn empty(_: Tag) -> Self;
}
pub trait ParseError<Tag: Copy + 'static>: Sized + 'static {
    fn congregate(_: Tag, _: Vec<Self>) -> Self;
    fn label(_: Tag, _: Self) -> Self;
}

pub struct Context<PID, O, E>(HashMap<PID, Parser<PID, O, E>>);
impl<PID, O, E> Context<PID, O, E> {
    pub fn new() -> Self {
        return Context(HashMap::new());
    }
}
impl<PID, O, E> Deref for Context<PID, O, E> {
    type Target = HashMap<PID, Parser<PID, O, E>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<PID, O, E> DerefMut for Context<PID, O, E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
pub type Parser<PID, O, E> =
    Box<dyn for<'a, 'b> Fn(&Context<PID, O, E>, &'a str) -> Result<(&'a str, O), (&'a str, E)>>;

pub fn star<
    PID: Eq + Hash + 'static,
    Tag: Copy + 'static,
    O: ParseResult<Tag>,
    E: ParseError<Tag>,
>(
    tag: Tag,
    parser: Result<Parser<PID, O, E>, PID>,
) -> Parser<PID, O, E> {
    Box::new(move |context: &Context<PID, O, E>, input: &str| {
        let mut head = input;
        let mut out = Vec::new();
        let p = match parser.as_ref() {
            Ok(p) => p,
            Err(i) => context.get(i).unwrap(),
        };
        while let Ok((i, o)) = p(context, input) {
            out.push(o);
            head = i;
        }
        return Ok((head, O::congregate(tag, out)));
    })
}

pub fn plus<
    PID: Eq + Hash + 'static,
    Tag: Copy + 'static,
    O: ParseResult<Tag>,
    E: ParseError<Tag>,
>(
    tag: Tag,
    parser: Result<Parser<PID, O, E>, PID>,
) -> Parser<PID, O, E> {
    Box::new(move |context: &Context<PID, O, E>, input: &str| {
        let parser = match parser.as_ref() {
            Ok(p) => p,
            Err(i) => context.get(i).unwrap(),
        };
        let first_res = parser(context, input);
        if let Ok((i, o)) = first_res {
            let mut out = vec![o];
            let mut head = i;
            while let Ok((i, o)) = parser(context, head) {
                out.push(o);
                head = i;
            }
            return Ok((head, O::congregate(tag, out)));
        } else if let Err((_, o)) = first_res {
            return Err((input, E::label(tag, o)));
        } else {
            unreachable!()
        }
    })
}

pub fn maybe<
    PID: Eq + Hash + 'static,
    Tag: Copy + 'static,
    O: ParseResult<Tag>,
    E: ParseError<Tag>,
>(
    tag: Tag,
    parser: Result<Parser<PID, O, E>, PID>,
) -> Parser<PID, O, E> {
    Box::new(move |context: &Context<PID, O, E>, input: &str| {
        let parser = match parser.as_ref() {
            Ok(p) => p,
            Err(i) => context.get(i).unwrap(),
        };
        let res = parser(context, input);
        if let Ok((i, o)) = res {
            return Ok((i, O::label(tag, o)));
        } else if let Err((_, _)) = res {
            return Ok((input, O::empty(tag)));
        } else {
            unreachable!();
        }
    })
}

pub fn seq<
    PID: Eq + Hash + 'static,
    Tag: Copy + 'static,
    O: ParseResult<Tag>,
    E: ParseError<Tag>,
>(
    tag: Tag,
    parsers: Vec<Result<Parser<PID, O, E>, PID>>,
) -> Parser<PID, O, E> {
    Box::new(move |context: &Context<PID, O, E>, input: &str| {
        let mut out = Vec::new();
        let mut head = input;
        for p in parsers.iter() {
            let p = match p {
                Ok(p) => &p,
                Err(i) => context.get(&i).unwrap(),
            };
            let res = p(context, head);
            if let Ok((i, o)) = res {
                out.push(o);
                head = i;
            } else if let Err((_, o)) = res {
                return Err((input, E::label(tag, o)));
            }
        }
        return Ok((head, O::congregate(tag, out)));
    })
}

pub fn choose<
    PID: Eq + Hash + 'static,
    Tag: Copy + 'static,
    O: ParseResult<Tag>,
    E: ParseError<Tag>,
>(
    tag: Tag,
    parsers: Vec<Result<Parser<PID, O, E>, PID>>,
) -> Parser<PID, O, E> {
    Box::new(move |context: &Context<PID, O, E>, input: &str| {
        let mut err = Vec::new();
        for p in parsers.iter() {
            let p = match p {
                Ok(p) => &p,
                Err(i) => context.get(&i).unwrap(),
            };
            let res = p(context, input);
            if let Ok((i, o)) = res {
                return Ok((i, O::label(tag, o)));
            } else if let Err((_, o)) = res {
                err.push(o);
            }
        }
        return Err((input, E::congregate(tag, err)));
    })
}
