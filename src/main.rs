mod ansi;
mod lexer;
mod misc;
mod tokens;
mod tara;
pub use tara::*;

use ansi::*;

#[derive(Debug, Clone, Copy)]
pub struct Provenance {
    pub start: usize,
    pub end: usize,
}

impl Provenance {
    pub fn meet(&self, other: &Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
    pub fn report<'a>(
        &self,
        source: &Module,
        pointer: Style,
        kind: StyledStr,
        title: &str,
        notes: impl Iterator<Item = &'a str>,
    ) {
        let mut notes = notes.peekable();
        let notes_empty = notes.peek().is_none();
        let src = source.get_source();
        let name = source.get_path();
        let Some(start) = src.get(..self.start) else {
            return;
        };
        let start = start.rfind("\n").map(|n| n + 1).unwrap_or(0);
        let Some(end) = src.get(self.end..) else {
            return;
        };
        let end = end.find("\n").map(|n| self.end + n).unwrap_or(src.len());
        let text = &src[start..end];

        let digits = start.checked_ilog10().unwrap_or(0) + 1;
        let digits = digits.max(end.checked_ilog10().unwrap_or(0) + 1);
        let digits = digits as usize;
        println!("╭─[{}]@{}:b{}: {}", kind, name.to_string_lossy(), self.start, title);
        for (start, line) in text.lines().scan(start, |c, l| {
            let start = *c;
            *c += l.len() + 1;
            Some((start, l))
        }) {
            let hl_start = self.start.saturating_sub(start);
            let hl_end = self.end.saturating_sub(start).min(line.len());
            let pretext = &line[0..hl_start];
            let text = &line[hl_start..hl_end];
            let posttext = &line[hl_end..];
            println!(
                "│ {:digits$} │ {}{}{}",
                start,
                Style::default().apply(pretext),
                pointer.apply(text),
                Style::default().apply(posttext)
            );
        }
        if !notes_empty {
            println!("├─[{}Notes{}]:", Style::yellow(), Style::default());
            for n in notes {
                let mut lines = n.lines();
                let Some(first) = lines.next() else {
                    continue;
                };
                println!("│ - {}", first);
                for l in lines {
                    println!("│   {}", l);
                }
            }
        }
        print!("╰──");
        for _ in 0..digits {
            print!("─");
        }
        println!("{}", if notes_empty {"╯"} else {"─"});
    }
}

fn main() {
    let mut ctx = Tara::from("example/main.tara");
    Provenance {
        start: 0,
        end: ctx.get_source(ctx.entry).len(),
    }
    .report(
        ctx.get_module(ctx.entry),
        Style::default(),
        Style::yellow().apply("Cat"),
        "",
        [].into_iter(),
    );
    // println!("{}", ctx.get_source(ctx.entry));

    // println!("[lex]:");
    // for t in ctx.get_module(ctx.entry).get_lexer() {
    //     println!("{:?}", t);
    // }

    let pi = ctx.preimport(preimport::In { m: ctx.entry });
    let ops: Vec<_> = pi.ops.into_iter().map(|o| format!("{:?}", o)).collect();
    Provenance {
        start: 0,
        end: 0,
    }.report(
        ctx.get_module(ctx.entry),
        Style::default(),
        Style::yellow().apply("Ops"),
        "",
        ops.iter().map(|s| s.as_ref())
    );
    // println!("[scan]:");
    // for t in &ctx.query::<prescan::Prescan>(ops).0 {
    //     println!("{:?}", t);
    // }

    // println!("[scan rep]:");
    // for t in &ctx.query::<prescan::Prescan>(ops).0 {
    //     t.loc.report(
    //         ctx.get_source(ctx.entry),
    //         Style::cyan().apply("Report"),
    //         "current token is:",
    //         &[],
    //     );
    // }

    // println!("[ops]:");
    // for o in &ctx.query::<prescan::Prescan>(ops).1 {
    //     println!("{:?}", o);
    // }

    // println!("[imports]:");
    // for o in &ctx.query::<prescan::Prescan>(ops).2 {
    //     println!("{:?}", o);
    // }
}
