mod lexer;
mod misc;
mod tokens;

#[derive(Clone, Copy)]
pub struct Provenance<'s> {
    pub start: usize,
    pub end: usize,
    pub source: &'s str,
}
impl std::fmt::Debug for Provenance<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Provenance(bytes {}:{})", self.start, self.end)
    }
}

impl<'s> Provenance<'s> {
    pub fn report(&self) {
        let start = self.source[..self.start]
            .rfind("\n")
            .map(|n| n + 1)
            .unwrap_or(0);
        let end = self.source[self.start..]
            .find("\n")
            .map(|n| self.start + n)
            .unwrap_or(self.source.len());
        let pretext = &self.source[start..self.start];
        let text = &self.source[self.start..self.end.min(end)];
        let posttext = &self.source[self.end.min(end)..end];
        println!("╭─[rprt]: at bytes [{}:{}]", self.start, self.end);
        println!(
            "│ {}{}{}{}{}{}",
            pretext,
            misc::Ansi::Red,
            misc::Ansi::Underline,
            text,
            misc::Ansi::Default,
            posttext
        );
        println!("╰───");
    }
}

fn main() {
    let mut input = String::new();
    input += "// this is a comment\n";
    input += "func main(): int {\n";
    input += "    let a = 5;\n";
    input += "    let s = \"this is a string\n";
    input += "        ;\n";
    input += "    a\n";
    input += "    // and another comment\n";
    input += "}";
    println!("[cat]:\n{}", input);

    println!("[lex]:");
    for t in lexer::Lexer::from(input.as_str()) {
        println!("{:?}", t);
    }

    for t in lexer::LexerIter::new(&input) {
        t.loc.report();
    }
}
