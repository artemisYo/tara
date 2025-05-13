use crate::{data::files::*, report, FmtMessage, Provenance};

impl File {
    pub fn open(ctx: &mut Files, file: std::path::PathBuf) -> Id {
        assert!(file.extension().is_some_and(|ex| ex == "tara") && file.is_file());

        // let source = std::fs::read_to_string(&file).unwrap();
        // let source = source.leak();
        let source_len = file.metadata().unwrap().len() as usize;
        let source = std::fs::File::open(&file).unwrap().into();
        let id = ctx.0.push(File {
            path: file.clone(),
            source_len,
            source,
            children: Vec::new(),
        });

        let mut subtree = file;
        subtree.set_extension("");
        if subtree.is_dir() {
            let children = std::fs::read_dir(subtree)
                .unwrap()
                .filter_map(Result::ok)
                .map(|p| p.path())
                .filter(|p| p.extension().is_some_and(|p| p == "tara") && p.is_file())
                .map(|p| File::open(ctx, p))
                .collect();
            ctx.0[id].children = children;
        }

        id
    }
}

impl Files {
    pub fn report(&self, msg: FmtMessage, aux: &[FmtMessage]) {
        report(self, msg, aux);
    }
    pub fn root(&self) -> Id { 0.into() }
    pub fn print(&self) {
        self.print_subtree(self.root(), 0);
    }
    fn print_subtree(&self, root: Id, level: usize) {
        print!("{}", "  ".repeat(level));
        println!("| {}", self[root].path.to_string_lossy());
        for &c in self[root].children.iter() {
            self.print_subtree(c, level+1);
        }
    }
    pub fn from(entry: std::path::PathBuf) -> Self {
        let mut out = Self(Default::default());
        File::open(&mut out, entry);
        out
    }
    pub fn eof_loc(&self, m: Id) -> Provenance {
        let len = self[m].source_len;
        Provenance::Span {
            module: m,
            start: len,
            end: len,
        }
    }
}
impl std::ops::Index<Id> for Files {
    type Output = File;
    fn index(&self, i: Id) -> &Self::Output {
        &self.0[i]
    }
}
