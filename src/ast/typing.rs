use super::*;

pub fn do_typing(root: &mut Root) {
    root.get_type();
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Type {
	Int,
	Void,
}

impl File {
	pub(super) fn get_type(&mut self) -> &Type {
        self.typing.get_or_insert_with(|| self.tail.get_type().clone())
	}
}

impl Statement {
	pub(super) fn get_type(&mut self) -> &Type {
		match self {
			Self::Let(l) => l.get_type(),
			Self::Expr(e) => e.get_type(),
		}
	}
}

impl LetStmt {
	pub(super) fn get_type(&mut self) -> &Type {
        self.typing.get_or_insert_with(|| self.init.get_type().clone())
	}
}

impl Expr {
	pub(super) fn get_type(&mut self) -> &Type {
		match self {
			Self::Binary(b) => b.get_type(),
			Self::Single(s) => s.get_type(),
			Self::While(w) => w.get_type(),
			Self::If(i) => i.get_type(),
		}
	}
}

impl Block {
	pub(super) fn get_type(&mut self) -> &Type {
		self.0.get_type()
	}
}

impl IfExpr {
	pub(super) fn get_type(&mut self) -> &Type {
        self.typing.get_or_insert_with(|| {
            let smash = self.smash.get_type();
		    let pass = self.pass.get_type();
            if smash != pass {
			    Type::Void
		    } else {
			    smash.clone()
		    }
        })
	}
}

impl WhileExpr {
	pub(super) fn get_type(&mut self) -> &Type {
        self.typing.get_or_insert_with(|| {
            let smashing = self.smashing.get_type();
            let pass = self.pass.get_type();
            if smashing != pass {
                Type::Void
            } else {
                smashing.clone()
            }
        })
	}
}

impl BinExpr {
	pub(super) fn get_type(&mut self) -> &Type {
        self.typing.get_or_insert_with(|| {
            assert!(self.args[0].get_type() == &Type::Int);
		    assert!(self.args[1].get_type() == &Type::Int);
            Type::Int
        })
	}
}

impl SinExpr {
	pub(super) fn get_type(&self) -> &Type {
		match self {
			Self::Number(_) => &Type::Int,
			Self::Name(_) => &Type::Int,
			Self::Empty => &Type::Void,
		}
	}
}
