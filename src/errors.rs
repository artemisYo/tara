pub trait Message {
	fn summary(&mut self) -> Box<str>;
	fn hints(&mut self) -> Box<dyn Iterator<Item = Box<str>> + '_>;
	fn location(&mut self) -> Box<str>;
}

pub struct UniqueError(Box<[Box<str>]>);
impl UniqueError {
	pub fn new(
		summary: &str,
		location: &str,
		mut hints: Vec<Box<str>>
	) -> Self {
		hints.push(summary.into());
		hints.push(location.into());
		Self(hints.into())
	}
}
impl Message for UniqueError {
	fn summary(&mut self) -> Box<str> {
		self.0[self.0.len() - 2].clone()
	}
	fn hints(&mut self) -> Box<dyn Iterator<Item = Box<str>> + '_> {
		Box::new(self.0.iter().take(self.0.len() - 2).cloned())
	}
	fn location(&mut self) -> Box<str> {
		self.0[self.0.len() - 1].clone()
	}
}

pub struct ErrorQueue {
	errors: Vec<Box<dyn Message>>,
	warns: Vec<Box<dyn Message>>,
}
impl ErrorQueue {
	pub fn new() -> Self {
		Self {
			errors: vec![],
			warns: vec![],
		}
	}
	#[inline]
	pub fn log_error<E: Message + 'static>(&mut self, error: E) {
		self.errors.push(Box::new(error));
	}
	#[inline]
	pub fn log_warning<E: Message + 'static>(&mut self, warning: E) {
		self.warns.push(Box::new(warning));
	}
	pub fn print(&mut self) {
		for e in &mut self.errors {
			println!("error: {}", e.summary());
			let loc = e.location();
			if !loc.is_empty() {
				println!("  @ {}", loc);
			}
			for h in e.hints() {
				println!("  | hint: {}", h);
			}
			print!("\n");
		}
		for w in &mut self.warns {
			println!("warning: {}", w.summary());
			let loc = w.location();
			if !loc.is_empty() {
				println!("  @ {}", loc);
			}
			for h in w.hints() {
				println!("  | hint: {}", h);
			}
			print!("\n");
		}
	}
}
