#[derive(Debug)]
pub struct Message {
	pub summary: Box<str>,
	pub hints: Vec<Box<str>>,
}

#[derive(Debug)]
pub struct ErrorQueue {
	errors: Vec<Message>,
	warns: Vec<Message>,
}
impl ErrorQueue {
	pub fn new() -> Self {
		Self {
			errors: vec![],
			warns: vec![],
		}
	}
	pub fn log_error(&mut self, error: Message) {
		self.errors.push(error);
	}
	pub fn log_warning(&mut self, warning: Message) {
		self.warns.push(warning);
	}
	pub fn print(&self) {
		for e in &self.errors {
			println!("error: {}", e.summary);
			for h in &e.hints {
				println!("  | hint: {}", h);
			}
			print!("\n");
		}
		print!("\n");
		for w in &self.warns {
			println!("warning: {}", w.summary);
			for h in &w.hints {
				println!("  | hint: {}", h);
			}
			print!("\n");
		}
	}
}
