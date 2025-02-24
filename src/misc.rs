pub struct PeekN<T: Iterator> {
	iter: T,
	buffer: std::collections::VecDeque<T::Item>,
}
impl<T: Iterator> Iterator for PeekN<T> {
	type Item = T::Item;
	fn next(&mut self) -> Option<Self::Item> {
		let Some(i) = self.buffer.pop_front()
		else { return self.iter.next(); };
		return Some(i);
    }
}
impl<T: Iterator> PeekN<T> {
	pub fn new(iter: T) -> Self {
		Self { iter, buffer: Default::default() }
	}
	pub fn peek(&mut self, n: usize) -> Option<&T::Item> {
		let d = n.saturating_sub(self.buffer.len());
		if d == 0 { return Some(&self.buffer[n]); }
		for _ in 0..d {
			self.buffer.push_back(self.iter.next()?);
		}
		return Some(&self.buffer[n]);
	}
}
impl<T: Iterator> From<T> for PeekN<T> {
    fn from(value: T) -> Self { Self::new(value) }
}

pub enum Ansi {
	Underline,
	Red,
	Default,
}
impl std::fmt::Display for Ansi {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use std::fmt::Write;
		match self {
			Ansi::Underline => write!(f, "\x1b[4m"),
			Ansi::Red => write!(f, "\x1b[31m"),
			Ansi::Default => write!(f, "\x1b[m")
		}
    }
}
