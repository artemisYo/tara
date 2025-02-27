use std::{collections::BTreeSet, marker::PhantomData};

pub struct PeekN<T: Iterator> {
    iter: T,
    buffer: std::collections::VecDeque<T::Item>,
}
impl<T: Iterator> Iterator for PeekN<T> {
    type Item = T::Item;
    fn next(&mut self) -> Option<Self::Item> {
        let Some(i) = self.buffer.pop_front() else {
            return self.iter.next();
        };
        Some(i)
    }
}
impl<T: Iterator> PeekN<T> {
    pub fn new(iter: T) -> Self {
        Self {
            iter,
            buffer: Default::default(),
        }
    }
    pub fn peek(&mut self, n: usize) -> Option<&T::Item> {
        let d = n.saturating_sub(self.buffer.len());
        if d == 0 {
            return Some(&self.buffer[n]);
        }
        for _ in 0..d {
            self.buffer.push_back(self.iter.next()?);
        }
        Some(&self.buffer[n])
    }
}
impl<T: Iterator> From<T> for PeekN<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

pub struct Ivec<I, T> {
    inner: Vec<T>,
    _p: PhantomData<I>,
}
impl<I: Indexer, T> Ivec<I, T> {
    pub fn push(&mut self, value: T) -> I {
        let index = I::from(self.inner.len());
        self.inner.push(value);
        index
    }
    pub fn push_range(&mut self, values: impl Iterator<Item = T>) -> (I, I) {
        let start = I::from(self.inner.len());
        self.inner.extend(values);
        let end = I::from(self.inner.len());
        (start, end)
    }
}
impl<I, T> Default for Ivec<I, T> {
    fn default() -> Self {
        Self {
            inner: Vec::default(),
            _p: PhantomData,
        }
    }
}

pub trait Indexer: From<usize> + Into<usize> {}
impl<I: Indexer, T> std::ops::Index<I> for Ivec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        let index = index.into();
        &self.inner[index]
    }
}
impl<I: Indexer, T> std::ops::IndexMut<I> for Ivec<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        let index = index.into();
        &mut self.inner[index]
    }
}
impl<I: Indexer, T> std::ops::Index<(I, I)> for Ivec<I, T> {
    type Output = [T];

    fn index(&self, index: (I, I)) -> &Self::Output {
        let (start, end) = index;
        let (start, end) = (start.into(), end.into());
        &self.inner[start..end]
    }
}
impl<I: Indexer, T> std::ops::IndexMut<(I, I)> for Ivec<I, T> {
    fn index_mut(&mut self, index: (I, I)) -> &mut Self::Output {
        let (start, end) = index;
        let (start, end) = (start.into(), end.into());
        &mut self.inner[start..end]
    }
}

#[macro_export]
macro_rules! MkIndexer {
    ($v:vis $name:ident, $base:ty) => {
        #[derive(Clone, Copy, PartialEq, Eq)]
        $v struct $name($base);
        impl From<usize> for $name {
            fn from(value: usize) -> Self {
                let Ok(value): Result<$base, _> = value.try_into() else {
                    panic!(
                        "Indexer {} could not be created with value {}!",
                        stringify!($name),
                        value
                    );
                };
                Self(value)
            }
        }
        impl From<$name> for usize {
            fn from(value: $name) -> Self {
                value.0.try_into().unwrap()
            }
        }
        impl Indexer for $name {}
    };
}

#[derive(Default)]
pub struct Interner {
    map: BTreeSet<&'static str>,
}
impl Interner {
    pub fn intern(&mut self, s: &str) -> &'static str {
        if let Some(o) = self.map.get(s) {
            return o;
        }
        let i: Box<str> = s.into();
        let i = Box::leak(i);
        self.map.insert(i);
        i
    }
}
