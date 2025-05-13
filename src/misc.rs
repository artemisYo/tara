use std::{
    collections::BTreeSet,
    io::{Read, Seek},
    marker::PhantomData,
};

#[derive(Debug)]
pub struct GuardedFile(std::fs::File);
impl From<std::fs::File> for GuardedFile {
    fn from(value: std::fs::File) -> Self {
        Self(value)
    }
}
impl GuardedFile {
    pub fn access<O, F: FnOnce(&std::fs::File) -> O>(&self, f: F) -> O {
        let mut file = &self.0;
        let start = file.stream_position().unwrap();
        let o = f(file);
        file.seek(std::io::SeekFrom::Start(start)).unwrap();
        o
    }
}

pub struct CharRead<R>(R);
impl<R: Read> CharRead<R> {
    pub fn new(r: R) -> Self {
        Self(r)
    }
}
impl<R: Read> Iterator for CharRead<R> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let mut bytes = [0; 4];
        let buf = &mut bytes[0..1];
        if self.0.read(buf).ok()? < buf.len() {
            return None;
        }
        let len = bytes[0].leading_ones() as usize;
        assert!(len != 1);
        match len {
            0 => std::str::from_utf8(&bytes[0..1]).unwrap().chars().next(),
            2..4 => {
                let buf = &mut bytes[1..len];
                if self.0.read(buf).ok()? < buf.len() {
                    return None;
                };
                std::str::from_utf8(&bytes[0..len]).unwrap().chars().next()
            }
            _ => panic!("utf8 input has non-utf8-encoded bytes?"),
        }
    }
}

#[derive(Debug)]
pub struct Svec<K, V> {
    keys: Vec<K>,
    vals: Vec<V>,
}
impl<K: Eq, V> Svec<K, V> {
    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.vals.iter()
    }
    #[inline]
    pub fn excursion<O>(&mut self, f: impl FnOnce(&mut Self) -> O) -> O {
        let start = self.len();
        let out = f(self);
        self.truncate(start);
        out
    }
    pub fn insert(&mut self, k: K, v: V) {
        self.keys.push(k);
        self.vals.push(v);
    }
    pub fn find(&self, k: &K) -> Option<&V> {
        self.keys
            .iter()
            .enumerate()
            .rev()
            .find(|(_, c)| &k == c)
            .map(|(i, _)| &self.vals[i])
    }
    pub fn len(&self) -> usize {
        self.keys.len()
    }
    pub fn truncate(&mut self, l: usize) {
        self.keys.truncate(l);
        self.vals.truncate(l);
    }
}
impl<K, V> Default for Svec<K, V> {
    fn default() -> Self {
        Self {
            keys: Default::default(),
            vals: Default::default(),
        }
    }
}

#[derive(Debug)]
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
    pub fn register(&mut self, value: T, id: I) -> Result<(), ()> {
        if I::from(self.inner.len()) != id {
            dbg!(id.into(), self.inner.len());
            return Err(());
        }
        self.inner.push(value);
        Ok(())
    }
    // pub fn push_range(&mut self, values: impl Iterator<Item = T>) -> (I, I) {
    //     let start = I::from(self.inner.len());
    //     self.inner.extend(values);
    //     let end = I::from(self.inner.len());
    //     (start, end)
    // }
    pub fn promise(&self) -> I {
        I::from(self.inner.len())
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

pub trait Indexer: From<usize> + Into<usize> + Eq {}
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
// impl<I: Indexer, T> std::ops::Index<(I, I)> for Ivec<I, T> {
//     type Output = [T];

//     fn index(&self, index: (I, I)) -> &Self::Output {
//         let (start, end) = index;
//         let (start, end) = (start.into(), end.into());
//         &self.inner[start..end]
//     }
// }
// impl<I: Indexer, T> std::ops::IndexMut<(I, I)> for Ivec<I, T> {
//     fn index_mut(&mut self, index: (I, I)) -> &mut Self::Output {
//         let (start, end) = index;
//         let (start, end) = (start.into(), end.into());
//         &mut self.inner[start..end]
//     }
// }

#[macro_export]
macro_rules! MkIndexer {
    ($v:vis $name:ident, $base:ty) => {
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
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
        impl $crate::misc::Indexer for $name {}
    };
}

#[derive(Default)]
pub struct Interner {
    map: BTreeSet<&'static str>,
}
impl Interner {
    fn intern(&mut self, s: &str) -> &'static str {
        if let Some(o) = self.map.get(s) {
            return o;
        }
        let i: Box<str> = s.into();
        let i = Box::leak(i);
        self.map.insert(i);
        i
    }
}

// #[derive(Clone, Copy, Debug)]
// pub struct Istr(pub &'static str);
// impl PartialEq for Istr {
//     fn eq(&self, other: &Self) -> bool {
//         std::ptr::eq(self.0, other.0)
//     }
// }
// impl Eq for Istr {}
// impl PartialOrd for Istr {
//     fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
//         Some(self.cmp(other))
//     }
// }
// impl Ord for Istr {
//     fn cmp(&self, other: &Self) -> std::cmp::Ordering {
//         // should be fine, as interning would recognize strings
//         // of different lengths as different
//         #[allow(ambiguous_wide_pointer_comparisons)]
//         (self.0 as *const str).cmp(&(other.0 as *const str))
//     }
// }
// impl From<&str> for Istr {
//     fn from(value: &str) -> Self {
//         thread_local! {
//             static STATE: RefCell<Interner> = const { RefCell::new(Interner { map: BTreeSet::new() }) };
//         }
//         Istr(STATE.with(|i| i.borrow_mut().intern(value)))
//     }
// }
// impl std::ops::Deref for Istr {
//     type Target = str;

//     fn deref(&self) -> &Self::Target {
//         self.0
//     }
// }

#[macro_export]
macro_rules! CommonEnum {
    (
        $(# $attr:tt)?
        $ve:vis enum $name:ident {
            $($case:ident : $body:ty),+
            $(,)?
        }
        $($fns:tt)*
    ) => {
        $(#$attr)?
        $ve enum $name {
            $($case($body)),+
        }
        $(impl From<$body> for $name {
            fn from(v: $body) -> $name {
                Self:: $case (v)
            }
        })+
        impl $name {
            CommonEnum! {
                impl { $($case : $body),+ }
                $($fns)*
            }
        }
    };
    (
        impl { $($case:ident : $body:ty),+ }
        $v:vis self.$fn:ident( $($n:ident : $t:ty),* )
        -> $out:ty;
        $($fns:tt)*
    ) => {
        $v fn $fn(self, $($n : $t),*) -> $out {
            CommonEnum! {
                match { $($case : $body),+ }
                self.$fn( $($n),* )
            }
        }
        CommonEnum! {
            impl { $($case : $body),+ }
            $($fns)*
        }
    };
    (
        impl { $($case:ident : $body:ty),+ }
        $v:vis &self.$fn:ident( $($n:ident : $t:ty),* )
        -> $out:ty;
        $($fns:tt)*
    ) => {
        $v fn $fn(&self, $($n : $t),*) -> $out {
            CommonEnum! {
                match { $($case : $body),+ }
                self.$fn( $($n),* )
            }
        }
        CommonEnum! {
            impl { $($case : $body),+ }
            $($fns)*
        }
    };
    (
        impl { $($case:ident : $body:ty),+ }
        $v:vis &mut self.$fn:ident( $($n:ident : $t:ty),* )
        -> $out:ty;
        $($fns:tt)*
    ) => {
        $v fn $fn(&mut self, $($n : $t),*) -> $out {
            CommonEnum! {
                match { $($case : $body),+ }
                self.$fn( $($n),* )
            }
        }
        CommonEnum! {
            impl { $($case : $body),+ }
            $($fns)*
        }
    };
    (impl { $($case:ident : $body:ty),+ }) => {};
    (
        match { $($case:ident : $body:ty),+ }
        $this:ident.$fn:ident $args:tt
    ) => {
        match $this {$(
            Self:: $case(c) => c.$fn $args
        ),+}
    }
}
