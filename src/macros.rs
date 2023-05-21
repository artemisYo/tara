pub trait Set<T: std::fmt::Debug>: std::fmt::Debug {
    fn contains(&self, _: &T) -> bool;
}
impl<T: std::fmt::Debug + PartialEq> Set<T> for [T] {
    fn contains(&self, e: &T) -> bool {
        self.contains(&e)
    }
}

pub trait Origin: std::fmt::Debug {}

#[macro_export]
macro_rules! orig {
    ($name:ident) => {
        #[derive(Debug)]
        struct $name;
        impl Origin for $name {}
        #[allow(non_upper_case_globals)]
        const origin: &dyn Origin = &$name;
    };
}
#[macro_export]
macro_rules! charset {
    ($name:ident) => {
        #[derive(Debug)]
        struct $name;
        #[allow(non_upper_case_globals)]
        const set: &dyn Set<char> = &$name;
    };
    ($name:ident, $pred:expr) => {
        #[derive(Debug)]
        struct $name;
        impl Set<char> for $name {
            fn contains(&self, o: &char) -> bool {
                $pred(o)
            }
        }
        #[allow(non_upper_case_globals)]
        const set: &dyn Set<char> = &$name;
    };
}
