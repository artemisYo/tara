use std::collections::HashMap as Map;
use std::marker::PhantomData as Ph;

pub struct Qmap<K, V, S>(Map<K, Option<V>>, Ph<S>);
pub trait Query<K, V>: Sized {
    type Inputs<'a>;
    fn query(_: &mut Qmap<K, V, Self>, _: &K, _: Self::Inputs<'_>) -> V;
}
impl<K, V, S> Default for Qmap<K, V, S> {
    fn default() -> Self {
        Self(Default::default(), Ph)
    }
}
impl<K: std::fmt::Debug, V: std::fmt::Debug, S> std::fmt::Debug for Qmap<K, V, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
impl<K: std::hash::Hash + Eq, V, S: Query<K, V>> Qmap<K, V, S> {
    pub fn query(&mut self, k: K, i: S::Inputs<'_>) -> V
    where
        V: Clone + std::fmt::Debug,
        K: Clone + std::fmt::Debug,
    {
        match self.0.get(&k) {
            Some(Some(v)) => v.clone(),
            Some(None) => {
                dbg!(self);
                panic!(
                    "Map '{}' got cycle at key '{:?}'!",
                    std::any::type_name::<Self>(),
                    &k
                )
            }
            None => {
                self.0.insert(k.clone(), None);
                let v = S::query(self, &k, i);
                self.0.insert(k, Some(v.clone()));
                v
            }
        }
    }
}
