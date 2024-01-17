pub struct ScopeMap<K, V>(Vec<(K, V)>);
#[derive(Debug, Clone, Copy)]
pub struct Scope(usize);

impl<K, V> ScopeMap<K, V> {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn from<T>(original: T) -> Self
    where
        T: IntoIterator<Item = (K, V)>,
    {
        Self(Vec::from_iter(original))
    }
    pub fn insert(&mut self, key: K, value: V) {
        self.0.push((key, value));
    }
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: std::borrow::Borrow<Q>,
        Q: PartialEq,
    {
        self.0
            .iter()
            .rfind(|(k, _)| k.borrow() == key)
            .map(|(_, v)| v)
    }
    pub fn scope(&self) -> Scope {
        Scope(self.0.len())
    }
    pub fn restore(&mut self, scope: Scope) {
        self.0.truncate(scope.0)
    }
}
