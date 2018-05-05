//! A wrapper around the string_interner library to make it easier to
//! deal with interners in the context of the elm language.
use fxhash::FxBuildHasher;
use string_interner::StringInterner;

pub type Symbol = usize;
pub struct Interner(StringInterner<Symbol,FxBuildHasher>);

impl Interner {
    pub fn new() -> Self {
        Interner(StringInterner::with_capacity_and_hasher(
                400usize, FxBuildHasher::default()
        ))
    }

    pub fn get_or_intern_path(&mut self, name: &str) -> Vec<Symbol> {
        name.split('.')
            .map(|component| self.0.get_or_intern(component))
            .collect()
    }

    pub fn get_or_intern_name<T>(&mut self, name: T) -> Symbol
        where T:Into<String> + AsRef<str> {
        self.0.get_or_intern(name)
    }

    pub fn resolve_name(&self, symbol: Symbol) -> Option<&str> {
        self.0.resolve(symbol)
    }

    pub fn resolve_path(&self, symbols: &[Symbol]) -> Option<String> {
        symbols.iter()
            .map(|&symbol| self.resolve_name(symbol))
            .collect::<Option<Vec<&str>>>()
            .map(|value_list| value_list.join("."))
    }
}
