//! A generic handler for scopes.
use std::hash::Hash;
use fxhash::FxHashMap;
use super::errors;

/// A wrapper around ScopedMap to handle gracefully scope names with
/// qualified paths.
#[derive(Clone,Debug)]
pub struct NameSpace<scope>
    where scope: Eq + Hash + Copy {
    /// Precises what is the "prefix" of a symbol that must be looked up.
    /// The prefixed form of a symbol is said to be "canonical"
    current_scope: Vec<scope>,
    /// The dictionary of names avaliable in the current scope.
    /// Ie: source code name --> compiler name (everything fully scoped)
    names: ScopedMap<Vec<scope>,Vec<scope>>,
}

impl<scope> NameSpace<scope> where scope: Eq + Hash + Copy {
    /// Create an empty name space at a top-level scope. All items will
    /// be prefixed with the given `base_scope`
    pub fn new(base_scope: Vec<scope>) -> Self {
        NameSpace {
            current_scope: base_scope,
            names: ScopedMap::new(FxHashMap::default()),
        }
    }

    /// Create a name space at a top-level scope.
    /// The name space will be pre-populated with given `base_map`.
    /// All items added afterward will be prefixed with the given `base_scope`.
    pub fn with_base_map(
        base_scope: Vec<scope>,
        base_map: FxHashMap<Vec<scope>,Vec<scope>>,
    ) -> Self {
        NameSpace {
            current_scope: base_scope,
            names: ScopedMap::new(base_map),
        }
    }

    /// Run `inner` with self inside a new scope.
    /// The returned value is the return value of `inner`, which
    /// should be self updated.
    pub fn within_scope(mut self, scope: scope, inner: fn(Self)->Self) -> Self
    {
        self.enter_scope(scope);
        let mut new_self = (inner)(self);
        new_self.leave_scope().unwrap();
        new_self
    }

    pub(super) fn enter_scope(&mut self, scope_name: scope) {
        self.current_scope.push(scope_name);
        self.names.enter_scope();
    }

    pub(super) fn leave_scope(&mut self) -> Option<()> {
        self.current_scope.pop()?;
        self.names.leave_scope()
    }

    /// Returns the fully qualified name of the given `path`, according to
    /// scoping rules.
    /// # Errors
    /// When the name is not in the scope
    pub fn get(&self, name: &Vec<scope>) -> Result<&[scope],errors::Scope> {
        self.names.get(name)
            .map(|x| x.as_ref())
            .ok_or(errors::Scope::FreeVariable)
    }

    /// Add the given name to the scope (the context handles the scope)
    /// If `new_name` was declared in a previous scope, it will shadow
    /// it. If `new_name` was declared previously in this scope, errors.
    /// # Error
    /// When `new_name` is a duplicate declaration.
    pub fn insert(&mut self, new_name: scope) -> Result<(),errors::Scope> {
        if self.names.is_in_last_scope(&vec![new_name]) {
            Err(errors::Scope::AlreadyDefined)
        } else {
            let mut full_path = self.current_scope.clone();
            full_path.push(new_name);
            self.names.insert(vec![new_name], full_path);
            Ok(())
        }
    }

}


// ScopedMap


/// A wrapper around HashMap to handle scoping rules.
#[derive(Clone,Debug)]
struct ScopedMap<key,value> where key: Eq + Hash {
    layers: Vec<FxHashMap<key,value>>,
}

impl<key,value> ScopedMap<key,value> where key: Eq + Hash {
    fn new(base: FxHashMap<key,value>) -> Self {
        ScopedMap { layers: vec![base] }
    }

    /// Run `inner` with self created with a new layer.
    fn within_scope(mut self, inner: fn(Self)->Self) -> Self {
        self.enter_scope();
        let mut new_self = (inner)(self);
        new_self.leave_scope().unwrap();
        new_self
    }

    fn enter_scope(&mut self) {
        let new_map = FxHashMap::default();
        self.layers.push(new_map);
    }

    fn leave_scope(&mut self) -> Option<()> {
        if self.layers.len() <= 1 {
            None
        } else {
            self.layers.pop().unwrap();
            Some(())
        }
    }

    fn insert(&mut self, key: key, value: value) -> Option<value> {
        self.layers.last_mut().unwrap().insert(key,value)
    }

    /// Get associated value of `key`. First check last layer, then
    /// former last up to base, in that order.
    /// The result is `None` when `key` is not contained in any layer.
    fn get(&self, key: &key) -> Option<&value> {
        self.layers
            .iter().rev()
            .fold(None, |value, scope| value.or_else(|| scope.get(key)))
    }

    /// Returns wether given key is in the last layer
    fn is_in_last_scope(&self, key: &key) -> bool {
        self.layers.last().unwrap().contains_key(key)
    }
}

#[cfg(test)] mod test { include!("scopedname-test.rs"); }
