//! Handles name resolution in the context of an elm program
use fxhash::FxHashMap;

use super::interner::Interner;
use super::scopedname::NameSpace;

pub enum Scope<'fname> {
    Function(&'fname str),
    // where u32, u16 are row,column to distinguish between different
    // syntactic elements.
    Let(u32,u16),
    Lambda(u32,u16),
    // where (u32, u16) is (row,column) of beginning of the branch.
    CaseBranch(u32,u16),
}

pub type ImportedEnv = FxHashMap<Vec<usize>,Vec<usize>>;

pub struct Context {
    interner: Interner,
    name_space: NameSpace<usize>,
}

impl Context {
    /// Creates a new Context for given module
    ///
    /// base_name: the module name (`Path.Like.That`)
    /// interner: the string interner to use for symbol names resolutions,
    /// imports: the imported names mapped to their actual fully qualified
    /// paths. Important to furnish ALL accessible names. Such that with
    /// `import A as B exposing (c)` where `A` exports `a`, `b` and `c`, we
    /// have `imports` mapping like that:
    /// ```not-rust
    /// B.a -> A.a
    /// B.b -> A.b
    /// B.c -> A.c
    /// c -> A.c
    /// ```
    pub fn new(
        base_name: &str,
        mut interner: Interner,
        imports: ImportedEnv,
    ) -> Self {
        let intern_name = interner.get_or_intern_path(base_name);

        let name_space = NameSpace::with_base_map(intern_name, imports);
        Context { interner, name_space }
    }

    pub fn within_scope(
        mut self,
        scope: &Scope,
        inner: fn(Self)->Self,
    ) -> Self {
        use self::Scope::*;
        let scope_layer_name = match scope {
            &Function(name) =>
                self.interner.get_or_intern_name(name),

            &Let(row,col) => {
                let name = format!("{}:{}let", row, col);
                self.interner.get_or_intern_name(name)
            },
            &Lambda(row,col) => {
                let name = format!("{}:{}\\", row, col);
                self.interner.get_or_intern_name(name)
            },
            &CaseBranch(row,col) => {
                let name = format!("{}:{}of", row, col);
                self.interner.get_or_intern_name(name)
            },
        };
        self.name_space.enter_scope(scope_layer_name);
        let mut new_self = (inner)(self);
        new_self.name_space.leave_scope();
        new_self
    }
}

