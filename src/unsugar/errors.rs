//! Errors that can be encountered during the unsugarization of the syntax
//! tree.
//!
//! TODO: location tracking, capturing faulty expression, etc.

quick_error! {
    #[derive(Debug,PartialEq)]
    pub enum Scope {
        FreeVariable {
            description("A variable that was not defined is used here:")
        }
        AlreadyDefined {
            description("This variable was already defined earlier")
        }
        UninternedVariable {
            description("A variable that was not interned (defined earlier) \
                        is used here:")
        }
    }
}

quick_error! {
    #[derive(Debug,PartialEq)]
    pub enum Module {
        Inexistant {
            description("Attempt to import from a moduel that doesn't exist!")
        }
        UnexposedImport {
            description("Attempt to import a symbol from a module that \
                        doesn't export that symbol")
        }
        PathImport {
            description("Use of A.Path in an import list")
        }
        Unsuported {
            description("The use of unqualified type constructor imports \
                        is currently not supported in this compiler :(")
        }
    }
}
