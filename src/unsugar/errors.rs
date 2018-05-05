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
