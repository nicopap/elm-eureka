//! Parses a token stream

use tokens::ElmToken;
use std::iter::Peekable;
use std::error::Error;
use std::fmt;


#[derive(Debug)] pub struct ElmDoc(String);

pub struct Path(String);
pub type Name=String;
pub type Operator=String;


pub enum ElmExports {
    Unqualified,
    List(Vec<ExportEntry>),
}

pub enum ExportEntry {
    Name(Name),
    Operator(Operator),
    WithConstructors(Name, Vec<Name>),
    WithAllConstructors(Name),
}

pub enum ModuleAttribute {
    Usual,
    Port,
    Effect { command: Option<Name>, subscription: Option<Name> },
}

pub struct ElmModule {
    name: Path,
    exports: ElmExports,
    attribute: ModuleAttribute,
}

pub struct ElmImport {
    global_name: Path,
    local_name: Path,
    exposes: Option<ElmExports>,
}

struct ElmSource {
    module_declaration: ElmModule,
    doc_string: Option<ElmDoc>,
    imports: Vec<ElmImport>,
    // TODO: rest of elm source
}

// Notes: a few adaptation must be done to the ElmToken stream in order
// to not trip up the parser:
// - N0's need to be nubed (de-doubled)
// - All leading N0's must be removed
// - Add one terminal N0
