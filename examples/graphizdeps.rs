//! Prints on stdout a graphiz "dot" file that represents
//! the dependency tree of the given project, showing only
//! the projects modules.
#![feature(io)]

extern crate elm_eureka;

use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use std::env::args;

use elm_eureka::Parser;
use elm_eureka::packages_reader;

pub fn main() {
    let packagepath =
        args().nth(1).unwrap_or(String::from("examples/elm-spa-example"));
    let sources = packages_reader::info(packagepath.as_ref()).unwrap();
    let keywordify = | name:String | name.replace(".","_").to_lowercase();

    println!("digraph dependencies {{");
    let iter_sources
        = sources.source_files.iter()
            .map(|(n,p)| (n,sources.project_dir.join(p)));
    for (module_name, source_path) in iter_sources {
        let small_name = keywordify(module_name.clone());
        let file = File::open(source_path).unwrap();
        let reader = BufReader::new(file);
        let char_stream = reader.chars().map(|x| x.unwrap());
        let mut parser = Parser::new(char_stream);
        println!("\t{}[label=\"{}\"];", small_name, module_name);

        for import in parser.imports() {
            let import_name : &String = &import.global_name;
            if !sources.source_files.contains_key(import_name) {continue}
            let small_import_name = keywordify(import_name.clone());
            println!( "\t{} -> {};", small_import_name, small_name);
        }
    }
    println!("}}");
}
