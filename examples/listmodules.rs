//! Displays module doc and exported symbols of
//! all elm source files in an elm project
#![feature(io)]

extern crate elm_eureka;

use std::path::Path;
use std::thread;
use std::sync::mpsc::{Receiver,Sender,channel};
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use std::ops::IndexMut;

use elm_eureka::lexer::Lexer;
use elm_eureka::parser::Parser;
use elm_eureka::packages_reader;
use elm_eureka::ast::ExportList;

//There is a race condition somehow????
//FIXME: if there is more than 1 thread, stuff breaks
const NTHREAD:i32=1;
fn parse_file(source_path : Box<Path>) -> Parser {
    let file = File::open(source_path).unwrap();
    let reader = BufReader::new(file);
    let lex = Lexer::new(reader.chars().map( |x| x.unwrap() ));
    Parser::new(lex)
}

fn parse_and_feedback(
    receive: Receiver<Option<(Box<Path>, String)>>,
    send: Sender<Option<(String, Parser)>>
) {
    while let Some((source_path, module_name)) = receive.recv().unwrap() {
        let parsed_tree = parse_file(source_path);
        send.send(Some((module_name, parsed_tree))).unwrap();
    }
    send.send(None).unwrap();
}

pub fn main() {
    let packagepath = Path::new("examples/elm-spa-example");
    let mut sources = packages_reader::info(packagepath).unwrap();

    let (send_processed, receive_processed) = channel();
    let mut send_channels : Vec<Sender<Option<(Box<Path>, String)>>> = Vec::new();
    for _ in 0..NTHREAD {
        let sender_copy = send_processed.clone();
        let (send_paths, receiver) = channel();
        thread::spawn(move || parse_and_feedback(receiver, sender_copy));
        send_channels.push(send_paths);
    }
    let mut i = 0i32;
    for (module_name, source_path) in sources.drain() {
        send_channels
            .index_mut((i % NTHREAD) as usize)
            .send(Some((source_path, module_name)))
            .unwrap();
        i += 1;
    }
    for chan in send_channels {
        chan.send(None).unwrap();
    }
    while let Some((module, tree)) = receive_processed.recv().unwrap() {
        let module_declr = tree.get_module_declr();
        let module_doc : String =
            tree.get_module_doc()
                .clone()
                .map(|x| x.chars().take_while(|&c| c != '\n').collect())
                .unwrap_or(String::from("No docstrings :("));

        let module_name = &module_declr.name;
        let module_exports = match module_declr.exports {
            ExportList::Unqualified => format!("{}", "unqualified"),
            ExportList::List(ref exports) => format!("{:?}", exports),
        };
        println!(
            "\n#### {} ####\nname: {},\nexports: {}\nhas doc: {}",
            module,
            module_name,
            module_exports,
            module_doc
        );
    }
}
