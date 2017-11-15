#![feature(io)]
extern crate elm_eureka;

use std::path::Path;
use std::thread;
use std::sync::mpsc::{Sender,channel};
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

use elm_eureka::elm_lexer::{ElmToken,IterLexer};

fn lex_file(source_path : Box<Path>) -> Vec<ElmToken> {
    let file = File::open(source_path).unwrap();
    let reader = BufReader::new(file);
    let lex = IterLexer::new(reader.chars().map( |x| x.unwrap() ));
    lex.collect()
}

fn lex_and_feedback(
    source_path: Box<Path>,
    module_name: String,
    send: Sender<(String, Vec<ElmToken>)>
) {
    let tokens = lex_file(source_path);
    send.send((module_name, tokens)).unwrap();
}

pub fn main() {
    let packagepath = Path::new("examples/elm-spa-example");
    let mut sources = elm_eureka::package_info( packagepath).unwrap();
    let module_count = sources.len();

    let (send, receive) = channel();
    for (module_name, source_path) in sources.drain() {
        let sender_copy = send.clone();
        thread::spawn(move || {
            lex_and_feedback(source_path, module_name, sender_copy);
        });
    }
    for _ in 1..module_count {
        let (module, tokens) = receive.recv().unwrap();
        println!("{:?}: {:?}\n", module, tokens);
    }
}
