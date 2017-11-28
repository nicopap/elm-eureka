//! Crawls the dependencies of an elm project
//!
//! Then outputs the contents of each source files.
#![feature(io)]

extern crate elm_eureka;

use std::path::Path;
use std::thread;
use std::sync::mpsc::{Receiver,Sender,channel};
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use std::ops::IndexMut;

use elm_eureka::lexer::LexableIterator;
use elm_eureka::tokens::ElmToken;
use elm_eureka::packages_reader;

// FIXME: race condition breaks the demo if ran concurrently
const NTHREAD:i32=1;
fn lex_file(source_path : Box<Path>) -> Vec<ElmToken> {
    let file = File::open(source_path).unwrap();
    let reader = BufReader::new(file);
    let lex = reader.chars().map(|x| x.unwrap()).lex();
    lex.collect()
}

fn lex_and_feedback(
    receive: Receiver<Option<(Box<Path>, String)>>,
    send: Sender<Option<(String, Vec<ElmToken>)>>
) {
    while let Some((source_path, module_name)) = receive.recv().unwrap() {
        let tokens = lex_file(source_path);
        send.send(Some((module_name, tokens))).unwrap();
    }
    send.send(None).unwrap();
}

pub fn main() {
    let packagepath = Path::new("examples/elm-spa-example");
    let source_info = packages_reader::info(packagepath).unwrap();
    let sources = source_info.dependencies.into_iter().chain(source_info.source_files.into_iter());

    let (send_processed, receive_processed) = channel();
    let mut send_channels : Vec<Sender<Option<(Box<Path>, String)>>> = Vec::new();
    for _ in 0..NTHREAD {
        let sender_copy = send_processed.clone();
        let (send_paths, receiver) = channel();
        thread::spawn(move || lex_and_feedback(receiver, sender_copy));
        send_channels.push(send_paths);
    }
    let mut i = 0i32;
    for (module_name, source_path) in sources {
        send_channels
            .index_mut((i % NTHREAD) as usize)
            .send(Some((source_path, module_name)))
            .unwrap();
        i += 1;
    }
    for chan in send_channels {
        chan.send(None).unwrap();
    }
    while let Some((module, tokens)) = receive_processed.recv().unwrap() {
        println!("\n#### {} ####\n", module);
        for token in tokens {
            print!("{} ", token);
        }
    }
}
