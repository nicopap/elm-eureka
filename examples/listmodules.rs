//! Displays module doc and exported symbols of
//! all elm source files in an elm project
#![feature(io)]

extern crate elm_eureka;

use std::path::{Path,PathBuf};
use std::thread;
use std::sync::mpsc::{Receiver,Sender,channel};
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use std::ops::IndexMut;

use elm_eureka::Parser;
use elm_eureka::packages_reader;
use elm_eureka::parser::tree;

//There is a race condition somehow????
//FIXME: if there is more than 1 thread, stuff breaks
const NTHREAD:i32=1;

fn parse_and_feedback(
    receive: Receiver<Option<(PathBuf, String)>>,
    send: Sender<Option<(String,
                         String,
                         Option<String>,
                         tree::ExportList,
                         Vec<tree::ElmImport>
                        )>>
) {
    while let Some((source_path, module_name)) = receive.recv().unwrap() {
        let file = File::open(source_path).unwrap();
        let char_stream = BufReader::new(file).chars().map(|x| x.unwrap());
        let mut parser = Parser::new(char_stream);
        let exports = parser.module_exports().clone();
        let imports = parser.imports().to_vec();
        let name = String::from(parser.module_name());
        let doc = parser.module_doc().clone();
        send.send(Some((module_name, name,doc,exports,imports))).unwrap();
    }
    send.send(None).unwrap();
}

pub fn main() {
    let packagepath = Path::new("examples/elm-spa-example");
    let source_info = packages_reader::info(packagepath).unwrap();
    let sources =
        source_info.dependencies
            .iter()
            .chain(source_info.source_files.iter())
            .map(|(n,p)| (n,source_info.project_dir.join(p)));

    let (send_processed, receive_processed) = channel();
    let mut send_channels : Vec<Sender<Option<(PathBuf, String)>>>
        = Vec::new();
    for _ in 0..NTHREAD {
        let sender_copy = send_processed.clone();
        let (send_paths, receiver) = channel();
        thread::spawn(move || parse_and_feedback(receiver, sender_copy));
        send_channels.push(send_paths);
    }
    let mut i = 0i32;
    for (module_name, source_path) in sources {
        send_channels
            .index_mut((i % NTHREAD) as usize)
            .send(Some((source_path, module_name.clone())))
            .unwrap();
        i += 1;
    }
    for chan in send_channels {
        chan.send(None).unwrap();
    }
    while let Some((module, name,doc,exports,imports))
        = receive_processed.recv().unwrap()
    {
        let module_doc : String =
            doc .clone()
                .map(|x| x.chars().take_while(|&c| c != '\n').collect())
                .unwrap_or(String::from("No docstrings :("));

        let module_name = &name;
        let module_exports = match exports {
            tree::ExportList::Unqualified => format!("{}", "unqualified"),
            tree::ExportList::List(ref exports) => format!("{:?}", exports),
        };
        let module_imports = format!("{:?}", imports);
        println!("
            #### {} ####
            name: {}
            exports: {}
            has doc: {}
            imports: {}
            ", module, module_name, module_exports,
            module_doc,
            module_imports
        );
    }
}
