//! Displays module doc and exported symbols of
//! all elm source files in an elm project
#![feature(io,use_nested_groups)]

extern crate elm_eureka;

use std::path::{Path,PathBuf};
use std::thread;
use std::sync::mpsc::{Receiver,Sender,channel};
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use std::ops::IndexMut;
use std::error::Error;

use elm_eureka::{Parser,packages_reader,parser::tree};

const NTHREAD:usize=4;

type Location=((u32,u16),(u32,u16));
struct ModuleMessage {
    module_name: String,
    name: Option<String>,
    doc: Option<String>,
    exports: tree::ExportList<String,Location>,
    imports: Vec<tree::Import<String,Location>>,
}

fn parse_and_feedback(
    receive: Receiver<Option<(PathBuf, String)>>,
    send: Sender<Option<ModuleMessage>>
) {
    while let Some((source_path, module_name)) = receive.recv().unwrap() {
        let file = File::open(source_path).unwrap();
        let char_stream = BufReader::new(file).chars().map(|x| x.unwrap());
        let mut parser = Parser::new(char_stream);
        let tree::Module{exports,imports,name,doc,..} = parser.into_parse_tree();
        match send.send(
            Some(ModuleMessage{module_name, name,doc,exports,imports})
        ) {
            Ok(()) => (),
            Err(err) => panic!("{:?}", err.description()),
        };
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
        thread::spawn(|| parse_and_feedback(receiver, sender_copy));
        send_channels.push(send_paths);
    }
    for (i, (module_name, source_path)) in sources.enumerate() {
        send_channels
            .index_mut(i % NTHREAD)
            .send(Some((source_path, module_name.clone())))
            .unwrap();
    }
    for chan in send_channels {
        chan.send(None).unwrap();
    }

    let mut closed_channels = 0;
    while closed_channels < NTHREAD {
        let maybe_msg = receive_processed.recv().unwrap();
        match maybe_msg {
            Some(ModuleMessage{module_name, name,doc,exports,imports}) => {
                let module_doc : String =
                    doc .clone()
                        .map(|x| x.chars().take_while(|&c| c != '\n').collect())
                        .unwrap_or_else(|| "No docstrings :(".to_owned());

                let module_exports = match exports {
                    tree::ExportList::Unqualified => "unqualified".to_owned(),
                    tree::ExportList::List(ref exports) => format!("{:?}", exports),
                };
                let module_imports = format!("{:?}", imports);
                println!("
                    #### {} ####
                    name: {:?}
                    exports: {}
                    has doc: {}
                    imports: {}
                    ", module_name, &name, module_exports,
                    module_doc,
                    module_imports
                );
            },
            None => closed_channels += 1,
        }
    }
}
