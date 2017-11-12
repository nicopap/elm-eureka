extern crate elm_eureka;

use std::path::Path;

pub fn main() {
    let packagepath = Path::new("examples/elm-spa-example");
    let sources = elm_eureka::package_info( packagepath).unwrap();

    print!("{:?}",sources);
}
