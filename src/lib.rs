#![feature(type_ascription)]

mod parse_packages;
mod elm_versions;

use parse_packages::elm_package_info;
use std::path::Path;
use std::collections::HashMap;
use std::error::Error;

pub fn package_info(root_dir : &Path)
    -> Result<HashMap<String, Box<Path>>, Box<Error>>
{
    elm_package_info(root_dir)
}
