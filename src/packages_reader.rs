//! Query informations about an elm project

use std::fs::{File, read_dir};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::error::Error;
use itertools::Itertools;

use walkdir::WalkDir;
use serde_json::{Value, from_reader};
use serde_json::map::Map;

#[derive(Debug)]
pub struct PackageInfo {
    /// The path to the root of the elm project (in fact,
    /// this is where the elm-package.json is located)
    pub project_dir : PathBuf,
    /// A map of module names to the location of the name
    /// relative to the root of the project.
    pub dependencies: HashMap<String, PathBuf>,
    /// the source files of the elm package, paths are relative to the
    /// project root
    pub source_files: HashMap<String, PathBuf>,
}

// Shortcut to cut some syntax cruft
macro_rules! strip_x {
    ($path_name:expr, $x:expr) => (
        $path_name.strip_prefix(&$x).unwrap().to_path_buf()
    )
}

/// Retreive from the elm project present in `root_dir`
/// informations about dependencies.
///
/// Returns relevent informations on the package.
///
/// # Panics
/// This function may panic, typically if there is
/// an IO read error.
pub fn info(root_dir : &Path) -> Result<PackageInfo, Box<Error>> {
    let project_dir = root_dir.to_path_buf();
    let foreign_dir = project_dir.join("elm-stuff/packages");
    let dep_file = root_dir.join("elm-package.json");
    let file = File::open(dep_file)?;
    let value : Value = from_reader(file)?;
    let dependencies =
        value["dependencies"]
            .as_object()
            .unwrap_or(&Map::new())
            .keys()
            .map(|x| Path::new(x).to_path_buf())
            .map(|x| foreign_dir.join(x))
            .map(|ref x| last_version(x))
            .flat_map(|ref x|  all_exposed_modules(x))
            .map(|(n,p)| (n, strip_x!(p, project_dir)))
            .collect::<HashMap<String, PathBuf>>();

    let source_files =
        value["source-directories"]
            .as_array()
            .unwrap_or(&Vec::new())
            .into_iter()
            .map(|x| PathBuf::from(x.as_str().expect("")))
            .flat_map(|ref source_subdir|
                all_modules(&project_dir.join(source_subdir))
                    .into_iter()
                    .map(|(module_name,source)|
                        (module_name, strip_x!(source, project_dir))
                    )
            )
            .collect::<HashMap<String, PathBuf>>();

    Ok(PackageInfo {dependencies, source_files, project_dir})
}

// Returns Vec of the modules present in the directory: a tuple of
// the module name and the location.
//
// The paths are prefixed by `dir`
fn all_modules(dir :&Path) -> Vec<(String, PathBuf)> {
    WalkDir::new(dir)
        .into_iter()
        .filter_map(Result::ok)
        .filter_map(|entry| {
            let entry_path : PathBuf = entry.path().to_path_buf();
            if entry.metadata().ok()?.is_file() { // Is module source file
                let module_name = String::from(
                    entry_path
                        .strip_prefix(dir).unwrap() //All subdirectories of `dir` should start with `dir`
                        .to_string_lossy()
                        .replace('/',".")
                        .trim_right_matches(".elm")
                );
                Some(vec![( module_name, entry_path)])
            } else { None }
        })
        .flatten().collect()
}

// In a directory `path` containing directories which names are based on
// semantic versioning. Returns one of them (may panic, may not be the last
// version).
fn last_version(path : &Path) -> PathBuf {
    read_dir(path).expect("").last().expect("").expect("").path()
}

// With given project directory, returns a list of tuples associating
// elm exposed module names with the file in which they were implemented.
fn all_exposed_modules(project_root : &Path) -> Vec<(String, PathBuf)> {
    let file = File::open(project_root.join("elm-package.json")).expect("");
    let value : Value = from_reader(file).expect("");

    value["exposed-modules"]
        .as_array()
        .expect("")
        .into_iter()
        .map(|x| String::from(x.as_str().expect("")))
        .map(move |exposed| {
            let exposed_copy = exposed.clone();
            let mut root_copy = project_root.to_owned().join("src");
            root_copy = root_copy.join(exposed_copy.replace('.',"/"));
            root_copy.set_extension("elm");

            (exposed_copy, root_copy)
        })
        .collect()
}
