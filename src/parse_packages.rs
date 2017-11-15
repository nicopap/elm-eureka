extern crate serde_json;

use std::fs::{File, read_dir, DirEntry};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::error::Error;
use std::fmt;
use std::iter::FromIterator;

use self::serde_json::{Value, from_reader};

use elm_versions::ElmVersion;

#[derive(Debug)]
struct PackageInfo {
    project_dir : Box<Path>,
    dependencies: Vec<Box<Path>>,
    source_dirs: Vec<Box<Path>>,
    elm_version: ElmVersion
}

#[derive(Debug)]
enum PackageError {
    InvalidDependencyFormat,
}
impl fmt::Display for PackageError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "PackageError")
    }
}
impl Error for PackageError {
    fn description(&self) -> &str {
        match self {
            &PackageError::InvalidDependencyFormat =>
                "Couldn't parse dependencies specification in elm-stuff/exact-dependencies.json"
            ,
        }
    }
}

fn extract_dependencies(value : &Value)
    -> Result<Vec<Box<Path>>,PackageError>
{
    value["dependencies"]
        .as_object()
        .map(|object| { object
                .keys()
                .map(|x| Path::new(x).to_path_buf().into_boxed_path() )
                .collect()
        })
        .ok_or(PackageError::InvalidDependencyFormat)
}

fn extract_source_files(value : &Value) -> Vec<Box<Path>>
{
    value["source-directories"]
        .as_array()
        .map(|array| { array
                .into_iter()
                .map(|x| x.as_str().unwrap())
                .map(|x| Path::new(x).to_path_buf().into_boxed_path())
                .collect()
            })
        .unwrap_or(Vec::new())
}

// Extracts from JSON Value the exposed modules
fn extract_exposed(value : Value) -> Vec<String>
{
    value["exposed-modules"]
        .as_array()
        .unwrap()
        .into_iter()
        .map(|x| String::from(x.as_str().unwrap()))
        .collect()
}

pub fn elm_package_info(root_dir : &Path)
    -> Result<HashMap<String, Box<Path>>, Box<Error>>
{
    let project_dir = root_dir.to_path_buf().into_boxed_path();

    let dep_file = root_dir.join("elm-package.json");
    let file = File::open(dep_file)?;
    let value : Value = from_reader(file)?;

    let dependencies = extract_dependencies(&value)?;

    let source_dirs = extract_source_files(&value);

    let package_infos = PackageInfo {
        dependencies,
        source_dirs,
        project_dir,
        elm_version : ElmVersion::Elm018
    };

    source_files(package_infos)
}

// Returns the path to the latest version in the given directory
// panics if there is no directories present in given path, or if the
// path is invalid.
// TODO: fn latest_version_in_dir


// Returns Vec of the modules present in the directory: a tuple of
// the module name and the location
fn all_packages(dir :&Path)
    -> Result<Vec<(String, Box<Path>)>, Box<Error>>
{
    all_packages_helper(dir, dir)
}

fn all_packages_helper(dir : &Path, root : &Path)
    -> Result<Vec<(String, Box<Path>)>, Box<Error>>
{
    read_dir(dir)?
        .map(|x| {
            let entry : DirEntry = x?;
            let path : PathBuf = entry.path().to_path_buf();
            if entry.metadata()?.is_file() {
                let module_name = String::from(
                    path
                        .strip_prefix(root)?
                        .to_string_lossy()
                        .replace('/',".")
                        .trim_right_matches(".elm")
                );
                Ok(vec![ (module_name, path.into_boxed_path()) ])
            } else {
                all_packages_helper(&path, root)
            }
        } : Result<Vec<(String, Box<Path>)>, Box<Error>> )
        .collect::< Vec<Result<_,_>> >()
        .into_iter()
        .collect::< Result<Vec<_>,_> >()
        .map(|x|  x.into_iter().flat_map(|x| x).collect() )
}

// An HashMap which keys are elm module names avaliable in the global name
// space, and entries (values) are the path to their source code.
// The globaly available modules are the ones that the explicitely declared
// dependencies exports AND the one in the various source paths.
fn source_files(infos : PackageInfo)
    -> Result<HashMap<String, Box<Path>>, Box<Error>>
{
    let project_dir = infos.project_dir;
    let foreign_dir = project_dir.join("elm-stuff/packages");

    let foreign_modules =
        infos.dependencies
            .into_iter()
            .map(|x| foreign_dir.join(x))
            .map(|ref x| last_version(x))
            .flat_map(|ref x|  all_exposed_modules(x));

    let local_modules : Vec<(String, Box<Path>)> =
        infos.source_dirs
            .into_iter()
            .map(move |x| project_dir.join(x))
            .flat_map(|ref x| all_packages(x).unwrap().into_iter())
            .collect();

    Ok(HashMap::from_iter(foreign_modules.chain(local_modules)))
}

// In a directory `path` containing directories which names are based on
// semantic versioning, return the path to the directory representing the
// latest release.
fn last_version(path : &Path) -> Box<Path>
{
    read_dir(path)
        .unwrap()
        .last().unwrap().unwrap()
        .path()
        .into_boxed_path()
}

// With given project directory, returns a list of tuples associating
// elm exposed module names with the file in which they were implemented.
fn all_exposed_modules(project_root : &Path)
    -> Vec<(String, Box<Path>)>
{
    let file = File::open(project_root.join("elm-package.json")).unwrap();
    let value : Value = from_reader(file).unwrap();

    extract_exposed(value)
        .into_iter()
        .map(move |exposed| {
            let exposed_copy = exposed.clone();
            let mut root_copy = project_root.clone().join("src");
            root_copy = root_copy.join(exposed_copy.replace('.',"/"));
            root_copy.set_extension("elm");

            (exposed_copy, root_copy.into_boxed_path())
        })
        .collect()
}
