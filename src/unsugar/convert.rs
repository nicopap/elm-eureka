//! Convert an elm module into the proper structure for handling with
//! parsers.
use fxhash::{FxHashMap,FxHashSet};

use parser::{tree as parser, Parser};
use unsugar::{
    elmscope::ImportedEnv,
    interner::Interner,
    errors,
};

type ModuleEnv = FxHashMap<Vec<usize>,FxHashSet<usize>>;

/// Determine what symbols are imported in a module based on the imports
/// declarations and the modules reachable from that file.
fn import_env<'a,T,I>(
    interner: &mut Interner,
    environment: &ModuleEnv,
    imports: I,
) -> Result<ImportedEnv,errors::Module>
    where T: 'a,
          I: Iterator<Item=&'a parser::Import<String,T>>
{
    use self::parser::{ExportList, ExportEntry_ as EE};

    let mut imported_env = FxHashMap::default();

    for &parser::Import {ref global_name, ref local_name, ref exposes, ..}
    in imports {
        let module_path = interner.get_or_intern_path(global_name);
        let prefix_path : Vec<usize> = match local_name {
            &Some(ref name) => interner.get_or_intern_path(name),
            &None => module_path.clone(),
        };

        macro_rules! add_toimportenv {
            (qualified $name:expr) => ({
                let mut full_name = module_path.clone();
                full_name.push($name);
                let mut local_alias = prefix_path.clone();
                local_alias.push($name);
                imported_env.insert(local_alias, full_name);
            });
            (unqualified $name:expr) => ({
                if $name.contains('.') {
                    return Err(errors::Module::PathImport)
                }
                let name = interner.get_or_intern_name($name);
                let mut full_name = module_path.clone();
                full_name.push(name);
                imported_env.insert(vec![name], full_name);
            });
        }

        { use self::errors::Module::Inexistant;
        for name in environment.get(&module_path).ok_or(Inexistant)?.iter() {
            add_toimportenv!(qualified *name)
        }}

        match exposes {
            &Some(ExportList::List(ref entries)) => {
                for &(_,ref entry) in entries.iter() {
                    match entry {
                        &EE::Name(ref name) | &EE::Operator(ref name) => {
                            add_toimportenv!(unqualified name.as_str());
                        },
                        &EE::WithConstructors(ref name, ref constr) => {
                            add_toimportenv!(unqualified name.as_str());
                            for name in constr.iter() {
                                add_toimportenv!(unqualified name.as_str())
                            }
                        },
                        &EE::WithAllConstructors(_) => {
                            return Err(errors::Module::Unsuported);
                        },
                    }
                }
            },
            &Some(ExportList::Unqualified) => {
                let exposed_names = environment.get(&module_path)
                    .ok_or(errors::Module::Inexistant)?;
                for name in exposed_names.iter() {
                    let mut full_name = module_path.clone();
                    full_name.push(*name);
                    imported_env.insert(vec![*name], full_name);
                }
            },
            &None => {},
        };
    }
    Ok(imported_env)
}

#[cfg(test)] mod test { include!("convert-test.rs"); }
