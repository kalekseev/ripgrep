/*!
doc
*/
use std::cell::RefCell;
use std::env;
use std::fs::File;
use std::io::{self, Read, BufRead};
use std::path::{PathBuf};
use std::str;
use std::sync::Arc;
use std::path::Path;

use globset::{GlobSetBuilder, GlobSet};
use regex::bytes::Regex;
use thread_local::ThreadLocal;

use {Error, PartialErrorBuilder};
use pathutil::{strip_prefix};


/// doc
#[derive(Clone, Debug)]
pub struct Glob {
    from: Option<PathBuf>,
    original: String,
    actual: String,
    is_only_dir: bool,
}


/// doc
#[derive(Clone, Debug)]
pub struct Hgignore {
    set: GlobSet,
    root: PathBuf,
    globs: Vec<Glob>,
    num_ignores: u64,
    matches: Arc<ThreadLocal<RefCell<Vec<usize>>>>,
}

impl Hgignore {
    /// doc
    pub fn new<P: AsRef<Path>>(
        hgignore_path: P,
    ) -> (Hgignore, Option<Error>) {
        let path = hgignore_path.as_ref();
        let parent = path.parent().unwrap_or(Path::new("/"));
        let mut builder = HgignoreBuilder::new(parent);
        let mut errs = PartialErrorBuilder::default();
        errs.maybe_push_ignore_io(builder.add(path));
        match builder.build() {
            Ok(gi) => (gi, errs.into_error_option()),
            Err(err) => {
                errs.push(err);
                (Hgignore::empty(), errs.into_error_option())
            }
        }
    }

    /// doc
    pub fn global() -> (Hgignore, Option<Error>) {
        match hgrc_ignore_path() {
            None => (Hgignore::empty(), None),
            Some(path) => {
                if !path.is_file() {
                    (Hgignore::empty(), None)
                } else {
                    Hgignore::new(path)
                }
            }
        }
    }

    /// doc
    pub fn empty() -> Hgignore {
        HgignoreBuilder::new("").build().unwrap()
    }
}

/// doc
pub struct HgignoreBuilder {
    builder: GlobSetBuilder,
    root: PathBuf,
    globs: Vec<Glob>,
}

impl HgignoreBuilder {
    /// doc
    pub fn new<P: AsRef<Path>>(root: P) -> HgignoreBuilder {
        let root = root.as_ref();
        HgignoreBuilder {
            builder: GlobSetBuilder::new(),
            root: strip_prefix("./", root).unwrap_or(root).to_path_buf(),
            globs: vec![],
        }
    }

    /// doc
    pub fn build(&self) -> Result<Hgignore, Error> {
        let nignore = self.globs.iter().count();
        let set = try!(
            self.builder.build().map_err(|err| {
                Error::Glob {
                    glob: None,
                    err: err.to_string(),
                }
            }));
        Ok(Hgignore {
            set: set,
            root: self.root.clone(),
            globs: self.globs.clone(),
            num_ignores: nignore as u64,
            matches: Arc::new(ThreadLocal::default()),
        })
    }

    /// doc
    pub fn add<P: AsRef<Path>>(&mut self, path: P) -> Option<Error> {
        let path = path.as_ref();
        let file = match File::open(path) {
            Err(err) => return Some(Error::Io(err).with_path(path)),
            Ok(file) => file,
        };
        let rdr = io::BufReader::new(file);
        let mut errs = PartialErrorBuilder::default();
        for (i, line) in rdr.lines().enumerate() {
            let lineno = (i + 1) as u64;
            let line = match line {
                Ok(line) => line,
                Err(err) => {
                    errs.push(Error::Io(err).tagged(path, lineno));
                    break;
                }
            };
            if let Err(err) = self.add_line(Some(path.to_path_buf()), &line) {
                errs.push(err.tagged(path, lineno));
            }
        }
        errs.into_error_option()
    }

    /// doc
    pub fn add_line(
        &mut self,
        from: Option<PathBuf>,
        mut line: &str
    ) -> Result<&mut HgignoreBuilder, Error> {
        Ok(self)
    }
}

fn hgrc_ignore_path() -> Option<PathBuf> {
    hgrc_contents()
        .and_then(|data| parse_excludes_file(&data))
}

fn hgrc_contents() -> Option<Vec<u8>> {
    let home = match env::var_os("HOME") {
        None => return None,
        Some(home) => PathBuf::from(home),
    };
    let mut file = match File::open(home.join(".hgrc")) {
        Err(_) => return None,
        Ok(file) => io::BufReader::new(file),
    };
    let mut contents = vec![];
    file.read_to_end(&mut contents).ok().map(|_| contents)
}


fn parse_excludes_file(data: &[u8]) -> Option<PathBuf> {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"(?ium)^\s*ignore\s*=\s*(.+)\s*$").unwrap();
    };
    let caps = match RE.captures(data) {
        None => return None,
        Some(caps) => caps,
    };
    str::from_utf8(&caps[1]).ok().map(|s| PathBuf::from(expand_tilde(s)))
}

/// Expands ~ in file paths to the value of $HOME.
fn expand_tilde(path: &str) -> String {
    let home = match env::var("HOME") {
        Err(_) => return path.to_string(),
        Ok(home) => home,
    };
    path.replace("~", &home)
}
