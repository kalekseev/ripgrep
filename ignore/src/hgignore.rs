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

use globset::{Candidate, GlobSetBuilder, GlobSet, GlobBuilder};
use regex::bytes::{Regex, RegexSet};
use thread_local::ThreadLocal;

use {Error, Match, PartialErrorBuilder};
use pathutil::{is_file_name, strip_prefix};


/// doc
#[derive(Clone, Debug)]
pub enum Pattern {
    /// doc
    Glob {
        /// doc
        from: Option<PathBuf>,
        /// doc
        original: String,
        /// doc
        actual: String,
        /// doc
        is_only_dir: bool
    },
    /// doc
    Regex {
        /// doc
        from: Option<PathBuf>,
        /// doc
        original: String,
    }
}

impl Pattern {
    /// Returns the file path that defined this glob.
    pub fn from(&self) -> Option<&Path> {
        match self {
            &Pattern::Glob { ref from, .. } => from.as_ref().map(|p| &**p),
            &Pattern::Regex { ref from, .. } => from.as_ref().map(|p| &**p),
        }
    }

    /// The original pattern as it was defined in a hgignore file.
    pub fn original(&self) -> &str {
        match self {
            &Pattern::Glob { ref original, .. } => &original,
            &Pattern::Regex { ref original, .. } => &original,
        }
    }

    /// The actual pattern that was compiled to respect hgignore
    /// semantics.
    pub fn actual(&self) -> &str {
        match self {
            &Pattern::Glob { ref actual, .. } => &actual,
            &Pattern::Regex { ref original, .. } => &original,
        }
    }

    /// Whether this pattern must match a directory or not.
    pub fn is_only_dir(&self) -> bool {
        match self {
            &Pattern::Glob { is_only_dir, .. } => is_only_dir,
            _ => false,
        }
    }
}


/// doc
#[derive(Clone, Debug)]
pub struct Hgignore {
    set: GlobSet,
    regex_set: RegexSet,
    root: PathBuf,
    globs: Vec<Pattern>,
    regexes: Vec<Pattern>,
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
            Ok(hgi) => (hgi, errs.into_error_option()),
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

    /// Returns true if and only if this hgignore has zero patterns, and
    /// therefore never matches any file path.
    pub fn is_empty(&self) -> bool {
        self.set.is_empty() && self.regex_set.len() == 0
    }

    /// Returns whether the given file path matched a pattern in this hgignore
    /// matcher.
    ///
    /// `is_dir` should be true if the path refers to a directory and false
    /// otherwise.
    ///
    /// The given path is matched relative to the path given when building
    /// the matcher. Specifically, before matching `path`, its prefix (as
    /// determined by a common suffix of the directory containing this
    /// hgignore) is stripped. If there is no common suffix/prefix overlap,
    /// then `path` is assumed to be relative to this matcher.
    pub fn matched<P: AsRef<Path>>(
        &self,
        path: P,
        is_dir: bool,
    ) -> Match<&Pattern> {
        if self.is_empty() {
            return Match::None;
        }
        self.matched_stripped(self.strip(path.as_ref()), is_dir)
    }

    /// Like matched, but takes a path that has already been stripped.
    fn matched_stripped<P: AsRef<Path>>(
        &self,
        path: P,
        is_dir: bool,
    ) -> Match<&Pattern> {
        if !self.set.is_empty() {
            let path = path.as_ref();
            let candidate = Candidate::new(path);
            let _matches = self.matches.get_default();
            let mut matches = _matches.borrow_mut();
            self.set.matches_candidate_into(&candidate, &mut *matches);
            for &i in matches.iter().rev() {
                let glob = &self.globs[i];
                if !glob.is_only_dir() || is_dir {
                    return Match::Ignore(glob)
                }
            }
        }
        if self.regex_set.len() != 0 {
            let path = path.as_ref();
            let candidate = Candidate::new(path);
            let re_matches = self.regex_set.matches(&*candidate.path);
            match re_matches.iter().next() {
                Some(i) => return Match::Ignore(&self.regexes[i]),
                None => return Match::None,
            };
        }
        Match::None
    }

    /// Strips the given path such that it's suitable for matching with this
    /// hgignore matcher.
    fn strip<'a, P: 'a + AsRef<Path> + ?Sized>(
        &'a self,
        path: &'a P,
    ) -> &'a Path {
        let mut path = path.as_ref();
        // A leading ./ is completely superfluous. We also strip it from
        // our hgignore root path, so we need to strip it from our candidate
        // path too.
        if let Some(p) = strip_prefix("./", path) {
            path = p;
        }
        // Strip any common prefix between the candidate path and the root
        // of the hgignore, to make sure we get relative matching right.
        // BUT, a file name might not have any directory components to it,
        // in which case, we don't want to accidentally strip any part of the
        // file name.
        if !is_file_name(path) {
            if let Some(p) = strip_prefix(&self.root, path) {
                path = p;
                // If we're left with a leading slash, get rid of it.
                if let Some(p) = strip_prefix("/", path) {
                    path = p;
                }
            }
        }
        path
    }
}

/// doc
#[derive(Copy, Clone, Debug)]
pub enum HgignoreSyntax {
    /// doc
    Glob,
    /// doc
    Regexp,
}

/// doc
pub struct HgignoreBuilder {
    builder: GlobSetBuilder,
    root: PathBuf,
    regex_lines: Vec<Pattern>,
    globs: Vec<Pattern>,
}

impl HgignoreBuilder {
    /// doc
    pub fn new<P: AsRef<Path>>(root: P) -> HgignoreBuilder {
        let root = root.as_ref();
        HgignoreBuilder {
            builder: GlobSetBuilder::new(),
            root: strip_prefix("./", root).unwrap_or(root).to_path_buf(),
            regex_lines: vec![],
            globs: vec![],
        }
    }

    /// doc
    pub fn build(&self) -> Result<Hgignore, Error> {
        let set = try!(
            self.builder.build().map_err(|err| {
                Error::Glob {
                    glob: None,
                    err: err.to_string(),
                }
            }));
        let rls: Vec<&str> = self.regex_lines
            .iter()
            .map(|ref x| x.original())
            .collect();
        let nignore = self.globs.iter().count() + rls.iter().count();
        return Ok(Hgignore {
            set: set,
            root: self.root.clone(),
            globs: self.globs.clone(),
            regex_set: RegexSet::new(rls).unwrap(),
            regexes: self.regex_lines.clone(),
            num_ignores: nignore as u64,
            matches: Arc::new(ThreadLocal::default()),
        });
    }

    /// doc
    pub fn add<P: AsRef<Path>>(&mut self, path: P) -> Option<Error> {
        lazy_static! {
            static ref SYNTAX_SWITCH: Regex = Regex::new(
                r"^syntax:\s*(regexp|glob)\s*$").unwrap();
        };
        let mut current_syntax = HgignoreSyntax::Regexp;
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
            match SYNTAX_SWITCH.captures(line.as_bytes()) {
                None => {
                    if let Err(err) = self.add_line(
                        Some(path.to_path_buf()), &line, current_syntax
                    ) {
                        errs.push(err.tagged(path, lineno));
                    };
                },
                Some(caps) => {
                    current_syntax = match &caps[1] {
                        b"glob" => HgignoreSyntax::Glob,
                        _ => HgignoreSyntax::Regexp,
                    }
                },
            };
        }
        errs.into_error_option()
    }

    /// doc
    pub fn add_line(
        &mut self,
        from: Option<PathBuf>,
        mut line: &str,
        current_syntax: HgignoreSyntax
    ) -> Result<&mut HgignoreBuilder, Error> {
        if line.starts_with("#") {
            return Ok(self);
        }
        line = line.trim_right();
        if line.is_empty() {
            return Ok(self);
        }
        // TODO: extended patterns support, see hg help patterns
        lazy_static! {
            static ref EXT_PATTERN: Regex = Regex::new(r"^(\w+):").unwrap();
        };
        match EXT_PATTERN.captures(line.as_bytes()) {
            Some(caps) => {
                match &caps[1] {
                    b"path" => debug!("HG pattern \"path:\" is not supported."),
                    b"rootfilesin" => debug!("HG pattern \"rootfilesin:\" is not supported."),
                    b"glob" => debug!("HG pattern \"glob:\" is not supported."),
                    b"re" => debug!("HG pattern \"re:\" is not supported."),
                    b"listfile" => debug!("HG pattern \"listfile:\" is not supported."),
                    b"listfile0" => debug!("HG pattern \"listfile0:\" is not supported."),
                    b"include" => debug!("HG pattern \"include:\" is not supported."),
                    b"subinclude" => debug!("HG pattern \"subinclude:\" is not supported."),
                    _ => {
                        return match current_syntax {
                            HgignoreSyntax::Glob => self.add_glob_line(from, line),
                            HgignoreSyntax::Regexp => self.add_regex_line(from, line),
                        }
                    },
                }
                return Ok(self);
            },
            None => {
                match current_syntax {
                    HgignoreSyntax::Glob => self.add_glob_line(from, line),
                    HgignoreSyntax::Regexp => self.add_regex_line(from, line),
                }
            },
        }
    }

    /// doc
    pub fn add_regex_line(
        &mut self,
        from: Option<PathBuf>,
        line: &str,
    ) -> Result<&mut HgignoreBuilder, Error> {
        // FIXME: validate regex otherwise RegexSet build will
        // fail. So we have to create regexes twice:
        // during validation and during RegexSet build.
        // Another option is to use list of regexes instead of RegexSet
        self.regex_lines.push(Pattern::Regex {
            from: from,
            original: line.to_string(),
        });
        Ok(self)
    }

    /// doc
    pub fn add_glob_line(
        &mut self,
        from: Option<PathBuf>,
        mut line: &str,
    ) -> Result<&mut HgignoreBuilder, Error> {
        let mut actual;
        let mut is_only_dir = false;
        let original = line.to_string();

        let mut literal_separator = false;
        let has_slash = line.chars().any(|c| c == '/');
        let mut is_absolute = false;
        if line.starts_with("\\#") {
            line = &line[1..];
            is_absolute = line.chars().nth(0) == Some('/');
        } else {
            // FIXME: this is not the case for hgignore
            if line.starts_with("/") {
                // `man gitignore` says that if a glob starts with a slash,
                // then the glob can only match the beginning of a path
                // (relative to the location of gitignore). We achieve this by
                // simply banning wildcards from matching /.
                literal_separator = true;
                line = &line[1..];
                is_absolute = true;
            }
        }
        // If it ends with a slash, then this should only match directories,
        // but the slash should otherwise not be used while globbing.
        if let Some((i, c)) = line.char_indices().rev().nth(0) {
            if c == '/' {
                is_only_dir = true;
                line = &line[..i];
            }
        }
        // If there is a literal slash, then we note that so that globbing
        // doesn't let wildcards match slashes.
        actual = line.to_string();
        if has_slash {
            literal_separator = true;
        }
        // If there was a leading slash, then this is a glob that must
        // match the entire path name. Otherwise, we should let it match
        // anywhere, so use a **/ prefix.
        if !is_absolute {
            // ... but only if we don't already have a **/ prefix.
            if !actual.starts_with("**/") {
                actual = format!("**/{}", actual);
            }
        }
        // If the glob ends with `/**`, then we should only match everything
        // inside a directory, but not the directory itself. Standard globs
        // will match the directory. So we add `/*` to force the issue.
        if actual.ends_with("/**") {
            actual = format!("{}/*", actual);
        }
        let parsed = try!(
            GlobBuilder::new(&actual)
                .literal_separator(literal_separator)
                .build()
                .map_err(|err| {
                    Error::Glob {
                        glob: Some(original.clone()),
                        err: err.kind().to_string(),
                    }
                }));
        self.builder.add(parsed);
        self.globs.push(Pattern::Glob {
            from: from,
            original: original,
            actual: actual,
            is_only_dir: is_only_dir,
        });
        Ok(self)

    }
}

fn hgrc_ignore_path() -> Option<PathBuf> {
    hgrc_contents()
        .and_then(|data| parse_ignore_file(&data))
}

/// Provides paths to per-user mercurial configs
/// TODO: return $HOME/Mercurial.ini only on windows
fn per_user_configs() -> Vec<PathBuf> {
    let configs = [".hgrc", "Mercurial.ini"];
    let mut result = vec![];
    if let Some(home) = env::home_dir() {
        for config in configs.iter() {
            let mut chome = home.clone();
            chome.push(config);
            result.push(chome);
        }
    }
    result
}

fn hgrc_contents() -> Option<Vec<u8>> {
    per_user_configs()
        .iter()
        .flat_map(|c| File::open(c).ok())
        .map(|f| io::BufReader::new(f))
        .flat_map(|mut br| {
            let mut contents = vec![];
            br.read_to_end(&mut contents).ok().map(|_| contents)
        })
        .next()
}

// TODO: parse only [ui] section
fn parse_ignore_file(data: &[u8]) -> Option<PathBuf> {
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

/// Expands `~/` in file paths to the value of $HOME.
fn expand_tilde(path: &str) -> String {
    // TODO: we don't handle `~username` shortcuts
    let expanded_path = if path.starts_with("~/") {
        env::home_dir().map(|home| path.replacen("~", home.to_str().unwrap(), 1))
    } else {
        None
    };
    expanded_path.unwrap_or(path.to_string())
}
