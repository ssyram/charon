use super::Diagnostics;
use annotate_snippets::*;
use miette::SourceOffset;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;

/// A context for reporting diagnostics
#[derive(Clone, Debug, Default)]
pub struct ReportCtx {
    files: HashMap<PathBuf, Rc<String>>,
}

/// Translates a line and column position into an absolute offset
fn compute_offset(src: &str, line: usize, col: usize) -> usize {
    SourceOffset::from_location(src, line, col).offset() + 1
}

impl ReportCtx {
    /// Read the contents of a file. The result is cached.
    fn file_contents<'a>(&'a mut self, path: PathBuf) -> Rc<String> {
        self.files
            .entry(path.clone())
            .or_insert_with(|| {
                let s =
                    std::fs::read_to_string(&path).expect(&format!("Unable to read file {path:?}"));
                Rc::new(s)
            })
            .clone()
    }
}

impl Diagnostics {
    /// Converts a `Diagnostics` to a `annotate_snippets::Message`,
    /// which can be accessed via `then`, a callback function.
    pub fn with_message<R, F: for<'a> FnMut(Message<'a>) -> R>(
        &self,
        report_ctx: &mut ReportCtx,
        working_dir: &Path,
        level: Level,
        mut then: F,
    ) -> R {
        let mut snippets_data = vec![];

        for span in &self.span {
            if let Some(path) = span.filename.to_path() {
                let source = {
                    let mut path = path.to_path_buf();
                    if path.is_relative() {
                        path = working_dir.join(&path);
                    };
                    report_ctx.file_contents(path)
                };
                let start = compute_offset(&source, span.lo.line, span.lo.col);
                let end = compute_offset(&source, span.hi.line, span.hi.col);
                let origin = format!("{}", path.display());
                snippets_data.push((source, origin, start..end));
            };
        }

        let title = format!("[{}] {self}", self.kind.code());
        let message =
            level
                .title(&title)
                .snippets(snippets_data.iter().map(|(source, origin, range)| {
                    Snippet::source(source)
                        .line_start(1)
                        .origin(&origin)
                        .fold(true)
                        .annotation(level.span(range.clone()))
                }));

        then(message)
    }
}
