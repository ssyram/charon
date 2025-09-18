use crate::prelude::*;
use crate::sinto_todo;

/// Reflects [`rustc_span::Loc`]
#[derive_group(Serializers)]
#[derive(Clone, Debug, JsonSchema, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Loc {
    pub line: usize,
    pub col: usize,
}

/// Reflects [`rustc_span::Span`]
#[derive(::serde::Serialize, ::serde::Deserialize, Clone, Debug, JsonSchema, Eq, Ord)]
pub struct Span {
    pub lo: Loc,
    pub hi: Loc,
    pub filename: FileName,
    /// Original rustc span; can be useful for reporting rustc
    /// diagnostics (this is used in Charon)
    #[cfg(feature = "rustc")]
    #[serde(skip)]
    pub rust_span_data: Option<rustc_span::SpanData>,
    #[cfg(not(feature = "rustc"))]
    #[serde(skip)]
    pub rust_span_data: Option<()>,
}

const _: () = {
    // `rust_span_data` is a metadata that should *not* be taken into
    // account while hashing or comparing

    impl std::hash::Hash for Span {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.lo.hash(state);
            self.hi.hash(state);
            self.filename.hash(state);
        }
    }
    impl PartialEq for Span {
        fn eq(&self, other: &Self) -> bool {
            self.lo == other.lo && self.hi == other.hi && self.filename == other.filename
        }
    }

    impl PartialOrd for Span {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(
                self.lo.partial_cmp(&other.lo)?.then(
                    self.hi
                        .partial_cmp(&other.hi)?
                        .then(self.filename.partial_cmp(&other.filename)?),
                ),
            )
        }
    }
};

#[cfg(feature = "rustc")]
impl From<rustc_span::Loc> for Loc {
    fn from(val: rustc_span::Loc) -> Self {
        Loc {
            line: val.line,
            col: val.col_display,
        }
    }
}

#[cfg(feature = "rustc")]
impl<'tcx, S: BaseState<'tcx>> SInto<S, Span> for rustc_span::Span {
    fn sinto(&self, s: &S) -> Span {
        if let Some(span) = s.with_global_cache(|cache| cache.spans.get(self).cloned()) {
            return span;
        }
        let span = translate_span(*self, s.base().tcx.sess);
        s.with_global_cache(|cache| cache.spans.insert(*self, span.clone()));
        span
    }
}

/// Reflects [`rustc_span::source_map::Spanned`]
#[derive_group(Serializers)]
#[derive(Clone, Debug, JsonSchema)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}
#[cfg(feature = "rustc")]
impl<'s, S: UnderOwnerState<'s>, T: SInto<S, U>, U> SInto<S, Spanned<U>>
    for rustc_span::source_map::Spanned<T>
{
    fn sinto<'a>(&self, s: &S) -> Spanned<U> {
        Spanned {
            node: self.node.sinto(s),
            span: self.span.sinto(s),
        }
    }
}

impl<'tcx, S> SInto<S, PathBuf> for PathBuf {
    fn sinto(&self, _: &S) -> PathBuf {
        self.clone()
    }
}

/// Reflects [`rustc_span::RealFileName`]
#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[args(<S>, from: rustc_span::RealFileName, state: S as _s)]
pub enum RealFileName {
    LocalPath(PathBuf),
    Remapped {
        local_path: Option<PathBuf>,
        virtual_name: PathBuf,
    },
}

#[cfg(feature = "rustc")]
impl<S> SInto<S, u64> for rustc_hashes::Hash64 {
    fn sinto(&self, _: &S) -> u64 {
        self.as_u64()
    }
}

/// Reflects [`rustc_span::FileName`]
#[derive(AdtInto)]
#[args(<S>, from: rustc_span::FileName, state: S as gstate)]
#[derive_group(Serializers)]
#[derive(Clone, Debug, JsonSchema, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum FileName {
    Real(RealFileName),
    CfgSpec(u64),
    Anon(u64),
    MacroExpansion(u64),
    ProcMacroSourceCode(u64),
    CliCrateAttr(u64),
    Custom(String),
    // #[map(FileName::DocTest(x.0.to_str().unwrap().into()))]
    #[custom_arm(FROM_TYPE::DocTest(x, _) => TO_TYPE::DocTest(x.to_str().unwrap().into()),)]
    DocTest(String),
    InlineAsm(u64),
}

impl FileName {
    pub fn to_string(&self) -> String {
        match self {
            Self::Real(RealFileName::LocalPath(path))
            | Self::Real(RealFileName::Remapped {
                local_path: Some(path),
                ..
            })
            | Self::Real(RealFileName::Remapped {
                virtual_name: path, ..
            }) => format!("{}", path.display()),
            _ => format!("{:?}", self),
        }
    }
    pub fn to_path(&self) -> Option<&std::path::Path> {
        match self {
            Self::Real(RealFileName::LocalPath(path))
            | Self::Real(RealFileName::Remapped {
                local_path: Some(path),
                ..
            })
            | Self::Real(RealFileName::Remapped {
                virtual_name: path, ..
            }) => Some(path),
            _ => None,
        }
    }
}

sinto_todo!(rustc_span, ErrorGuaranteed);
