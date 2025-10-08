#![allow(rustdoc::private_intra_doc_links)]
#![cfg_attr(feature = "rustc", feature(if_let_guard))]
#![cfg_attr(feature = "rustc", feature(macro_metavar_expr))]
#![cfg_attr(feature = "rustc", feature(rustc_private))]
#![cfg_attr(feature = "rustc", feature(sized_hierarchy))]
#![cfg_attr(feature = "rustc", feature(trait_alias))]
#![cfg_attr(feature = "rustc", feature(type_changing_struct_update))]

macro_rules! cfg_feature_rustc {
    ($($item:item)*) => {
        $(
            #[cfg(feature = "rustc")]
            $item
        )*
    }
}

cfg_feature_rustc! {
    // When the feature `rustc` is enabled, we enable the bridges
    // between rustc ASTs, which are defined in the crates
    // `rustc_*`. We thus need to import them with `extern crate
    // rustc_*`
    extern crate rustc_abi;
    extern crate rustc_ast;
    extern crate rustc_ast_pretty;
    extern crate rustc_attr_data_structures;
    extern crate rustc_apfloat;
    extern crate rustc_const_eval;
    extern crate rustc_data_structures;
    extern crate rustc_driver;
    extern crate rustc_hashes;
    extern crate rustc_errors;
    extern crate rustc_hir;
    extern crate rustc_hir_analysis;
    extern crate rustc_index;
    extern crate rustc_infer;
    extern crate rustc_interface;
    extern crate rustc_middle;
    extern crate rustc_mir_build;
    extern crate rustc_session;
    extern crate rustc_span;
    extern crate rustc_target;
    extern crate rustc_trait_selection;
    extern crate rustc_type_ir;
    extern crate rustc_lexer;

    mod rustc_utils;
    pub mod state;
    mod utils;
    mod deterministic_hash;
    pub mod comments;
}

mod body;
mod constant_utils;
pub mod id_table;
mod types;

mod index_vec;
mod prelude;

pub use hax_frontend_exporter_options as options;
pub use prelude::*;

mod sinto;
mod traits;

pub use hax_adt_into::AdtInto;
pub use sinto::SInto;
