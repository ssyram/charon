//! Hax-specific helpers for Rust programs. Those helpers are usually
//! no-ops when compiled normally but meaningful when compiled under
//! hax.
//!
//! # Example:
//!
//! ```rust
//! use hax_lib::*;
//! fn sum(x: Vec<u32>, y: Vec<u32>) -> Vec<u32> {
//!   hax_lib::assume!(x.len() == y.len());
//!   hax_lib::assert!(x.len() >= 0);
//!   hax_lib::assert_prop!(forall(|i: usize| implies(i < x.len(), x[i] < 4242)));
//!   hax_lib::debug_assert!(exists(|i: usize| implies(i < x.len(), x[i] > 123)));
//!   x.into_iter().zip(y.into_iter()).map(|(x, y)| x + y).collect()
//! }
//! ```

#![no_std]

#[cfg(feature = "macros")]
mod proc_macros;

// hax engine relies on `hax-lib` names: to avoid cluttering names with
// an additional `implementation` in all paths, we `include!` instead
// of doing conditional `mod` and `pub use`.

#[cfg(not(hax))]
core::include!("dummy.rs");
#[cfg(hax)]
core::include!("implementation.rs");
