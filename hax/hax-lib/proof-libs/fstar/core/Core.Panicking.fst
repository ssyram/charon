module Core.Panicking

open Rust_primitives
open Rust_primitives.Hax

type t_AssertKind = | AssertKind_Eq

let explicit_panic (_: unit {False}): t_Never
  = match () with

let panic (message: string {False}): t_Never
  = match () with

let panic_fmt (fmt: Core.Fmt.t_Arguments {False}): t_Never
  = match () with
