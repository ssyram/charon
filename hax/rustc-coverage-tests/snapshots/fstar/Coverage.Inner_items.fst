module Coverage.Inner_items
#set-options "--fuel 0 --ifuel 1 --z3rlimit 15"
open Core
open FStar.Mul

let _ =
  (* This module has implicit dependencies, here we make them explicit. *)
  (* The implicit dependencies arise from typeclasses instances. *)
  let open Std.Env in
  ()

let main__t_in_mod__v_IN_MOD_CONST: u32 = mk_u32 1000

let main__in_func (a: u32) : Prims.unit =
  let b:u32 = mk_u32 1 in
  let c:u32 = a +! b in
  let _:Prims.unit =
    Std.Io.Stdio.e_print (Core.Fmt.Rt.impl_2__new_v1 (mk_usize 2)
          (mk_usize 1)
          (let list = ["c = "; "\n"] in
            FStar.Pervasives.assert_norm (Prims.eq2 (List.Tot.length list) 2);
            Rust_primitives.Hax.array_of_list 2 list)
          (let list = [Core.Fmt.Rt.impl__new_display #u32 c <: Core.Fmt.Rt.t_Argument] in
            FStar.Pervasives.assert_norm (Prims.eq2 (List.Tot.length list) 1);
            Rust_primitives.Hax.array_of_list 1 list)
        <:
        Core.Fmt.t_Arguments)
  in
  ()

type main__t_InStruct = { main__f_in_struct_field:u32 }

let main__v_IN_CONST: u32 = mk_u32 1234

(* item error backend: (reject_TraitItemDefault) ExplicitRejection { reason: "a node of kind [Trait_item_default] have been found in the AST" }
Last available AST for this item:

#[<cfg>(any(feature = "json"))]#[allow(unused_assignments, unused_variables, dead_code)]#[feature(coverage_attribute)]#[allow(unused_attributes)]#[allow(dead_code)]#[allow(unreachable_code)]#[feature(register_tool)]#[register_tool(_hax)]trait main__t_InTrait<Self_>{#[_hax::json("\"TraitMethodNoPrePost\"")]fn main__f_trait_func_pre(_: Self,_: int) -> bool;
#[_hax::json("\"TraitMethodNoPrePost\"")]fn main__f_trait_func_post(_: Self,_: int,_: Self) -> bool;
fn main__f_trait_func(_: Self,_: int) -> Self;
fn main__f_default_trait_func((self: Self)) -> Self{{let _: tuple0 = {coverage::inner_items::main__in_func(coverage::inner_items::main__v_IN_CONST)};{let self: Self = {coverage::inner_items::main__f_trait_func(self,coverage::inner_items::main__v_IN_CONST)};self}}}}

Last AST:
/** print_rust: pitem: not implemented  (item: { Concrete_ident.T.def_id =
  { Explicit_def_id.T.is_constructor = false;
    def_id =
    { Types.index = (0, 0, None); is_local = true; kind = Types.Trait;
      krate = "coverage";
      parent =
      (Some { Types.contents =
              { Types.id = 0;
                value =
                { Types.index = (0, 0, None); is_local = true;
                  kind = Types.Fn; krate = "coverage";
                  parent =
                  (Some { Types.contents =
                          { Types.id = 0;
                            value =
                            { Types.index = (0, 0, None); is_local = true;
                              kind = Types.Mod; krate = "coverage";
                              parent =
                              (Some { Types.contents =
                                      { Types.id = 0;
                                        value =
                                        { Types.index = (0, 0, None);
                                          is_local = true; kind = Types.Mod;
                                          krate = "coverage"; parent = None;
                                          path = [] }
                                        }
                                      });
                              path =
                              [{ Types.data = (Types.TypeNs "inner_items");
                                 disambiguator = 0 }
                                ]
                              }
                            }
                          });
                  path =
                  [{ Types.data = (Types.TypeNs "inner_items");
                     disambiguator = 0 };
                    { Types.data = (Types.ValueNs "main"); disambiguator = 0
                      }
                    ]
                  }
                }
              });
      path =
      [{ Types.data = (Types.TypeNs "inner_items"); disambiguator = 0 };
        { Types.data = (Types.ValueNs "main"); disambiguator = 0 };
        { Types.data = (Types.TypeNs "InTrait"); disambiguator = 0 }]
      }
    };
  moved = None; suffix = None }) */
const _: () = ();
 *)

[@@ FStar.Tactics.Typeclasses.tcinstance]
let main__impl: main__t_InTrait main__t_InStruct =
  {
    main__f_trait_func_pre = (fun (self: main__t_InStruct) (incr: u32) -> true);
    main__f_trait_func_post
    =
    (fun (self: main__t_InStruct) (incr: u32) (out: main__t_InStruct) -> true);
    main__f_trait_func
    =
    fun (self: main__t_InStruct) (incr: u32) ->
      let self:main__t_InStruct =
        { self with main__f_in_struct_field = self.main__f_in_struct_field +! incr }
        <:
        main__t_InStruct
      in
      let _:Prims.unit = main__in_func self.main__f_in_struct_field in
      self
  }

let main (_: Prims.unit) : Prims.unit =
  let is_true:bool =
    (Core.Iter.Traits.Exact_size.f_len #Std.Env.t_Args
        #FStar.Tactics.Typeclasses.solve
        (Std.Env.args () <: Std.Env.t_Args)
      <:
      usize) =.
    mk_usize 1
  in
  let countdown:u32 = mk_u32 0 in
  let countdown:u32 =
    if is_true
    then
      let countdown:u32 = mk_u32 10 in
      countdown
    else countdown
  in
  let _:Prims.unit =
    if is_true
    then
      let _:Prims.unit = main__in_func countdown in
      ()
  in
  let v_val:main__t_InStruct = { main__f_in_struct_field = mk_u32 101 } <: main__t_InStruct in
  let v_val:main__t_InStruct =
    main__f_default_trait_func #main__t_InStruct #FStar.Tactics.Typeclasses.solve v_val
  in
  ()
