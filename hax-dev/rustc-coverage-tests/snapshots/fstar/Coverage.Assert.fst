module Coverage.Assert
#set-options "--fuel 0 --ifuel 1 --z3rlimit 15"
open Core
open FStar.Mul

let might_fail_assert (one_plus_one: u32) : Prims.unit =
  let args:t_Array Core.Fmt.Rt.t_Argument (mk_usize 1) =
    let list = [Core.Fmt.Rt.impl__new_display #u32 one_plus_one] in
    FStar.Pervasives.assert_norm (Prims.eq2 (List.Tot.length list) 1);
    Rust_primitives.Hax.array_of_list 1 list
  in
  let _:Prims.unit =
    Std.Io.Stdio.e_print (Core.Fmt.Rt.impl_1__new_v1 (mk_usize 2)
          (mk_usize 1)
          (let list = ["does 1 + 1 = "; "?\n"] in
            FStar.Pervasives.assert_norm (Prims.eq2 (List.Tot.length list) 2);
            Rust_primitives.Hax.array_of_list 2 list)
          args
        <:
        Core.Fmt.t_Arguments)
  in
  let _:Prims.unit = () in
  let _:Prims.unit =
    match mk_u32 1 +! mk_u32 1, one_plus_one <: (u32 & u32) with
    | left_val, right_val -> Hax_lib.v_assert (left_val =. right_val <: bool)
  in
  ()

let main (_: Prims.unit) : Core.Result.t_Result Prims.unit u8 =
  let countdown:i32 = mk_i32 10 in
  let countdown:i32 =
    Rust_primitives.Hax.while_loop (fun countdown ->
          let countdown:i32 = countdown in
          countdown >. mk_i32 0 <: bool)
      (fun countdown ->
          let countdown:i32 = countdown in
          true)
      (fun countdown ->
          let countdown:i32 = countdown in
          Rust_primitives.Hax.Int.from_machine (mk_u32 0) <: Hax_lib.Int.t_Int)
      countdown
      (fun countdown ->
          let countdown:i32 = countdown in
          let _:Prims.unit =
            if countdown =. mk_i32 1
            then
              let _:Prims.unit = might_fail_assert (mk_u32 3) in
              ()
            else
              if countdown <. mk_i32 5
              then
                let _:Prims.unit = might_fail_assert (mk_u32 2) in
                ()
          in
          let countdown:i32 = countdown -! mk_i32 1 in
          countdown)
  in
  Core.Result.Result_Ok (() <: Prims.unit) <: Core.Result.t_Result Prims.unit u8
