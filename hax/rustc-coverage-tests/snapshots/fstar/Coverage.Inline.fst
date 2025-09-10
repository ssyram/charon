module Coverage.Inline
#set-options "--fuel 0 --ifuel 1 --z3rlimit 15"
open Core
open FStar.Mul

let length (#v_T: Type0) (xs: t_Slice v_T) : usize = Core.Slice.impl__len #v_T xs

let swap
      (#v_T: Type0)
      (#[FStar.Tactics.Typeclasses.tcresolve ()] i1: Core.Marker.t_Copy v_T)
      (xs: t_Slice v_T)
      (i j: usize)
    : t_Slice v_T =
  let t:v_T = xs.[ i ] in
  let xs:t_Slice v_T =
    Rust_primitives.Hax.Monomorphized_update_at.update_at_usize xs i (xs.[ j ] <: v_T)
  in
  let xs:t_Slice v_T = Rust_primitives.Hax.Monomorphized_update_at.update_at_usize xs j t in
  xs

let display
      (#v_T: Type0)
      (#[FStar.Tactics.Typeclasses.tcresolve ()] i1: Core.Fmt.t_Display v_T)
      (xs: t_Slice v_T)
    : Prims.unit =
  let _:Prims.unit =
    Rust_primitives.Hax.failure "(FunctionalizeLoops) something is not implemented yet.This is discussed in issue https://github.com/hacspec/hax/issues/405.\nPlease upvote or comment this issue if you see this error message.\nLoop without mutation"
      "{\n for x in (core::iter::traits::collect::f_into_iter(xs)) {\n {\n let _: tuple0 = {\n std::io::stdio::e_print(\n core::fmt::rt::impl_2__new_v1::<\n generic_value!(todo),\n generic_value!(todo),\n >([\"\"], [c..."

  in
  let _:Prims.unit =
    Std.Io.Stdio.e_print (Core.Fmt.Rt.impl_2__new_const (mk_usize 1)
          (let list = ["\n"] in
            FStar.Pervasives.assert_norm (Prims.eq2 (List.Tot.length list) 1);
            Rust_primitives.Hax.array_of_list 1 list)
        <:
        Core.Fmt.t_Arguments)
  in
  let _:Prims.unit = () in
  ()

let error (_: Prims.unit) : Prims.unit =
  Rust_primitives.Hax.never_to_any (Core.Panicking.panic_fmt (Core.Fmt.Rt.impl_2__new_const (mk_usize
              1)
            (let list = ["error"] in
              FStar.Pervasives.assert_norm (Prims.eq2 (List.Tot.length list) 1);
              Rust_primitives.Hax.array_of_list 1 list)
          <:
          Core.Fmt.t_Arguments)
      <:
      Rust_primitives.Hax.t_Never)

let rec permutate
      (#v_T: Type0)
      (#[FStar.Tactics.Typeclasses.tcresolve ()] i1: Core.Marker.t_Copy v_T)
      (#[FStar.Tactics.Typeclasses.tcresolve ()] i2: Core.Fmt.t_Display v_T)
      (xs: t_Slice v_T)
      (k: usize)
    : t_Slice v_T =
  let n:usize = length #v_T xs in
  let xs:t_Slice v_T =
    if k =. n
    then
      let _:Prims.unit = display #v_T xs in
      xs
    else
      if k <. n
      then
        Rust_primitives.Hax.Folds.fold_range k
          n
          (fun xs temp_1_ ->
              let xs:t_Slice v_T = xs in
              let _:usize = temp_1_ in
              true)
          xs
          (fun xs i ->
              let xs:t_Slice v_T = xs in
              let i:usize = i in
              let xs:t_Slice v_T = swap #v_T xs i k in
              let xs:t_Slice v_T = permutate #v_T xs (k +! mk_usize 1 <: usize) in
              let xs:t_Slice v_T = swap #v_T xs i k in
              xs)
      else
        let _:Prims.unit = error () in
        xs
  in
  xs

let permutations
      (#v_T: Type0)
      (#[FStar.Tactics.Typeclasses.tcresolve ()] i1: Core.Marker.t_Copy v_T)
      (#[FStar.Tactics.Typeclasses.tcresolve ()] i2: Core.Fmt.t_Display v_T)
      (xs: t_Slice v_T)
    : Prims.unit =
  let ys:Alloc.Vec.t_Vec v_T Alloc.Alloc.t_Global =
    Alloc.Borrow.f_to_owned #(t_Slice v_T) #FStar.Tactics.Typeclasses.solve xs
  in
  let ys:Alloc.Vec.t_Vec v_T Alloc.Alloc.t_Global = permutate #v_T ys (mk_usize 0) in
  ()

let main (_: Prims.unit) : Prims.unit =
  let _:Prims.unit =
    permutations #char
      ((let list = ['a'; 'b'; 'c'] in
          FStar.Pervasives.assert_norm (Prims.eq2 (List.Tot.length list) 3);
          Rust_primitives.Hax.array_of_list 3 list)
        <:
        t_Slice char)
  in
  ()
