module Rust_primitives.Hax

open Rust_primitives.Integers
open Rust_primitives.Arrays

type t_Never = False
let never_to_any #t: t_Never -> t = (fun _ -> match () with)

let repeat #a (x: a) (len: usize): t_Array a len = 
  FStar.Seq.create (v len) x

open Core.Ops.Index
class update_at_tc self idx = {
  [@@@FStar.Tactics.Typeclasses.tcinstance]
  super_index: t_Index self idx;
  update_at: s: self -> i: idx {f_index_pre s i} -> super_index.f_Output -> self;
}

open Core.Slice

/// We have an instance for `int_t n`, but we often work with refined
/// `int_t n`, and F* typeclass inference doesn't support subtyping
/// well, hence the instance below.
instance impl__index_refined t l n r: t_Index (t_Array t l) (x: int_t n {r x})
  = { f_Output = t;
      f_index_pre = (fun (s: t_Array t l) (i: int_t n {r i}) -> v i >= 0 && v i < v l);
      f_index_post = (fun _ _ _ -> true);
      f_index = (fun s i -> Seq.index s (v i));
    }

/// Similarly to `impl__index_refined`, we need to define a instance
/// for refined `int_t n`.
instance update_at_tc_array_refined t l n r: update_at_tc (t_Array t l) (x: int_t n {r x}) = {
  super_index = impl__index_refined t l n r;
  update_at = (fun arr i x -> FStar.Seq.upd arr (v i) x);
}

instance impl__index t l n: t_Index (t_Array t l) (int_t n)
  = { f_Output = t;
      f_index_pre = (fun (s: t_Array t l) (i: int_t n) -> v i >= 0 && v i < v l);
      f_index_post = (fun _ _ _ -> true);
      f_index = (fun s i -> Seq.index s (v i));
    }

instance update_at_tc_array t l n: update_at_tc (t_Array t l) (int_t n) = {
  super_index = FStar.Tactics.Typeclasses.solve <: t_Index (t_Array t l) (int_t n);
  update_at = (fun arr i x -> FStar.Seq.upd arr (v i) x);
}

let (.[]<-) #self #idx {| update_at_tc self idx |} (s: self) (i: idx {f_index_pre s i})
  = update_at s i

unfold let array_of_list (#t:Type)
  (n: nat {n < maxint U16})
  (l: list t {FStar.List.Tot.length l == n})
  : t_Array t (sz n)
  = Seq.seq_of_list l

let box_new (#t:Type) (v: t): Alloc.Boxed.t_Box t Alloc.Alloc.t_Global = v

class iterator_return (self: Type u#0): Type u#1 = {
  [@@@FStar.Tactics.Typeclasses.tcresolve]
  parent_iterator: Core.Iter.Traits.Iterator.t_Iterator self;
  f_fold_return: #b:Type0 -> s:self -> b -> (b -> i:parent_iterator.f_Item{parent_iterator.f_contains s i} -> Core.Ops.Control_flow.t_ControlFlow b b) -> Core.Ops.Control_flow.t_ControlFlow b b;
}
let while_loop #acc_t 
  (condition: acc_t -> bool) 
  (inv: acc_t -> Type0)
  (fuel: (a:acc_t{inv a} -> nat))
  (init: acc_t {inv init}) 
  (f: (i:acc_t{condition i /\ inv i} -> o:acc_t{inv o /\ fuel o < fuel i})): 
  (res: acc_t {inv res /\ not (condition res)})
  = 
  let rec while_loop_internal
  (current: acc_t {inv current}): 
  Tot (res: acc_t {inv res /\ not (condition res)}) (decreases (fuel current))
  = if condition current
    then 
      let next = f current in 
      assert (fuel next < fuel current);
      while_loop_internal next
    else current in 
  while_loop_internal init
  

/// Represents backend failures
let failure #t (_error: string) (_ast: string): Pure t False (fun _ -> True) = ()
