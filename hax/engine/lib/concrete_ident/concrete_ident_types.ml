open Prelude

(** An [ExplicitDefId.t] is a Rust [Types.def_id] tagged with some diambiguation metadata.
    
    Rust raw [Types.def_id] can be ambiguous: consider the following Rust code:
    ```rust
    struct S;
    fn f() -> S { S }
    ```
    Here, the return type of `f` (that is, `S`) and the constructor `S` in the body of `f` refers to the exact same identifier `mycrate::S`.
    Yet, they denotes two very different objects: a type versus a constructor.

    [ExplicitDefId.t] clears up this ambiguity, making constructors and types two separate things.

    Also, an [ExplicitDefId.t] always points to an item: an [ExplicitDefId.t] is never pointing to a crate alone.
*)
module type ExplicitDefId = sig
  type t [@@deriving show, yojson, hash, compare, sexp, hash, eq]
  (** Representation of explicit definition identifiers. *)

  val of_def_id : ?constructor:bool -> Types.def_id -> t option
  (** Smart constructor for [t].
      Creates an explicit def id out of a raw Rust definition identifier [Types.def_id].

      When [of_def_id] is called with [id] a [Types.def_id], if the [kind] of [id] is either [Struct] or [Union], then [constructor] is mandatory.
      Otherwise, the argument [constructor] should be [true] only if [id] is a variant.

      This function returns [Some] only when those condition are met.
  *)

  val make_exn : ?constructor:bool -> Types.def_id -> t
  (** Exception-throwing variant of [make].
      This should be used when we know statically that the conditions 
      described in the documentation of [make] are met. 

      For instance, with static [Types.def_id]s or in [Import_thir].
  *)

  val is_constructor : t -> bool
  (** Checks wether a definition identifier [id] points to a constructor.

      [is_constructor id] returns [true] when:
      - the kind of [id] is [Struct] or [Union] and the identifier was tagged as a constructor;
      - the kind of [id] is [Variant].
      Otherwise, [is_constructor id] returns [false].
  *)

  val parent : t -> t option
  (** Looks up the parent of a definition identifier.
      Note that the parent of the identifier of a field is always a constructor.
      
      Also, a top-level item (e.g. `my_crate::some_item`) has no parent: recall that [t] represent only items, not crates.
  *)

  val parents : t -> t list
  (** Ordered list of parents for an identifier [id], starting with [id], up to the top-most parent identifier. *)

  val to_def_id : t -> Types.def_id_contents
  (** Destructor for [t]. *)

  module State : sig
    val list_all : unit -> t list
    (** List all identifiers the engine dealt with so far. Beware, this function is stateful. *)
  end
end

module ViewTypes = struct
  type disambiguator = Int64.t
  [@@deriving show, hash, compare, sexp, hash, eq, map]

  module DisambiguatedString = struct
    type t = { disambiguator : disambiguator; data : string }
    [@@deriving show, hash, compare, sexp, hash, eq, map]
  end
end
