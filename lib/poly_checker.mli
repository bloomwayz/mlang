(*
 * SNU 4190.310 Programming Languages
 * Type Checker Interface File
 *)

exception Unimplemented

module Ty : sig
  type t

  val to_string : t -> string
  val to_typ : t -> Syntax.typ

  val string : t
  val int : t
  val bool : t

  val fn : (t * t) -> t
  val app : (t * t) -> t
  val ref : t -> t
  val deref : t -> t
  val pair : (t * t) -> t
  val fst : t -> t
  val snd : t -> t
end

type subs = Ty.t -> Ty.t

module Tyenv : sig
  type t

  val empty : t
  val find : Syntax.id -> t -> Ty.t
end

val new_var : unit -> Ty.t
val infer : Tyenv.t -> Ty.t -> Syntax.expr -> Tyenv.t * subs
val check : Syntax.expr -> Syntax.typ
val string_of_typ : Syntax.typ -> string