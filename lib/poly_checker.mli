(*
 * SNU 4190.310 Programming Languages
 * Type Checker Interface File
 *)

exception Unimplemented
exception Type_error of string

module Ty : sig
  type t

  val to_string : t -> string
  val to_typ : t -> Syntax.typ
  val new_var : unit -> t
  val string : t
  val int : t
  val bool : t
  val fn : t * t -> t
  val app : t * t -> t
  val ref : t -> t
  val deref : t -> t
  val pair : t * t -> t
  val fst : t -> t
  val snd : t -> t
end

type subst = Ty.t -> Ty.t

module Ty_env : sig
  type t

  val empty : t
  val find : Syntax.id -> t -> Ty.t
end

val infer : Ty_env.t -> Syntax.expr -> Ty.t -> Ty_env.t * subst
val check : Syntax.expr -> Syntax.typ
val string_of_typ : Syntax.typ -> string
