(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * Type Checker Interface File
 *)

exception Unimplemented
exception Type_error of string

module Ty : sig
  type t

  val to_string : t -> string
  val to_typ : t -> Syntax.typ
  val new_var : unit -> t
end

module Ty_env : sig
  module T : sig
    type t

    val to_string : t -> string
    val int : t
    val bool : t
    val string : t
    val fn : t * t -> t
    val app : t * t -> t
    val ref : t -> t
    val deref : t -> t
    val pair : t * t -> t
    val fst : t -> t
    val snd : t -> t
  end

  type t
  type value = T.t

  val empty : t
  val lookup : Syntax.id -> t -> value
end

type subst = Ty.t -> Ty.t

val infer : Ty_env.t -> Syntax.expr -> Ty.t -> Ty_env.t * subst
val check : Syntax.expr -> Syntax.typ
