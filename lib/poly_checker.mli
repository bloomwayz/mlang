(*
 * SNU 4190.310 Programming Languages
 * Type Checker Interface File
 *)

exception Unimplemented

type ty
type subs = ty -> ty

module Tyenv : sig
  type t

  val empty : t
  val find : Syntax.id -> t -> ty
end

val new_var : unit -> ty
val string_of_ty : ty -> string
val m_ty_of_ty : ty -> Syntax.typ
val infer : Tyenv.t -> ty -> Syntax.expr -> Tyenv.t * subs
val check : Syntax.expr -> Syntax.typ
