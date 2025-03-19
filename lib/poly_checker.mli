(*
 * SNU 4190.310 Programming Languages
 * Type Checker Interface File
 *)

type ty
type ty_env
type subst

val string_of_ty : ty -> string
val m_ty_of_ty : ty -> Syntax.typ
val infer : ty_env ref -> ty -> Syntax.expr -> subst
val check : Syntax.expr -> Syntax.typ