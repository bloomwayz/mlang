(*
 * SNU 4190.310 Programming Languages
 * Type Checker Interface File
 *)

type var = string

type ty = 
  | Int
  | Bool
  | String
  | Pair of ty * ty
  | Loc of ty
  | Arrow of ty * ty
  | Var of var

type ty_env
type subst = ty -> ty

val empty_tyenv : ty_env
val string_of_ty : ty -> string
val m_ty_of_ty : ty -> Syntax.typ
val infer : ty_env ref -> ty -> Syntax.expr -> subst
val check : Syntax.expr -> Syntax.typ