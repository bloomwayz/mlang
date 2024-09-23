open! Core
(** Definition of M's syntax, type and interpreter *)

type expr =
  | Const of const
  | Var of id
  | Fn of id * expr
  | App of expr * expr
  | Let of decl * expr
  | If of expr * expr * expr
  | Bop of bop * expr * expr
  | Read
  | Write of expr
  | Malloc of expr (* malloc e *)
  | Assign of expr * expr (* e := e *)
  | Deref of expr (* !e *)
  | Seq of expr * expr (* e; e *)
  | Pair of expr * expr (* (e, e) *)
  | Fst of expr (* e.1 *)
  | Snd of expr (* e.2 *)

and const = String of string | Int of int | Bool of bool
and id = string

and decl =
  | Rec of id * id * expr (* Recursive fn (fun_id, arg_id, body) *)
  | Val of id * expr (* Value, including non-recursive fns *)

and bop = Add | Sub | Eq | And | Or

type typ = T_int | T_bool | T_string | T_pair of typ * typ | T_loc of typ
