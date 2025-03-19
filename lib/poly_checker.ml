(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton
 *)

open Syntax
open Interp

type var = string

type ty = 
  | Int
  | Bool
  | String
  | Pair of ty * ty
  | Loc of ty
  | Arrow of ty * ty
  | Var of var
  (* Modify, or add more if needed *)

let rec string_of_ty : ty -> string =
  let open Printf in
  function
  | Int -> "int"
  | Bool -> "bool"
  | String -> "string"
  | Pair (ty1, ty2) -> sprintf "(%s * %s)" (string_of_ty ty1) (string_of_ty ty2)
  | Loc ty -> string_of_ty ty ^ " ref"
  | Arrow (ty1, ty2) -> sprintf "%s -> %s" (string_of_ty ty1) (string_of_ty ty2)
  | Var a -> a

let rec m_ty_of_ty : ty -> Syntax.typ = function
  | Int -> T_int
  | Bool -> T_bool
  | String -> T_string
  | Pair (ty1, ty2) -> T_pair (m_ty_of_ty ty1, m_ty_of_ty ty2)
  | Loc ty -> T_loc (m_ty_of_ty ty)
  | Arrow (ty1, ty2) -> T_arrow (m_ty_of_ty ty1, m_ty_of_ty ty2)
  | Var _ as ty -> failwith ("Incomplete " ^ string_of_ty ty)

type ty_scheme =
  | SimpleTyp of ty
  | GenTyp of (var list * ty)

type ty_env = (Syntax.id * ty_scheme) list

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

(* Definitions related to free type variable *)

let union_ftv ftv_1 ftv_2 = 
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2
  
let sub_ftv ftv_1 ftv_2 =
  List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : ty -> var list = function
  | Int | Bool | String -> []
  | Pair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | Loc t -> ftv_of_typ t
  | Arrow (t1, t2) ->  union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | Var v -> [v]

let ftv_of_scheme : ty_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas 

let ftv_of_env : ty_env -> var list = fun tyenv ->
  List.fold_left 
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv 

(* Generalize given type into a type scheme *)
let generalize : ty_env -> ty -> ty_scheme = fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then
    SimpleTyp t
  else
    GenTyp(ftv, t)

(* Definitions related to substitution *)

type subst = ty -> ty

let empty_subst : subst = fun t -> t

let make_subst : var -> ty -> subst = fun x t ->
  let rec subs t' = 
    match t' with
    | Var x' -> if (x = x') then t else t'
    | Pair (t1, t2) -> Pair (subs t1, subs t2)
    | Loc t'' -> Loc (subs t'')
    | Arrow (t1, t2) -> Arrow (subs t1, subs t2)
    | Int | Bool | String -> t'
  in subs

let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let subst_scheme : subst -> ty_scheme -> ty_scheme = fun subs tyscm ->
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun _ -> new_var()) alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> make_subst alpha (Var beta) @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

let subst_env : subst -> ty_env -> ty_env = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

(* TODO
 * Implement this function;
 * Do not modify the signature, or the Language Server will not work
 *)
let infer : ty_env ref -> ty -> Syntax.expr -> subst =
  raise (Interp.Type_error "Type Checker Unimplemented")

let check (exp : Syntax.expr) : Syntax.typ =
  let tyenv = ref [] in
  let a = Var (new_var ()) in
  let top_type = (infer tyenv a exp) a in
  m_ty_of_ty top_type