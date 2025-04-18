(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton
 *)

open Syntax
open Interp

exception Unimplemented

type var = string

module Ty = struct
  type t =
    | Int
    | Bool
    | String
    | Pair of t * t
    | Loc of t
    | Arrow of t * t
    | Var of var

  let rec to_string : t -> string =
    let open Printf in
    function
    | Int -> "int"
    | Bool -> "bool"
    | String -> "string"
    | Pair (ty1, ty2) -> (
        match (ty1, ty2) with
        | Arrow _, Arrow _ ->
            sprintf "((%s) * (%s))" (to_string ty1) (to_string ty2)
        | Arrow _, _ ->
            sprintf "((%s) * %s)" (to_string ty1) (to_string ty2)
        | _, Arrow _ ->
            sprintf "(%s * (%s))" (to_string ty1) (to_string ty2)
        | _, _ -> sprintf "(%s * %s)" (to_string ty1) (to_string ty2))
    | Loc ty -> (
        match ty with
        | Arrow _ -> sprintf "(%s) loc" (to_string ty)
        | _ -> to_string ty ^ " loc")
    | Arrow (ty1, ty2) -> sprintf "%s -> %s" (to_string ty1) (to_string ty2)
    | Var a -> a

  let rec to_typ : t -> Syntax.typ = function
    | Int -> T_int
    | Bool -> T_bool
    | String -> T_string
    | Pair (ty1, ty2) -> T_pair (to_typ ty1, to_typ ty2)
    | Loc ty -> T_loc (to_typ ty)
    | Arrow (ty1, ty2) -> T_arrow (to_typ ty1, to_typ ty2)
    | Var _ as ty -> failwith ("Incomplete " ^ to_string ty)

  let string : t = String
  let int : t = Int
  let bool : t = Bool

  let fn : (t * t) -> t =
    function (ty1, ty2) -> Arrow (ty1, ty2)

  let app : (t * t) -> t = function
    | Arrow (ty1, ty2), ty3 -> ty2
    | _ -> failwith "Not a function"

  let ref : t -> t = function ty -> Loc ty

  let deref : t -> t = function
    | Loc ty -> ty
    | _ -> failwith "Not a location"

  let pair : (t * t) -> t =
    function (ty1, ty2) -> Pair (ty1, ty2)
  
  let fst : t -> t = function
    | Pair (ty1, _) -> ty1
    | _ -> failwith "Not a pair"

  let snd : t -> t = function
    | Pair (_, ty2) -> ty2
    | _ -> failwith "Not a pair"
end

type ty_scheme = SimpleTyp of Ty.t | GenTyp of var list * Ty.t

module Tyenv = struct
  type t = (id * ty_scheme) list

  let empty = []

  let find id env =
    match List.assoc id env with SimpleTyp ty | GenTyp (_, ty) -> ty
end

(* Generate a fresh symbol for type variables *)
let gen_sym : unit -> var =
  let count = ref 0 in
  fun () ->
    incr count;
    "'a" ^ string_of_int !count

let new_var () : Ty.t = Var (gen_sym ())

(* Definitions related to free type variable *)

let union_ftv ftv_1 ftv_2 =
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2

let sub_ftv ftv_1 ftv_2 = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : Ty.t -> var list = function
  | Int | Bool | String -> []
  | Pair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | Loc t -> ftv_of_typ t
  | Arrow (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | Var v -> [ v ]

let ftv_of_scheme : ty_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas

let ftv_of_env : Tyenv.t -> var list =
 fun tyenv ->
  List.fold_left
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv

(* Generalize given type into a type scheme *)
let generalize : Tyenv.t -> Ty.t -> ty_scheme =
 fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then SimpleTyp t else GenTyp (ftv, t)

(* Definitions related to substitution *)

type subs = Ty.t -> Ty.t

let empty_subs : subs = fun ty -> ty

let make_subs (a : var) (ty : Ty.t) : subs =
  let rec subs : Ty.t -> Ty.t = function
    | (Int | Bool | String) as t -> t
    | Pair (t1, t2) -> Pair (subs t1, subs t2)
    | Loc t -> Loc (subs t)
    | Arrow (t1, t2) -> Arrow (subs t1, subs t2)
    | Var b when a = b -> ty
    | Var _ as t -> t
  in
  subs

(* a |-> t is a substitution from a to t *)
let ( |-> ) = make_subs
let compose_subs (s1 : subs) (s2 : subs) : subs = fun t -> s2 (s1 t)

(* s2 << s1 means that s1 is applied first, then s2 *)
let ( << ) s2 s1 = compose_subs s1 s2

let subs_scheme (subs : subs) (tyscm : ty_scheme) : ty_scheme =
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
      (* S (\all a.t) = \all b.S{a->b}t (where b is new variable) *)
      let betas = List.map (fun _ -> gen_sym ()) alphas in
      let s' =
        List.fold_left2
          (fun acc alpha beta -> make_subs alpha (Var beta) << acc)
          empty_subs alphas betas
      in
      GenTyp (betas, subs (s' t))

let apply_subs_in_env ~(subs : subs) : Tyenv.t -> Tyenv.t =
  List.map (fun (x, tyscm) -> (x, subs_scheme subs tyscm))

let rec infer : Tyenv.t -> Ty.t -> Syntax.expr -> Tyenv.t * subs =
 fun _ _ _ -> raise Unimplemented

let check (exp : Syntax.expr) : Syntax.typ =
  let tyenv = Tyenv.empty in
  let a : Ty.t = new_var () in
  let _, subs = infer tyenv a exp in
  let ty = subs a in
  Ty.to_typ ty

let rec string_of_typ (ty : Syntax.typ) : string =
  match ty with
  | T_int -> "int"
  | T_bool -> "bool"
  | T_string -> "string"
  | T_pair (tau1, tau2) ->
      "(" ^ string_of_typ tau1 ^ ", " ^ string_of_typ tau2 ^ ")"
  | T_loc tau -> string_of_typ tau ^ " loc"
  | T_arrow (tau1, tau2) -> string_of_typ tau1 ^ " -> " ^ string_of_typ tau2
  