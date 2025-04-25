(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * Type Checker Skeleton
 *)

exception Unimplemented
exception Type_error = Interp.Type_error

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
  (* Modify, or add more if needed *)

  (* Type interfaces *)

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
        | Arrow _, _ -> sprintf "((%s) * %s)" (to_string ty1) (to_string ty2)
        | _, Arrow _ -> sprintf "(%s * (%s))" (to_string ty1) (to_string ty2)
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
    | Arrow (ty1, ty2) -> failwith "Function type not supported"
    | Var _ as ty -> failwith ("Incomplete " ^ to_string ty)

  (* Generate a fresh symbol for type variables *)
  let gen_sym : unit -> var =
    let count = ref 0 in
    fun () ->
      incr count;
      "'a" ^ string_of_int !count

  (* Generate a new type variable *)
  let new_var : unit -> t = fun () -> Var (gen_sym ())
  let int : t = Int
  let bool : t = Bool
  let string : t = String
  let fn : t * t -> t = fun (ty1, ty2) -> Arrow (ty1, ty2)

  let app : t * t -> t = function
    | Arrow (ty1, ty2), ty3 ->
        let rec collect t1 t2 acc =
          match (t1, t2) with
          | Int, Int | Bool, Bool | String, String -> acc
          | Pair (x1, y1), Pair (x2, y2) | Arrow (x1, y1), Arrow (x2, y2) ->
              let acc' = collect x1 x2 acc in
              collect y1 y2 acc'
          | Loc x1, Loc x2 -> collect x1 x2 acc
          | Var x, t | t, Var x -> (Var x, t) :: acc
          | _ -> failwith "Not feasible"
        in
        let rec substitute ty tbl =
          match ty with
          | Int | Bool | String -> ty
          | Var x -> (
              match List.assoc_opt ty tbl with Some ty' -> ty' | None -> Var x)
          | Pair (x, y) -> Pair (substitute x tbl, substitute y tbl)
          | Loc x -> substitute x tbl
          | Arrow (x, y) -> Arrow (substitute x tbl, substitute y tbl)
        in
        substitute ty2 (collect ty1 ty3 [])
    | _ -> failwith "Not applicable"

  let ref : t -> t = fun t -> Loc t
  let deref : t -> t = function Loc t -> t | _ -> failwith "Not a location"
  let pair : t * t -> t = fun (ty1, ty2) -> Pair (ty1, ty2)

  let fst : t -> t = function
    | Pair (ty1, _) -> ty1
    | _ -> failwith "Not a pair"

  let snd : t -> t = function
    | Pair (_, ty2) -> ty2
    | _ -> failwith "Not a pair"
end

type ty_scheme = SimpleTy of Ty.t | GenTy of (var list * Ty.t)

module Ty_env = struct
  type t = (Syntax.id * ty_scheme) list

  let empty : t = []

  let find : Syntax.id -> t -> Ty.t =
   fun id env ->
    match List.assoc id env with SimpleTy ty -> ty | GenTy (_, ty) -> ty
end

(* Definitions related to free type variables *)

let union_ftv ftv_1 ftv_2 =
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2

let sub_ftv ftv_1 ftv_2 = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_ty : Ty.t -> var list = function
  | Int | Bool | String -> []
  | Pair (t1, t2) -> union_ftv (ftv_of_ty t1) (ftv_of_ty t2)
  | Loc t -> ftv_of_ty t
  | Arrow (t1, t2) -> union_ftv (ftv_of_ty t1) (ftv_of_ty t2)
  | Var v -> [ v ]

let ftv_of_scheme : ty_scheme -> var list = function
  | SimpleTy t -> ftv_of_ty t
  | GenTy (alphas, t) -> sub_ftv (ftv_of_ty t) alphas

let ftv_of_env : Ty_env.t -> var list =
 fun tyenv ->
  List.fold_left
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv

(* Generalize given type into a type scheme *)
let generalize : Ty_env.t -> Ty.t -> ty_scheme =
 fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_ty t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then SimpleTy t else GenTy (ftv, t)

(* Definitions related to substitution *)

type subst = Ty.t -> Ty.t

let empty_subst : subst = fun t -> t

let make_subst (x : var) (t : Ty.t) : subst =
  let rec subs : Ty.t -> Ty.t = function
    | (Int | Bool | String) as t' -> t'
    | Pair (t1, t2) -> Pair (subs t1, subs t2)
    | Loc t' -> Loc (subs t')
    | Arrow (t1, t2) -> Arrow (subs t1, subs t2)
    | Var y when x = y -> t
    | Var _ as t' -> t'
  in
  subs

let ( << ) s1 s2 t = s1 (s2 t) (* substitution composition *)

let subst_scheme : subst -> ty_scheme -> ty_scheme =
 fun subs tyscm ->
  match tyscm with
  | SimpleTy t -> SimpleTy (subs t)
  | GenTy (alphas, t) ->
      (* S (\all a.t) = \all b.S{a->b}t (where b is new variable) *)
      let betas = List.map (fun _ -> Ty.gen_sym ()) alphas in
      let s' =
        List.fold_left2
          (fun acc_subst alpha beta -> make_subst alpha (Var beta) << acc_subst)
          empty_subst alphas betas
      in
      GenTy (betas, subs (s' t))

let subst_env : subst -> Ty_env.t -> Ty_env.t =
 fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

(* TODO : Implement this function *)
let rec infer : Ty_env.t -> Syntax.expr -> Ty.t -> Ty_env.t * subst =
 fun _ _ _ -> raise Unimplemented

let check (exp : Syntax.expr) : Syntax.typ =
  let tyenv = Ty_env.empty in
  let tau = Ty.new_var () in
  let env, sol = infer tyenv exp tau in
  Ty.to_typ (sol tau)

let rec string_of_typ (ty : Syntax.typ) : string =
  match ty with
  | T_int -> "int"
  | T_bool -> "bool"
  | T_string -> "string"
  | T_pair (tau1, tau2) ->
      "(" ^ string_of_typ tau1 ^ ", " ^ string_of_typ tau2 ^ ")"
  | T_loc tau -> string_of_typ tau ^ " loc"
