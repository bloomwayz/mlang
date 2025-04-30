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

let make_var_subst : var -> var -> subst =
 fun var_old var_new ->
  let rec subs : Ty.t -> Ty.t =
   fun t ->
    match t with
    | Pair (t1, t2) -> Pair (subs t1, subs t2)
    | Loc t' -> Loc (subs t')
    | Arrow (t1, t2) -> Arrow (subs t1, subs t2)
    | Var x -> if x = var_old then Var var_new else t
    | Int | Bool | String -> t
  in
  subs

let ( << ) s1 s2 t = s1 (s2 t) (* substitution composition *)

module Ty_scheme = struct
  type t = SimTy of Ty.t | GenTy of (var list * Ty.t)

  let to_string : t -> string = function
    | SimTy t -> Ty.to_string t
    | GenTy (vars, t) -> "∀ (" ^ String.concat " " vars ^ ")." ^ Ty.to_string t

  let instantiate : t -> Ty.t = function
    | SimTy t -> t
    | GenTy (alphas, t) ->
        let betas = List.map (fun _ -> Ty.gen_sym ()) alphas in
        List.fold_left2
          (fun acc_typ alpha beta -> (make_var_subst alpha beta) acc_typ)
          t alphas betas

  let int : t = SimTy Ty.Int
  let bool : t = SimTy Ty.Bool
  let string : t = SimTy Ty.String

  let fn : t * t -> t = function
    | SimTy ty1, SimTy ty2 -> SimTy (Ty.fn (ty1, ty2))
    | SimTy ty1, GenTy (vars, ty2) -> GenTy (vars, Ty.fn (ty1, ty2))
    | ty1, ty2 ->
        let ty1' = instantiate ty1 in
        let ty2' = instantiate ty2 in
        SimTy (Ty.fn (ty1', ty2'))

  let app : t * t -> t =
   fun (t1, t2) ->
    let ty1 = instantiate t1 in
    let ty2 = instantiate t2 in
    SimTy (Ty.app (ty1, ty2))

  let ref : t -> t = function
    | SimTy ty -> SimTy (Ty.ref ty)
    | GenTy (_, _) as gty ->
        let ty = instantiate gty in
        SimTy (Ty.ref ty)

  let deref : t -> t = function
    | SimTy ty -> SimTy (Ty.deref ty)
    | GenTy (_, _) as gty ->
        let ty = instantiate gty in
        SimTy (Ty.deref ty)

  let pair : t * t -> t = function
    | SimTy ty1, SimTy ty2 -> SimTy (Ty.pair (ty1, ty2))
    | SimTy ty1, GenTy (vs, ty2) | GenTy (vs, ty2), SimTy ty1 ->
        GenTy (vs, Ty.pair (ty1, ty2))
    | GenTy (vs1, ty1), GenTy (vs2, ty2) ->
        let vs = List.sort_uniq String.compare (vs1 @ vs2) in
        GenTy (vs, Ty.pair (ty1, ty2))

  let fst : t -> t = function
    | SimTy ty -> SimTy (Ty.fst ty)
    | GenTy (vs, ty) -> GenTy (vs, Ty.fst ty)

  let snd : t -> t = function
    | SimTy ty -> SimTy (Ty.snd ty)
    | GenTy (vs, ty) -> GenTy (vs, Ty.snd ty)
end

module Make_env (T : sig
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
end) =
struct
  module T = T

  type t = (Syntax.id * value) list
  and value = T.t

  let empty : t = []

  let lookup : Syntax.id -> t -> value =
   fun x tyenv ->
    try List.assoc x tyenv
    with Not_found -> raise (Type_error ("Unbound tvar: " ^ x))
end

module Ty_env = Make_env (Ty_scheme)

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

let ftv_of_scheme : Ty_scheme.t -> var list = function
  | SimTy t -> ftv_of_ty t
  | GenTy (alphas, t) -> sub_ftv (ftv_of_ty t) alphas

let ftv_of_env : Ty_env.t -> var list =
 fun tyenv ->
  List.fold_left
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv

(* Generalize given type into a type scheme *)
let generalize : Ty_env.t -> Ty.t -> Ty_scheme.t =
 fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_ty t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then SimTy t else GenTy (ftv, t)

let subst_scheme : subst -> Ty_scheme.t -> Ty_scheme.t =
 fun subs tyscm ->
  match tyscm with
  | SimTy t -> SimTy (subs t)
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
