open! Base
open Stdio
open Syntax

exception Run_error of string
exception Type_error of string

module Dom = struct
  type loc = int

  type value =
    | Int of int
    | String of string
    | Bool of bool
    | Loc of loc
    | Pair of value * value
    | Closure of closure

  and closure = fexpr * env
  and fexpr = Fun of id * expr | Rec of id * id * expr
  and env = id -> value

  let to_int = function Int n -> n | _ -> raise (Type_error "not an int")
  let to_bool = function Bool b -> b | _ -> raise (Type_error "not a bool")
  let to_loc = function Loc l -> l | _ -> raise (Type_error "not a loc")

  let to_pair = function
    | Pair (a, b) -> (a, b)
    | _ -> raise (Type_error "not a pair")

  let to_closure = function
    | Closure c -> c
    | _ -> raise (Type_error "not a function")

  let print_value = function
    | Int n -> Out_channel.print_endline (Int.to_string n)
    | Bool b -> Out_channel.print_endline (Bool.to_string b)
    | String s -> Out_channel.print_endline s
    | _ -> raise (Type_error "Write operand is not int/bool/string")
end

(* notations (see 5 page in M.pdf) *)
(* f @+ (x, v)             f[x |-> v]
 * store M (l, v)          M[l |-> v]
 * load M l                M(l)
 *)
let loc_count = ref 0
let ( @+ ) f (x, v) y = if Poly.(y = x) then v else f y
let store m (l, v) = m @+ (l, v)
let load m l = m l
let bind env (x, v) = env @+ (x, v)

let malloc m =
  incr loc_count;
  (!loc_count, m)

(* auxiliary functions *)
let op_to_fn =
  let open Dom in
  function
  | Add -> fun (v1, v2) -> Int (to_int v1 + to_int v2)
  | Sub -> fun (v1, v2) -> Int (to_int v1 - to_int v2)
  | And -> fun (v1, v2) -> Bool (to_bool v1 && to_bool v2)
  | Or -> fun (v1, v2) -> Bool (to_bool v1 || to_bool v2)
  | Eq -> (
      fun (v1, v2) ->
        match (v1, v2) with
        | Int n1, Int n2 -> Bool (n1 = n2)
        | String s1, String s2 -> Bool String.(s1 = s2)
        | Bool b1, Bool b2 -> Bool Bool.(b1 = b2)
        | Loc l1, Loc l2 -> Bool (l1 = l2)
        | _ -> raise (Type_error "Eq operands are not int/bool/str/loc"))

let rec eval env mem exp =
  match exp.desc with
  | Const (String s) -> (Dom.String s, mem)
  | Const (Int n) -> (Int n, mem)
  | Const (Bool b) -> (Bool b, mem)
  | Var x -> (env x, mem)
  | Fn (x, e) -> (Closure (Fun (x, e), env), mem)
  | App (e1, e2) -> (
      let v1, m' = eval env mem e1 in
      let v2, m'' = eval env m' e2 in
      let c, env' = Dom.to_closure v1 in
      match c with
      | Fun (x, e) -> eval (bind env' (x, v2)) m'' e
      | Rec (f, x, e) ->
          let env'' = bind env' (x, v2) in
          let env''' = bind env'' (f, v1) in
          eval env''' m'' e)
  | If (e1, e2, e3) ->
      let v1, m' = eval env mem e1 in
      eval env m' (if Dom.to_bool v1 then e2 else e3)
  | Bop (op, e1, e2) ->
      let v1, m' = eval env mem e1 in
      let v2, m'' = eval env m' e2 in
      ((op_to_fn op) (v1, v2), m'')
  | Read ->
      Out_channel.(flush stdout);
      let n =
        try Int.of_string In_channel.(input_line_exn stdin)
        with _ -> raise (Run_error "read error")
      in
      (Int n, mem)
  | Write e ->
      let v, m' = eval env mem e in
      Dom.print_value v;
      (v, m')
  | Pair (e1, e2) ->
      let v1, m' = eval env mem e1 in
      let v2, m'' = eval env m' e2 in
      (Pair (v1, v2), m'')
  | Fst e ->
      let v, m' = eval env mem e in
      (fst (Dom.to_pair v), m')
  | Snd e ->
      let v, m' = eval env mem e in
      (snd (Dom.to_pair v), m')
  | Seq (e1, e2) ->
      let _, m' = eval env mem e1 in
      eval env m' e2
  | Let (Val (x, e1), e2) ->
      let v1, m' = eval env mem e1 in
      eval (bind env (x, v1)) m' e2
  | Let (Rec (f, x, e1), e2) ->
      let closure = Dom.Closure (Rec (f, x, e1), env) in
      eval (bind env (f, closure)) mem e2
  | Malloc e ->
      let v, m' = eval env mem e in
      let l, m'' = malloc m' in
      (Loc l, store m'' (l, v))
  | Assign (e1, e2) ->
      let v1, m' = eval env mem e1 in
      let v2, m'' = eval env m' e2 in
      (v2, store m'' (Dom.to_loc v1, v2))
  | Deref e ->
      let v, m' = eval env mem e in
      (load m' (Dom.to_loc v), m')

let empty_env x = raise (Run_error ("unbound id: " ^ x))
let empty_mem l = raise (Run_error ("uninitialized loc: " ^ Int.to_string l))
let run exp = ignore (eval empty_env empty_mem exp)
