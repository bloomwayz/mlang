(*
 * M Language Server
 * Type Inferer
 *
 * 2025 Junyoung Park <jypark@ropas.snu.ac.kr>
 *)

open Str
open Range
open Document
open Lang_m
open Lang_m.Syntax

let count = ref 0

let gen_sym = fun () ->
  incr count;
  "'a" ^ string_of_int !count

let check_top (exp : Syntax.expr) : Poly_checker.ty option =
  let open Poly_checker in
  let tyenv = ref empty_tyenv in
  let a = Var (gen_sym ()) in
  match infer tyenv a exp with
  | subs -> Some (subs a)
  | exception _ -> None

let check_sub (top : Syntax.expr) (sub : Syntax.expr) : Poly_checker.ty =
  let open Poly_checker in
  let tyenv = ref empty_tyenv in
  let a = Var (gen_sym ()) in
  let _ = infer tyenv a top in

  let b = Var (gen_sym ()) in
  (infer tyenv b sub) b
let string_of_cnt n =
  let base = Char.code 'a' in
  if n < 26 then
    Printf.sprintf "'%c" (Char.chr (base + n))
  else
    Printf.sprintf "'%c%d" (Char.chr (base + n mod 26)) (n / 26)

let undisclose s =
  let count = ref 0 in
  let r = Str.regexp {|'a[0-9]+|} in
  let rec collect s i =
    match Str.search_forward r s i with
    | i ->
        let sub = Str.matched_string s in
        let subr = Str.regexp sub in
        let s = Str.global_replace subr (string_of_cnt !count) s in
      incr count; collect s (i + 1)
    | exception Not_found -> s
  in
  collect s 0

let rec traverse_ast (exp : expr) (acc : expr list) =
  match exp.desc with
  | Const _ | Var _ | Read -> exp :: acc
  | Fn (_, e) | Write e | Fst e | Snd e | Malloc e | Deref e ->
      traverse_ast e (exp :: acc)
  | App (e1, e2)
  | Bop (_, e1, e2)
  | Assign (e1, e2)
  | Seq (e1, e2)
  | Pair (e1, e2)
  | Let (Val (_, e1), e2)
  | Let (Rec (_, _, e1), e2) ->
      let acc' = traverse_ast e1 (exp :: acc) in
      traverse_ast e2 acc'
  | If (e1, e2, e3) ->
      let acc' = traverse_ast e1 (exp :: acc) in
      let acc'' = traverse_ast e2 acc' in
      traverse_ast e3 acc''

let in_range (pos : Position.t) (exp : expr) =
  let exp_range = Range.from_location exp.loc in
  Range.contains_p exp_range pos

let subexp_at_pos (ast : expr) (pos : Position.t) =
  let subexps = traverse_ast ast [] in
  let subexps' = List.filter (in_range pos) subexps in
  List.nth_opt subexps' 0

let slice txt lnum cnum =
  let r = Str.regexp "\n" in
  let txtlen = String.length txt in

  let rec compute s i ln =
    match Str.search_forward r s i with
    | i -> if ln = lnum then i else compute s i (ln + 1)
    | exception Not_found -> failwith "Not_found"
  in

  let start = compute txt 0 0 in

  String.sub txt start (txtlen - start)

let string_of_token (token : Parser.token) =
  match token with
  | WRITE -> "'a -> 'a"
  | TRUE | FALSE -> "bool"
  | READ -> "int"
  | PLUS | MINUS -> "int -> int -> int"
  | OR | AND -> "bool -> bool -> bool"
  | MALLOC -> "'a -> 'a loc"
  | COLEQ -> "'a loc -> 'a -> 'a"
  | BANG -> "'a loc -> 'a"
  | STRING _ -> "string"
  | INT _ -> "int"
  | _ -> ""

let token_with_lexbuf (lexbuf : Lexing.lexbuf) (pos : Position.t) =
  let rec inner () =
    let token = Lexer.read lexbuf in
    match token with
    | EOF -> None
    | _ ->
        let range = Range.from_lexbuf lexbuf in
        if Range.contains_p range pos then Some (token, range) else inner ()
  in
  inner ()

let infer_var (id : string) (top : expr) (sub : expr) (range : Range.t) =
  let open Poly_checker in
  try 
    match sub.desc with
    | Var x ->
        let ty = string_of_ty (check_sub top sub) in
        Some (ty, range)
    | Let (Val (x, e1), _) ->
        let ty = string_of_ty (check_sub top e1) in
        Some (ty, range)
    | Let (Rec (f, x, e1), e2) ->
        let fexp = { desc = Fn (x, e1); loc = sub.loc } in
        let fty = string_of_ty (check_sub top fexp) in
        if id = f then Some (fty, range)
        else if id = x then
          let r = Str.regexp {| -> |} in
          let i = Str.search_forward r fty 0 in
          Some (String.sub fty 0 i, range)
        else None
    | Fn (x, e) ->
        let fty = string_of_ty (check_sub top sub) in
        let r = Str.regexp {| -> |} in
        let i = Str.search_forward r fty 0 in
        Some (String.sub fty 0 i, range)
    | _ -> None
  with _ -> None

let token_at_pos (raw : string) (pos : Position.t) =
  match Lexing.from_string raw with
  | lexbuf -> token_with_lexbuf lexbuf pos
  | exception _ -> None

let infer_fn (top : expr) (sub : expr) =
  let open Poly_checker in
  let range = Range.from_location sub.loc in
  match check_sub top sub with
  | x -> Some (string_of_ty x, range)
  | exception _ -> None

let infer_bind (top : expr) (sub : expr) =
  let open Poly_checker in

  let fexp, e1 = (
    match sub.desc with
    | Let (Val (x, e1), _) -> e1, e1
    | Let (Rec (f, x, e1), e2) -> { desc = Fn (x, e1); loc = sub.loc }, e1
    | _ -> failwith "not a let-bind" )
  in

  match check_sub top fexp with
  | fty -> 
      let fty_str = string_of_ty fty in
      let r = Range.from_location sub.loc in
      let sln, scl = (r.start.ln, r.start.col) in
      let _, eln, ecl = Location.get_pos_info e1.loc.loc_end in
      let r' = Range.from_tuples (sln, scl) (eln - 1, ecl) in
      Some (fty_str, r')
  | exception _ -> None

let infer_pair (top : expr) (sub : expr) =
  let open Poly_checker in
  let range = Range.from_location sub.loc in
  match check_sub top sub with
  | x -> Some (string_of_ty x, range)
  | exception _ -> None

let infer_sub (st : States.state) (exp : expr) (curr_pos : Position.t) :
    (string * Range.t) option =
  let pgmtxt = st.rawState in
  let token_opt = token_at_pos pgmtxt curr_pos in

  match subexp_at_pos exp curr_pos with
  | Some subexp -> (
      match token_opt with
      | Some (ID x, range) -> infer_var x exp subexp range
      | Some (VAL, range) | Some (REC, range) -> infer_bind exp subexp
      | Some (FN, range) | Some (RARROW, range) -> infer_fn exp subexp
      | Some (EQ, range) -> (
          match subexp.desc with
          | Let _ -> infer_bind exp subexp
          | Bop (Eq, _, _) -> Some ("'a -> 'a -> 'a", range)
          | _ -> failwith "Unreachable")
      | Some (INT 1, range) -> (
          match subexp.desc with
          | Fst _ -> infer_pair exp subexp
          | Const (Int 1) -> Some ("int", range)
          | _ -> failwith "Unreachable")
      | Some (INT 2, range) -> (
          match subexp.desc with
          | Snd _ -> infer_pair exp subexp
          | Const (Int 2) -> Some ("int", range)
          | _ -> failwith "Unreachable")
      | Some (DOT, range) | Some (COMMA, range) -> infer_pair exp subexp
      | Some (token, range) -> (
          match string_of_token token with "" -> None | s -> Some (s, range))
      | _ -> None)
  | _ -> None
