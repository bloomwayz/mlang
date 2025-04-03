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

let check_top (exp : Syntax.expr) : Poly_checker.ty =
  let open Poly_checker in
  let tyenv = Tyenv.empty in
  let a = new_var () in
  (snd (infer tyenv a exp)) a

let check_sub (top : Syntax.expr) (sub : Syntax.expr) : Poly_checker.ty =
  let open Poly_checker in
  let tyenv = Tyenv.empty in
  let a = new_var () in
  let tyenv', _ = infer tyenv a top in
  let b = new_var () in
  let _, subs = infer tyenv' b sub in
  subs b

let check_var (top : Syntax.expr) (sub : Syntax.expr) (id : string) :
    Poly_checker.ty =
  let open Poly_checker in
  let tyenv = Tyenv.empty in
  let a = new_var () in
  let tyenv', _ = infer tyenv a top in
  let b = new_var () in
  let tyenv'', subs = infer tyenv' b sub in
  Tyenv.find id tyenv''

let string_of_cnt n =
  let base = Char.code 'a' in
  if n < 26 then Printf.sprintf "'%c" (Char.chr (base + n))
  else Printf.sprintf "'%c%d" (Char.chr (base + (n mod 26))) (n / 26)

let undisclose s =
  let count = ref 0 in
  let r = Str.regexp {|'a[0-9]+|} in
  let rec collect s i =
    match Str.search_forward r s i with
    | i ->
        let sub = Str.matched_string s in
        let subr = Str.regexp sub in
        let s = Str.global_replace subr (string_of_cnt !count) s in
        incr count;
        collect s (i + 1)
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
        if Range.contains_p range pos then Some (token, range)
        else if pos < range.start then None
        else inner ()
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
        if id = f then
          let ty = string_of_ty (check_sub top fexp) in
          Some (ty, range)
        else if id = x then
          let ty = string_of_ty (check_var top fexp id) in
          Some (ty, range)
        else None
    | Fn (x, e) ->
        let ty = string_of_ty (check_var top sub x) in
        Some (ty, range)
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
  let fexp, e1 =
    match sub.desc with
    | Let (Val (x, e1), _) -> (e1, e1)
    | Let (Rec (f, x, e1), e2) -> ({ desc = Fn (x, e1); loc = sub.loc }, e1)
    | _ -> failwith "not a let-bind"
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

let infer_branch (top : expr) (sub : expr)
    (tko : (Parser.token * Range.t) option) =
  let open Poly_checker in
  match sub.desc with
  | If (e1, e2, e3) -> (
      match tko with
      | Some (IF, if_range) ->
          let e1_range = Range.from_location e1.loc in
          let start, end_ = (if_range.start, e1_range.end_) in
          let range = Range.create ~start ~end_ in
          Some ("bool", range)
      | Some (THEN, th_range) -> (
          let e2_range = Range.from_location e2.loc in
          let start, end_ = (th_range.start, e2_range.end_) in
          let range = Range.create ~start ~end_ in
          match check_sub top e2 with
          | x -> Some (string_of_ty x, range)
          | exception _ -> None)
      | Some (ELSE, el_range) -> (
          let e3_range = Range.from_location e3.loc in
          let start, end_ = (el_range.start, e3_range.end_) in
          let range = Range.create ~start ~end_ in
          match check_sub top e3 with
          | x -> Some (string_of_ty x, range)
          | exception _ -> None)
      | _ -> None)
  | _ -> None

let infer_others (top : expr) (sub : expr) =
  let open Poly_checker in
  let range = Range.from_location sub.loc in
  match check_sub top sub with
  | x -> Some (string_of_ty x, range)
  | exception _ -> None

let infer_par (top : expr) (sub : expr) =
  let glb =
    match sub.desc with
    | Pair _ -> sub
    | _ ->
        let subexps = traverse_ast sub [] in
        List.nth (List.rev subexps) 1
  in
  infer_others top glb

let infer_space (top : expr) (sub : expr) =
  let open Poly_checker in
  match sub.desc with
  | Let (Val (x, e1), _) | Let (Rec (_, x, e1), _) ->
      let r = Range.from_location sub.loc in
      let sln, scl = r.start.ln, r.start.col in
      let _, eln, ecl = Location.get_pos_info e1.loc.loc_end in
      let range = Range.from_tuples (sln, scl) (eln, ecl) in
      (match check_sub top e1 with
      | x -> Some (string_of_ty x, range)
      | exception _ -> None)
  | _ -> infer_others top sub

let print_token : (Parser.token * Range.t) option -> string = function
  | Some (ID x, _) -> "ID: " ^ x
  | Some (VAL, _) -> "VAL"
  | Some (REC, _) -> "REC"
  | Some (FN, _) -> "FN"
  | Some (RARROW, _) -> "RARR"
  | Some (EQ, _) -> "EQ"
  | Some (INT n, _) -> "INT: " ^ string_of_int n
  | Some (LPAREN, _) -> "LPAR"
  | Some (RPAREN, _) -> "RPAR"
  | Some (IF, _) -> "IF"
  | Some (THEN, _) -> "THEN"
  | Some (ELSE, _) -> "ELSE"
  | Some (LET, _) -> "LET"
  | Some (IN, _) -> "IN"
  | Some (END, _) -> "END"
  | Some (DOT, _) -> "DOT"
  | Some (COMMA, _) -> "COMMA"
  | Some (SEMI, _) -> "SEMI"
  | Some (token, _) -> "ETC"
  | None -> "NULL"

let print_expr : expr option -> string = function
  | Some { desc; _ } -> (
      match desc with
      | Const _ -> "Const"
      | Var _ -> "Var"
      | Fn _ -> "Fn"
      | App _ -> "App"
      | Let _ -> "Let"
      | If _ -> "If"
      | Bop _ -> "Bop"
      | Read -> "Read"
      | Write _ -> "Write"
      | Malloc _ -> "Ref"
      | Assign _ -> "Asn"
      | Deref _ -> "Drf"
      | Seq _ -> "Seq"
      | Pair _ -> "Pair"
      | Fst _ -> "Fst"
      | Snd _ -> "Snd")
  | None -> "Null"

let infer_sub (st : States.state) (exp : expr) (curr_pos : Position.t) :
    (string * Range.t) option =
  let pgmtxt = st.rawState in
  let token_opt = token_at_pos pgmtxt curr_pos in
  let subexp_opt = subexp_at_pos exp curr_pos in
  let _ =
    Printf.eprintf "%s\t%s\n" (print_token token_opt) (print_expr subexp_opt)
  in

  match (token_opt, subexp_opt) with
  | _, None -> None
  | Some (ID x, range), Some subexp -> infer_var x exp subexp range
  | Some ((VAL | REC), range), Some subexp -> infer_bind exp subexp
  | Some ((FN | RARROW), range), Some subexp -> infer_fn exp subexp
  | Some (EQ, range), Some subexp -> (
      match subexp.desc with
      | Let _ -> infer_bind exp subexp
      | Bop (Eq, _, _) -> Some ("'a -> 'a -> 'a", range)
      | _ -> failwith "Unreachable")
  | Some (INT 1, range), Some subexp -> (
      match subexp.desc with
      | Fst _ -> infer_others exp subexp
      | Const (Int 1) -> Some ("int", range)
      | _ -> failwith "Unreachable")
  | Some (INT 2, range), Some subexp -> (
      match subexp.desc with
      | Snd _ -> infer_others exp subexp
      | Const (Int 2) -> Some ("int", range)
      | _ -> failwith "Unreachable")
  | Some ((LPAREN | RPAREN), _), Some subexp -> infer_par exp subexp
  | Some ((IF | THEN | ELSE), _), Some subexp ->
      infer_branch exp subexp token_opt
  | Some ((LET | IN | END | DOT | COMMA | SEMI), _), Some subexp ->
      infer_others exp subexp
  | Some (token, range), Some subexp -> (
      match string_of_token token with "" -> None | s -> Some (s, range))
  | None, Some subexp -> infer_space exp subexp
