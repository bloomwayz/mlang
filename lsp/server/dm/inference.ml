(*
 * M Language Server
 * Type Checker-Language Server Interface
 *
 * 2025 Junyoung Park <jypark@ropas.snu.ac.kr>
 *)

open Str
open Range
open Document
open Lang_m
open Lang_m.Poly_checker

type token_info = (Parser.token * Range.t) option

let check_top (exp : Syntax.expr) : Ty.t =
  let tyenv = Ty_env.empty in
  let a = Ty.new_var () in
  (snd (infer tyenv exp a)) a

let string_of_cnt n =
  let sprintf = Printf.sprintf in
  let base = Char.code 'a' in
  if n < 26 then sprintf "'%c" (Char.chr (base + n))
  else sprintf "'%c%d" (Char.chr (base + (n mod 26))) (n / 26)

(* let undisclose s = let count = ref 0 in let r = Str.regexp {|'a[0-9]+|} in
   let rec collect s i = match Str.search_forward r s i with | i -> let sub =
   Str.matched_string s in let subr = Str.regexp sub in let s =
   Str.global_replace subr (string_of_cnt !count) s in incr count; collect s (i
   + 1) | exception Not_found -> s in collect s 0 *)

let rec traverse_ast (exp : Syntax.expr) (acc : Syntax.expr list) =
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

let in_range (pos : Position.t) (exp : Syntax.expr) =
  let exp_range = Range.from_location exp.loc in
  Range.contains_p exp_range pos

let subexp_at_pos (ast : Syntax.expr) (pos : Position.t) =
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

let token_at_pos (raw : string) (pos : Position.t) =
  match Lexing.from_string raw with
  | lexbuf -> token_with_lexbuf lexbuf pos
  | exception _ -> None

let rec ty_of_exp (env : Ty_env.t) (exp : Syntax.expr) : Ty.t =
  match exp.desc with
  | Const (String _) -> Ty.string
  | Const (Int _) -> Ty.int
  | Const (Bool _) -> Ty.bool
  | Var x -> Ty_env.find x env
  | Fn (x, e) ->
      let param = Ty_env.find x env in
      let body = ty_of_exp env e in
      Ty.fn (param, body)
  | App (e1, e2) ->
      let ty1 = ty_of_exp env e1 in
      let ty2 = ty_of_exp env e2 in
      Ty.app (ty1, ty2)
  | Let (_, e2) -> ty_of_exp env e2
  | If (e0, e1, e2) -> ty_of_exp env e1
  | Bop (op, e1, e2) -> (
      match op with Add | Sub -> Ty.int | Eq | And | Or -> Ty.bool)
  | Read -> Ty.int
  | Write e -> ty_of_exp env e
  | Malloc e -> Ty.ref (ty_of_exp env e)
  | Assign (e1, e2) -> ty_of_exp env e2
  | Deref e -> Ty.deref (ty_of_exp env e)
  | Seq (e1, e2) -> ty_of_exp env e2
  | Pair (e1, e2) ->
      let t1 = ty_of_exp env e1 in
      let t2 = ty_of_exp env e2 in
      Ty.pair (t1, t2)
  | Fst e -> Ty.fst (ty_of_exp env e)
  | Snd e -> Ty.snd (ty_of_exp env e)

(* let infer_var (id : string) (exp : Syntax.expr) (range : Range.t) = let open
   Poly_checker in try match exp.desc with | Var x -> let ty = string_of_ty
   (Ty.) in Some (ty, range) | Let (Val (x, e1), _) -> let ty = string_of_ty
   (check_sub top e1) in Some (ty, range) | Let (Rec (f, x, e1), e2) -> let fexp
   = { desc = Fn (x, e1); loc = sub.loc } in if id = f then let ty =
   string_of_ty (check_sub top fexp) in Some (ty, range) else if id = x then let
   ty = string_of_ty (check_var top fexp id) in Some (ty, range) else None | Fn
   (x, e) -> let ty = string_of_ty (check_var top sub x) in Some (ty, range) | _
   -> None with _ -> None *)

(* let infer_fn (top : expr) (sub : expr) = let open Poly_checker in let range =
   Range.from_location sub.loc in match check_sub top sub with | x -> Some
   (string_of_ty x, range) | exception _ -> None *)

let infer_bind (env : Ty_env.t) (exp : Syntax.expr) =
  let open Poly_checker in
  let fexp, e1 =
    match exp.desc with
    | Let (Val (x, e1), _) -> (e1, e1)
    | Let (Rec (f, x, e1), e2) -> ({ desc = Fn (x, e1); loc = exp.loc }, e1)
    | _ -> failwith "not a let-bind"
  in

  match ty_of_exp env fexp with
  | fty ->
      let fty_str = Ty.to_string fty in
      let r = Range.from_location exp.loc in
      let sln, scl = (r.start.ln, r.start.col) in
      let _, eln, ecl = Location.get_pos_info e1.loc.loc_end in
      let r' = Range.from_tuples (sln, scl) (eln - 1, ecl) in
      Some (fty_str, r')
  | exception _ -> None

let infer_branch (env : Ty_env.t) (exp : Syntax.expr) (tko : token_info) =
  let open Poly_checker in
  match exp.desc with
  | If (e1, e2, e3) -> (
      match tko with
      | Some (IF, if_range) ->
          let ty = Ty.bool in
          let e1_range = Range.from_location e1.loc in
          let start, end_ = (if_range.start, e1_range.end_) in
          let range = Range.create ~start ~end_ in
          Some (Ty.to_string ty, range)
      | Some (THEN, th_range) ->
          let ty = ty_of_exp env e2 in
          let e2_range = Range.from_location e2.loc in
          let start, end_ = (th_range.start, e2_range.end_) in
          let range = Range.create ~start ~end_ in
          Some (Ty.to_string ty, range)
      | Some (ELSE, el_range) ->
          let ty = ty_of_exp env e3 in
          let e3_range = Range.from_location e3.loc in
          let start, end_ = (el_range.start, e3_range.end_) in
          let range = Range.create ~start ~end_ in
          Some (Ty.to_string ty, range)
      | _ -> None)
  | _ -> None

let pipeline (env : Ty_env.t) (exp : Syntax.expr) =
  let open Poly_checker in
  let range = Range.from_location exp.loc in
  match ty_of_exp env exp with
  | x -> Some (Ty.to_string x, range)
  | exception _ -> None

let infer_par (env : Ty_env.t) (exp : Syntax.expr) =
  let glb =
    match exp.desc with
    | Pair _ -> exp
    | _ ->
        let subexps = traverse_ast exp [] in
        List.nth (List.rev subexps) 1
  in
  pipeline env glb

let infer_space (env : Ty_env.t) (exp : Syntax.expr) =
  let open Poly_checker in
  match exp.desc with
  | Let (Val (x, e1), _) | Let (Rec (_, x, e1), _) ->
      let ty = ty_of_exp env e1 in
      let r = Range.from_location exp.loc in
      let sln, scl = (r.start.ln, r.start.col) in
      let _, eln, ecl = Location.get_pos_info e1.loc.loc_end in
      let range = Range.from_tuples (sln, scl) (eln - 1, ecl) in
      Some (Ty.to_string ty, range)
  | _ -> pipeline env exp

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

(* let print_expr : expr option -> string = function | Some { desc; _ } -> (
   match desc with | Const _ -> "Const" | Var _ -> "Var" | Fn _ -> "Fn" | App _
   -> "App" | Let _ -> "Let" | If _ -> "If" | Bop _ -> "Bop" | Read -> "Read" |
   Write _ -> "Write" | Malloc _ -> "Ref" | Assign _ -> "Asn" | Deref _ -> "Drf"
   | Seq _ -> "Seq" | Pair _ -> "Pair" | Fst _ -> "Fst" | Snd _ -> "Snd") | None
   -> "Null" *)

let tystr_of_exp (env : Ty_env.t) (atbl : Amem.mem) (tko : token_info)
    (exp : Syntax.expr) =
  match tko with
  | Some (ID x, range) ->
      let x' = List.assoc (x, exp.loc) atbl in
      let _ = Printf.eprintf "Var %s\n" x' in
      let ty = Ty_env.find x' env in
      Some (Ty.to_string ty, range)
  | Some ((VAL | REC), range) -> infer_bind env exp
  | Some (EQ, range) -> (
      match exp.desc with
      | Let _ -> infer_bind env exp
      | Bop (Eq, _, _) -> Some ("'a -> 'a -> 'a", range)
      | _ -> failwith "Not an equality")
  | Some (INT 1, range) -> (
      match exp.desc with
      | Fst _ -> pipeline env exp
      | Const (Int 1) -> Some ("int", range)
      | _ -> failwith "Not 1")
  | Some (INT 2, range) -> (
      match exp.desc with
      | Snd _ -> pipeline env exp
      | Const (Int 2) -> Some ("int", range)
      | _ -> failwith "Not 2")
  | Some ((LPAREN | RPAREN), _) -> infer_par env exp
  | Some ((IF | THEN | ELSE), _) -> infer_branch env exp tko
  | Some ((FN | RARROW | LET | IN | END | DOT | COMMA | SEMI), _) ->
      pipeline env exp
  | Some (token, range) -> (
      match string_of_token token with "" -> None | s -> Some (s, range))
  | None -> infer_space env exp
