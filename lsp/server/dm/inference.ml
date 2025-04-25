(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * M Language Server
 *)

open Range
open Document
open Lang_m
open Lang_m.Poly_checker

type token_info = (Parser.token * Range.t) option

let check_top (exp : Syntax.expr) : Ty.t =
  let tyenv = Ty_env.empty in
  let a = Ty.new_var () in
  (snd (infer tyenv exp a)) a

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

let ty_of_token (token : Parser.token) =
  let open Ty in
  match token with
  | TRUE | FALSE -> bool
  | READ -> fn (int, int)
  | PLUS | MINUS -> fn (int, fn (int, int))
  | OR | AND -> fn (bool, fn (bool, bool))
  | STRING _ -> string
  | INT _ -> int
  | _ -> failwith "not a type constant"

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

let rec ty_of_exp (env : States.tytbl) (exp : Syntax.expr) : Ty.t =
  match exp.desc with
  | Const (String _) -> Ty.string
  | Const (Int _) -> Ty.int
  | Const (Bool _) -> Ty.bool
  | Var x -> List.assoc x env
  | Fn (x, e) ->
      let param = List.assoc x env in
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

let infer_bind (env : States.tytbl) (exp : Syntax.expr) =
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

let infer_branch (env : States.tytbl) (exp : Syntax.expr) (tko : token_info) =
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

let pipeline (env : States.tytbl) (exp : Syntax.expr) =
  let open Poly_checker in
  let range = Range.from_location exp.loc in
  match ty_of_exp env exp with
  | x -> Some (Ty.to_string x, range)
  | exception _ -> None

let infer_par (env : States.tytbl) (exp : Syntax.expr) =
  let glb =
    match exp.desc with
    | Pair _ -> exp
    | _ ->
        let subexps = traverse_ast exp [] in
        List.nth (List.rev subexps) 1
  in
  pipeline env glb

let infer_space (env : States.tytbl) (exp : Syntax.expr) =
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

let tystr_of_exp (env : States.tytbl) (atbl : Amem.mem) (tko : token_info)
    (exp : Syntax.expr) =
  match tko with
  | Some (ID x, range) ->
      let x' = List.assoc (x, exp.loc) atbl in
      let _ = Printf.eprintf "Var %s\n" x' in
      let ty = List.assoc x' env in
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
  | Some (WRITE, range) -> (
      match exp.desc with
      | Write e ->
          let ty = ty_of_exp env e in
          let ty' = Ty.fn (ty, ty) in
          Some (Ty.to_string ty', range)
      | _ -> failwith "Not a write")
  | Some (MALLOC, range) -> (
      match exp.desc with
      | Malloc e ->
          let ty = ty_of_exp env e in
          let ty' = Ty.fn (ty, Ty.ref ty) in
          Some (Ty.to_string ty', range)
      | _ -> failwith "Not a malloc")
  | Some (COLEQ, range) -> (
      match exp.desc with
      | Assign (e1, e2) ->
          let ty = ty_of_exp env e2 in
          let ty' = Ty.fn (Ty.ref ty, Ty.fn (ty, ty)) in
          Some (Ty.to_string ty', range)
      | _ -> failwith "Not a assign")
  | Some (BANG, range) -> (
      match exp.desc with
      | Deref e ->
          let ty = ty_of_exp env e in
          let ty' = Ty.fn (ty, Ty.deref ty) in
          Some (Ty.to_string ty', range)
      | _ -> failwith "Not a assign")
  | Some ((FN | RARROW | LET | IN | END | DOT | COMMA | SEMI), _) ->
      pipeline env exp
  | Some ((EOF | COMMENT _), _) -> None
  | Some (token, range) -> Some (Ty.to_string (ty_of_token token), range)
  | None -> infer_space env exp
