(*
 * SNU 4190.310 Programming Languages
 *
 * M pretty printer
 *)

open Syntax

module M_Printer = struct
  let ps = print_string
  let nl = print_newline

  let indent i =
    let rec it = function
      | 0 -> ()
      | n ->
          ps " ";
          it (n - 1)
    in
    nl ();
    it i

  let rec pp n exp =
    match exp.desc with
    | Const (String s) -> ps s
    | Const (Int m) -> ps (Int.to_string m)
    | Const (Bool true) -> ps "true"
    | Const (Bool false) -> ps "false"
    | Var s -> ps s
    | Fn (x, e) -> (
        ps ("fn " ^ x ^ " -> ");
        match e.desc with
        | Fn _ -> pp (n + 1) e
        | _ ->
            indent (n + 1);
            pp (n + 1) e)
    | App (e, e') ->
        pp n e;
        ps " ";
        pp n e'
    | If (e1, e2, e3) ->
        ps "if ";
        pp n e1;
        ps " then ";
        indent (n + 1);
        pp (n + 1) e2;
        indent n;
        ps "else";
        indent (n + 1);
        pp (n + 1) e3
    | Read -> ps "read "
    | Write e ->
        ps "write(";
        pp n e;
        ps ")"
    | Let (d, e) ->
        let rec sugaring l acc =
          match l.desc with
          | Let (d, e) -> (
              match e.desc with
              | Let (d', e') -> sugaring e (d :: acc)
              | _ -> (List.rev (d :: acc), e))
          | _ -> raise (Invalid_argument "impossible")
        in
        let decls, body = sugaring exp [] in
        ps "let ";
        List.iter
          (fun x ->
            indent (n + 1);
            printDecl (n + 1) x)
          decls;
        indent n;
        ps "in";
        indent (n + 1);
        pp (n + 1) body;
        indent n;
        ps "end"
    | Malloc e ->
        ps "malloc ";
        pp (n + 1) e
    | Assign (e, e') ->
        pp n e;
        ps " := ";
        pp n e'
    | Deref e ->
        ps "!";
        pp n e
    | Seq (e, e') ->
        pp n e;
        ps ";";
        indent n;
        pp n e'
    | Pair (e1, e2) ->
        ps "(";
        pp n e1;
        ps ", ";
        pp n e2;
        ps ")"
    | Fst e ->
        pp n e;
        ps ".1"
    | Snd e ->
        pp n e;
        ps ".2"
    | Bop (op, e1, e2) ->
        let op_str =
          match op with
          | Add -> "+"
          | Sub -> "-"
          | Eq -> "="
          | And -> "and"
          | Or -> "or"
        in
        ps "(";
        pp n e1;
        ps (" " ^ op_str ^ " ");
        pp n e2;
        ps ")"

  and printDecl n = function
    | Val (x, e) ->
        ps "val ";
        ps (x ^ " = ");
        pp (n + 1) e
    | Rec (f, x, e) ->
        ps ("rec " ^ f ^ "(" ^ x ^ ") = ");
        pp (n + 1) e

  let rec pp_type ty =
    match ty with
    | T_int -> ps "int"
    | T_bool -> ps "bool"
    | T_string -> ps "string"
    | T_pair (tau1, tau2) ->
        ps "(";
        pp_type tau1;
        ps " , ";
        pp_type tau2;
        ps ")"
    | T_loc tau1 ->
        ps "loc (";
        pp_type tau1;
        ps ")"

  let print_exp = pp 0

  let print_typ ty =
    pp_type ty;
    nl ()
end
