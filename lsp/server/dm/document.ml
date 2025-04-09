(*
 * M Language Server
 * Document Synchronization
 *
 * 2025 Junyoung Park <jypark@ropas.snu.ac.kr>
 *)

open Yojson.Safe.Util
open Range
open Lang_m

module Tyenv = Poly_checker.Tyenv

module Aenv = struct
  type t = Syntax.id -> avar
  and avar = string

  let count = ref 0

  let new_avar () =
    incr count;
    "#" ^ (string_of_int !count)

  let empty : t = fun x -> x

  let init () : t =
    count := 0; empty

  let bind (env : t) (id, avar : Syntax.id * avar) =
    fun x -> if x = id then avar else (env x)

  let bind_new (env : t) (id : Syntax.id) : (avar * t) =
    let s = new_avar () in
    s, bind env (id, s)
end

module States = struct
  type t = (string, state) Hashtbl.t
  and state = {
    rawState : string;
    parsedState : pstate;
    typeState : tstate;
  }
  and pstate = Ast of Syntax.expr | Fail of string * Range.t
  and tstate =
    | Checked of Tyenv.t
    | Typerr of string
    | Otherr of string * Range.t

  exception Lookup_error

  let init () : t = Hashtbl.create 39

  let parse_with_error (lexbuf : Lexing.lexbuf) : Syntax.expr =
    Parser.prog Lexer.read lexbuf

  let rec convert (exp : Syntax.expr) (env : Aenv.t) =
    match exp.desc with
    | Const _ | Read -> exp
    | Var id -> { exp with desc = Var (env id) }
    | Fn (x, e) ->
      let s, env' = Aenv.bind_new env x in
      let e' = convert e env' in
      { exp with desc = Fn (s, e') }
    | App (e1, e2) ->
      let e1' = convert e1 env in
      let e2' = convert e2 env in
      { exp with desc = App (e1', e2') }
    | Let (Val (x, e1), e2) ->
      let s, env' = Aenv.bind_new env x in
      let e1' = convert e1 env' in
      let e2' = convert e2 env' in
      { exp with desc = Let (Val (s, e1'), e2') }
    | Let (Rec (f, x, e1), e2) ->
      let s1, env' = Aenv.bind_new env f in
      let s2, env'' = Aenv.bind_new env' x in
      let e1' = convert e1 env'' in
      let e2' = convert e2 env' in
      { exp with desc = Let (Rec (s1, s2, e1'), e2') }
    | If (e0, e1, e2) ->
      let e0' = convert e1 env in
      let e1' = convert e1 env in
      let e2' = convert e2 env in
      { exp with desc = If (e0', e1', e2') }
    | Bop (op, e1, e2) ->
      let e1' = convert e1 env in
      let e2' = convert e2 env in
      { exp with desc = Bop (op, e1', e2') }
    | Write e -> { exp with desc = Write (convert e env) }
    | Malloc e -> { exp with desc = Malloc (convert e env) }
    | Assign (e1, e2) ->
      let e1' = convert e1 env in
      let e2' = convert e2 env in
      { exp with desc = Assign (e1', e2') }
    | Deref e -> { exp with desc = Deref (convert e env) }
    | Seq (e1, e2) ->
      let e1' = convert e1 env in
      let e2' = convert e2 env in
      { exp with desc = Seq (e1', e2') }
    | Pair (e1, e2) ->
      let e1' = convert e1 env in
      let e2' = convert e2 env in
      { exp with desc = Pair (e1', e2') }
    | Fst e -> { exp with desc = Fst (convert e env) }
    | Snd e -> { exp with desc = Snd (convert e env) }

  let get_pstate (filename : string) (rstate : string) : pstate =
    let lexbuf = Lexing.from_string rstate in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

    match parse_with_error lexbuf with
    | ast ->
      let aenv = Aenv.init () in
      Ast (convert ast aenv)
    | exception Parser.Error ->
        let open Lexing in
        let range = Range.from_lexbuf lexbuf in
        Fail ("Parsing Error", range)
    | exception Lexer.SyntaxError msg ->
        let open Lexing in
        let range = Range.from_lexbuf lexbuf in
        Fail (msg, range)

  let get_tstate (pstate : pstate) : tstate =
    let open Poly_checker in
    match pstate with
    | Ast exp ->
      let tyenv = Tyenv.empty in
      let a = new_var () in
      (match infer tyenv a exp with
      | tyenv', _ -> Tyenv.print tyenv'; Checked tyenv'
      | exception Unimplemented -> Typerr "Type checker unimplemented"
      | exception _ -> Typerr "Type error")
    | Fail (msg, range) -> Otherr (msg, range)

  (* synchronize functions *)
  let update (states : t) (uri : string) (raw : string) =
    let uri_len = String.length uri in
    let fname = String.sub uri 8 (uri_len - 8) in
    let _ = Printf.eprintf "===== Parsing Starts =====\n" in
    let pstate = get_pstate fname raw in
    let _ = Printf.eprintf "===== Infering Starts =====\n" in
    let tstate = get_tstate pstate in
    let st = {
      rawState = raw;
      parsedState = pstate;
      typeState = tstate;
    }
    in

    match Hashtbl.find_opt states uri with
    | Some _ -> Hashtbl.replace states uri st
    | None -> Hashtbl.add states uri st

  let remove (states : t) (uri : string) = Hashtbl.remove states uri

  (* Lookup functions *)
  let find (states : t) (uri : string) = Hashtbl.find_opt states uri

  let find_rstate (states : t) (uri : string) =
    match find states uri with Some st -> Some st.rawState | None -> None

  let find_pstate (states : t) (uri : string) =
    match find states uri with Some st -> Some st.parsedState | None -> None

  let find_tstate (states : t) (uri : string) =
    match find states uri with Some st -> st.typeState | None -> raise Lookup_error
end

let ( @+ ) states (uri, raw) = States.update states uri raw
let ( @- ) states uri = States.remove states uri
let states = States.init ()
let finds = States.find states
let findr = States.find_rstate states
let findp = States.find_pstate states
let findt = States.find_tstate states

(** parse string from params **)

let get_uri params =
  params |> member "textDocument" |> member "uri" |> to_string

let get_text params =
  params |> member "textDocument" |> member "text" |> to_string

let get_change params =
  let changes = params |> member "contentChanges" |> to_list in
  match changes with `Assoc [ ("text", `String x) ] :: _ -> x | _ -> ""

(** document synchronization **)

let on_did_open params =
  let _ = Printf.eprintf "===== Server Initialized =====\n" in
  let uri = get_uri params in
  let raw = get_text params in
  let _ = Printf.eprintf "===== Update Starts =====\n" in
  states @+ (uri, raw)

let on_did_change params =
  let uri = get_uri params in
  let raw = get_change params in
  states @+ (uri, raw)

let on_did_close params =
  let uri = get_uri params in
  states @- uri
