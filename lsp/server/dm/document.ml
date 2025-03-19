(*
 * M Language Server
 * Document Synchronization
 *
 * 2025 Junyoung Park <jypark@ropas.snu.ac.kr>
 *)

open Yojson.Safe.Util
open Lang_m

module States = struct
  type t = (string, state) Hashtbl.t
  and state = { rawState : string; parsedState : pstate }
  and pstate = Ast of Syntax.expr | Fail of string * int * int

  let init () : t = Hashtbl.create 39

  let parse_with_error (lexbuf : Lexing.lexbuf) : Syntax.expr =
    Parser.prog Lexer.read lexbuf

  let get_pstate (filename : string) (rstate : string) : pstate =
    let lexbuf = Lexing.from_string rstate in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

    match parse_with_error lexbuf with
    | ast -> Ast ast
    | exception Parser.Error ->
        let open Lexing in
        let pos = lexbuf.lex_curr_p in
        Fail ("Parsing Error", pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)
    | exception Lexer.SyntaxError msg ->
        let open Lexing in
        let pos = lexbuf.lex_curr_p in
        Fail ("Parsing Error", pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)

  (* synchronize functions *)
  let update (states : t) (uri : string) (raw : string) =
    let uri_len = String.length uri in
    let fname = String.sub uri 8 (uri_len - 8) in
    let parsed = get_pstate fname raw in
    let st = { rawState = raw; parsedState = parsed } in

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
end

let ( @+ ) states (uri, raw) = States.update states uri raw
let ( @- ) states uri = States.remove states uri
let states = States.init ()
let finds = States.find states
let findr = States.find_rstate states
let findp = States.find_pstate states

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
  let uri = get_uri params in
  let raw = get_text params in
  states @+ (uri, raw)

let on_did_change params =
  let uri = get_uri params in
  let raw = get_change params in
  states @+ (uri, raw)

let on_did_close params =
  let uri = get_uri params in
  states @- uri
