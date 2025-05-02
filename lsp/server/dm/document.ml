(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * M Language Server
 *)

open Yojson.Safe.Util
open Range
open Lang_m
open Lang_m.Syntax
open Lang_m.Poly_checker

module Amem = struct
  type t = mem ref
  and mem = ((Syntax.id * Location.t) * avar) list
  and avar = string

  let init () : t = ref []

  let store (id : id) (loc : Location.t) (avar : avar) (mem : t) =
    mem := ((id, loc), avar) :: !mem
end

module Aenv = struct
  type t = id -> avar
  and avar = string

  let count = ref 0

  let new_avar () =
    incr count;
    "#" ^ string_of_int !count

  let empty : t = fun x -> x

  let init () : t =
    count := 0;
    empty

  let bind (env : t) ((id, avar) : id * avar) =
   fun x -> if x = id then avar else env x

  let bind_new (env : t) (id : id) : avar * t =
    let s = new_avar () in
    (s, bind env (id, s))
end

module States = struct
  type t = (string, state) Hashtbl.t
  and state = { rawState : string; parsedState : pstate; typeState : tstate }
  and pstate = Ast of expr * Amem.mem | Fail of string * Range.t

  and tstate =
    | Checked of tytbl
    | Typerr of string
    | Otherr of string * Range.t

  and tytbl = (id * Ty_env.value) list

  exception Lookup_error

  let init () : t = Hashtbl.create 39

  let parse_with_error (lexbuf : Lexing.lexbuf) : expr =
    Parser.prog Lexer.read lexbuf

  let convert exp env =
    let mem = Amem.init () in
    let rec convert (exp : expr) (env : Aenv.t) =
      match exp.desc with
      | Const _ | Read -> exp
      | Var id ->
          let converted = env id in
          Amem.store id exp.loc converted mem;
          { exp with desc = Var converted }
      | Fn (x, e) ->
          let s, env' = Aenv.bind_new env x in
          let _ = Amem.store x exp.loc s mem in
          let e' = convert e env' in
          { exp with desc = Fn (s, e') }
      | App (e1, e2) ->
          let e1' = convert e1 env in
          let e2' = convert e2 env in
          { exp with desc = App (e1', e2') }
      | Let (Val (x, e1), e2) ->
          let s, env' = Aenv.bind_new env x in
          let _ = Amem.store x exp.loc s mem in
          let e1' = convert e1 env' in
          let e2' = convert e2 env' in
          { exp with desc = Let (Val (s, e1'), e2') }
      | Let (Rec (f, x, e1), e2) ->
          let s1, env' = Aenv.bind_new env f in
          let _ = Amem.store f exp.loc s1 mem in
          let s2, env'' = Aenv.bind_new env' x in
          let _ = Amem.store x exp.loc s2 mem in
          let e1' = convert e1 env'' in
          let e2' = convert e2 env' in
          { exp with desc = Let (Rec (s1, s2, e1'), e2') }
      | If (e0, e1, e2) ->
          let e0' = convert e0 env in
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
    in
    let aexp = convert exp env in
    (aexp, !mem)

  let get_pstate (filename : string) (rstate : string) : pstate =
    let lexbuf = Lexing.from_string rstate in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

    match parse_with_error lexbuf with
    | ast ->
        let aenv = Aenv.init () in
        let aast, atbl = convert ast aenv in
        Ast (aast, atbl)
    | exception Lexer.SyntaxError msg ->
        let open Lexing in
        let range = Range.from_lexbuf lexbuf in
        Fail (msg, range)
    | exception _ ->
        let open Lexing in
        let range = Range.from_lexbuf lexbuf in
        Fail ("Syntax Error", range)

  let string_of_cnt n =
    let sprintf = Printf.sprintf in
    let base = Char.code 'a' in
    if n < 26 then sprintf "'%c" (Char.chr (base + n))
    else sprintf "'%c%d" (Char.chr (base + (n mod 26))) (n / 26)

  let tbl_of_env (env : Ty_env.t) : tytbl =
    let rec traverse i acc =
      if i = 0 then acc
      else
        let query = "#" ^ Int.to_string i in
        let item = Ty_env.lookup query env in
        traverse (i - 1) ((query, item) :: acc)
    in
    traverse !Aenv.count []

  let get_tstate (pstate : pstate) : tstate =
    let open Poly_checker in
    match pstate with
    | Ast (exp, _) -> (
        let tyenv = Ty_env.empty in
        let a = Ty.new_var () in
        match infer tyenv exp a with
        | tyenv', _ -> Checked (tbl_of_env tyenv')
        | exception Unimplemented -> Typerr "Type checker unimplemented"
        | exception Type_error msg -> Typerr msg)
    | Fail (msg, range) -> Otherr (msg, range)

  (* synchronize functions *)
  let update (states : t) (uri : string) (raw : string) =
    let uri_len = String.length uri in
    let fname = String.sub uri 8 (uri_len - 8) in
    let pstate = get_pstate fname raw in
    let tstate = get_tstate pstate in
    let st = { rawState = raw; parsedState = pstate; typeState = tstate } in

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
    match find states uri with
    | Some st -> st.typeState
    | None -> raise Lookup_error
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
