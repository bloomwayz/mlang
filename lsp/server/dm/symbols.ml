(*
 * M Language Server
 * Document Symbol Provider
 *
 * 2025 Junyoung Park <jypark@ropas.snu.ac.kr>
 *)

open Ppx_yojson_conv_lib.Yojson_conv
open Yojson.Safe.Util
open Protocol
open Document
open Range
open Lang_m

module DocumentSymbols = struct
  type t = documentSymbol list option [@@yojson.option]

  and documentSymbol = {
    name : string;
    kind : int;
    range : Range.t;
    selectionRange : Range.t;
  }
  [@@deriving yojson]

  let create_sym ~name ~kind ~range ~selectionRange : documentSymbol =
    { name; kind; range; selectionRange }

  let create ~symbols : t = Some symbols
  let append (l : documentSymbol list ref) (x : documentSymbol) = l := x :: !l
end

module SymbolKind = struct
  type t =
    | File
    | Module
    | Namespace
    | Package
    | Class
    | Method
    | Property
    | Field
    | Constructor
    | Enum
    | Interface
    | Function
    | Variable
    | Constant
    | String
    | Number
    | Boolean
    | Array
    | Object
    | Key
    | Null
    | EnumMember
    | Struct
    | Event
    | Operator
    | TypeParameter

  let to_int = function
    | File -> 1
    | Module -> 2
    | Namespace -> 3
    | Package -> 4
    | Class -> 5
    | Method -> 6
    | Property -> 7
    | Field -> 8
    | Constructor -> 9
    | Enum -> 10
    | Interface -> 11
    | Function -> 12
    | Variable -> 13
    | Constant -> 14
    | String -> 15
    | Number -> 16
    | Boolean -> 17
    | Array -> 18
    | Object -> 19
    | Key -> 20
    | Null -> 21
    | EnumMember -> 22
    | Struct -> 23
    | Event -> 24
    | Operator -> 25
    | TypeParameter -> 26
end

let lexbuf_to_symbols lexbuf ast =
  let acc = ref [] in

  let append (name, kind, range, selectionRange) =
    if Range.contains range selectionRange then
      let kind = SymbolKind.to_int kind in
      let sym = DocumentSymbols.create_sym ~name ~kind ~range ~selectionRange in
      DocumentSymbols.append acc sym
  in

  let search_id (id : string) (range : Range.t) (exp : Syntax.expr) =
    match exp.desc with
    | Let (Val (x, e1), e2) when id = x ->
        let r = Range.from_location exp.loc in
        let sln, scl = (r.start.ln, r.start.col) in
        let _, eln, ecl = Location.get_pos_info e1.loc.loc_end in
        let r' = Range.from_tuples (sln, scl) (eln - 1, ecl) in
        let kind : SymbolKind.t =
          match e1.desc with Fn _ -> Function | _ -> Variable
        in
        append (x, kind, r', range)
    | Let (Rec (f, x, e1), e2) when id = f ->
        let r = Range.from_location exp.loc in
        let sln, scl = (r.start.ln, r.start.col) in
        let _, eln, ecl = Location.get_pos_info e1.loc.loc_end in
        let r' = Range.from_tuples (sln, scl) (eln - 1, ecl) in
        append (x, Function, r', range)
    | _ -> ()
  in

  let rec inner () =
    match Lexer.read lexbuf with
    | EOF -> ()
    | ID x ->
        let range = Range.from_lexbuf lexbuf in
        let pos = Position.create ~ln:range.end_.ln ~col:range.end_.col in
        (match Inference.subexp_at_pos ast pos with
        | Some subexp -> search_id x range subexp
        | None -> ());
        inner ()
    | _ -> inner ()
    | exception _ -> inner ()
  in

  inner ();
  !acc

let compute params =
  let uri = get_uri params in
  let raw =
    match findr uri with Some x -> x | None -> failwith "Lookup failure"
  in
  match findp uri with
  | Some (Ast (ast, _)) -> (
      match Lexing.from_string raw with
      | lexbuf -> (
          match lexbuf_to_symbols lexbuf ast with
          | [] -> None
          | symbols -> DocumentSymbols.create ~symbols)
      | exception _ -> None)
  | _ -> None

let run id params =
  let result_ = compute params in
  let result = DocumentSymbols.yojson_of_t result_ in
  send (Resp { id; result })
