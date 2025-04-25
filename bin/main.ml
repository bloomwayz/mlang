(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * Main interface for M
 *)

open! Base
open Stdio
open Lang_m

let print_position (outx : Out_channel.t) (lexbuf : Lexing.lexbuf) : unit =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Out_channel.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error (lexbuf : Lexing.lexbuf) : Syntax.expr =
  Parser.prog Lexer.read lexbuf

let get_program (filename : string) : Syntax.expr =
  let filename, inx =
    if String.(filename = "-") then ("<stdin>", In_channel.stdin)
    else (filename, In_channel.create filename)
  in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

  match parse_with_error lexbuf with
  | prog ->
      In_channel.close inx;
      prog
  | exception Parser.Error ->
      Out_channel.fprintf stderr "%a: syntax error\n" print_position lexbuf;
      In_channel.close inx;
      Stdlib.exit 2

let () =
  let module Arg = Stdlib.Arg in
  let module Sys = Stdlib.Sys in
  let module Filename = Stdlib.Filename in
  let filename = ref "" in
  let opt_pp = ref false in

  let usage_msg =
    "Usage : " ^ Filename.basename Sys.argv.(0) ^ " [-option] [filename] "
  in
  let speclist =
    [ ("-pp", Arg.Unit (fun _ -> opt_pp := true), "Pretty-print program") ]
  in
  Arg.parse speclist (fun x -> filename := x) usage_msg;
  if String.is_empty !filename then Arg.usage speclist usage_msg
  else
    let prog = get_program !filename in
    Interp.run prog;

    Out_channel.(
      output_char stdout '\n';
      flush stdout);

    Stdlib.exit 0
