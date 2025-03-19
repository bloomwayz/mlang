open! Core
open Lang_m

exception SyntaxError of string * int * int

let print_position (outx : Out_channel.t) (lexbuf : Lexing.lexbuf) : unit =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
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
      fprintf stderr "%a: syntax error\n" print_position lexbuf;
      In_channel.close inx;
      exit (-1)

let get_program_from_string (filename : string) (state : string) : Syntax.expr =
  let lexbuf = Lexing.from_string state in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

  match parse_with_error lexbuf with
  | prog -> prog
  | exception Parser.Error ->
      let open Lexing in
      let pos = lexbuf.lex_curr_p in
      raise
        (SyntaxError
           ("Parsing Error", pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1))
  | exception Lexer.SyntaxError msg ->
      let open Lexing in
      let pos = lexbuf.lex_curr_p in
      raise (SyntaxError (msg, pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1))

let command : Command.t =
  Command.basic ~summary:"The Language M"
    ~readme:(fun () -> "Language M is a programming language")
    (let%map_open.Command filename =
       anon (maybe_with_default "-" ("filename" %: Filename_unix.arg_type))
     in
     fun () ->
       let prog = get_program filename in
       Interp.run prog)

let () = Command_unix.run ~version:"0.1.0" ~build_info:"Lang M" command
