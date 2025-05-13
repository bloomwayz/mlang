(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * Main interface for M
 *)

open Lang_m
open Pp

let src = ref ""
let opt_pp = ref false

let main () =
  Arg.parse
    [ ("-pp", Arg.Unit (fun _ -> opt_pp := true), "print an M program") ]
    (fun x -> src := x)
    ("Usage : " ^ Filename.basename Sys.argv.(0) ^ " [-option] [filename] ");
  try
    let lexbuf =
      Lexing.from_channel (if !src = "" then stdin else open_in !src)
    in
    let pgm = Parser.prog Lexer.read lexbuf in
    if !opt_pp then (
      print_endline "== Input Program ==";
      M_Printer.print_exp pgm;
      print_newline ());
    print_endline "== Running with M Interpreter ==";
    Interp.run pgm
  with v -> Error.handle_exn v

let () = main ()
