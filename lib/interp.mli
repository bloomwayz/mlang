(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * M Interpreter Interface File
 *)

exception Run_error of string
exception Type_error of string

val run : Syntax.expr -> unit
