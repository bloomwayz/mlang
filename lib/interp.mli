exception Run_error of string
exception Type_error of string

val run : Syntax.expr -> unit
