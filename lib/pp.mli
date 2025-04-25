(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * M Pretty Printer Intrerface File
 *)

module M_Printer : sig
  val print_exp : Syntax.expr -> unit
  val print_typ : Syntax.typ -> unit
end
