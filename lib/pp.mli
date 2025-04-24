module M_Printer :
  sig
    val print_exp : Syntax.expr -> unit
    val print_typ : Syntax.typ -> unit
  end
