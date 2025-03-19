%{
open Lexing
open Syntax

exception Empty_binding
exception Invalid_selection

let make_loc (startpos, endpos) =
  Location.{ loc_start = startpos; loc_end = endpos; loc_ghost = false }

let mkexp ~loc d = mk ~loc:(make_loc loc) d

let mkdcl ~loc d = mk_ ~loc:(make_loc loc) d

let rec desugar_let = function
  | [], _ -> raise Empty_binding
  | [ { decl_; loc } ], e -> Let (decl_, e)
  | decl1 :: decl2 :: xs, e ->
    let decl_, loc = decl1.decl_, decl2.loc in
    let desc = desugar_let (decl2 :: xs, e) in
    Let (decl_, { desc; loc })

let select = function
  | e, 1 -> Fst e
  | e, 2 -> Snd e
  | _ -> raise Invalid_selection
%}

%token TRUE FALSE
%token <int> INT
%token <string> ID
%token <string> STRING
%token VAL FN REC LET IN END
%token READ WRITE
%token IF THEN ELSE
%token EQ
%token AND OR
%token PLUS MINUS
%token COLEQ MALLOC BANG
%token LPAREN RPAREN
%token RARROW DOT COMMA SEMI
%token <string * Lexing.position * Lexing.position> COMMENT
%token EOF

%nonassoc RARROW
%right    SEMI
%nonassoc ELSE
%right    COLEQ
%right    OR
%right    AND
%left     EQ
%left     PLUS MINUS
%nonassoc DOT
%nonassoc BANG

%start <expr> prog
%type <expr> expr
%type <decl> decl
%%

%inline mkexp(symb): symb { mkexp ~loc:$sloc $1 }
%inline mkdcl(symb): symb { mkdcl ~loc:$sloc $1 }

prog:
    | cprog; EOF { $1 }
cprog:
    | COMMENT; expr; COMMENT { $2 }
    | COMMENT; expr { $2 }
    | expr; COMMENT { $1 }
    | expr { $1 }
expr:
    | apply { $1 }
    | lexpr { $1 }
    | mkexp(FN; param = ID; RARROW; body = expr { Fn (param, body) }) { $1 }
    | mkexp(IF; pred = expr; THEN; con = expr; ELSE; alt = expr { If (pred, con, alt) }) { $1 }
    | mkexp(e1 = expr; COLEQ; e2 = expr { Assign (e1, e2) }) { $1 }
    | mkexp(e = expr; DOT; n = INT { select (e, n) }) { $1 }
    | mkexp(e1 = expr; SEMI; e2 = expr { Seq (e1, e2) }) { $1 }
    | mkexp(BANG; e = expr { Deref e }) { $1 }
    | mkexp(READ { Read }) { $1 }
    | mkexp(left = expr; op = bop; right = expr { Bop (op, left, right) }) { $1 }
%inline bop:
    | EQ { Eq }
    | AND { And }
    | OR { Or }
    | PLUS { Add }
    | MINUS { Sub }
lexpr:
    | mkexp(LET; decls = list(decl); IN; body = expr; END { desugar_let (decls, body) }) { $1 }
decl:
    | mkdcl(VAL; x = ID; EQ; e = expr { Val (x, e) }) { $1 }
    | mkdcl(REC; f = ID; EQ; FN; x = ID; RARROW; e = expr { Rec (f, x, e) }) { $1 }
apply:
    | atom { $1 }
    | mkexp(f = apply; x = atom { App (f, x) }) { $1 }
    | mkexp(WRITE; e = atom { Write e }) { $1 }
    | mkexp(MALLOC; e = atom { Malloc e }) { $1 }
atom:
    | mkexp(TRUE { Const (Bool true) }) { $1 }
    | mkexp(FALSE { Const (Bool false) }) { $1 }
    | mkexp(n = INT { Const (Int n) }) { $1 }
    | mkexp(s = STRING { Const (String s) }) { $1 }
    | mkexp(x = ID { Var x }) { $1 }
    | mkexp(LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN { Pair (e1, e2) }) { $1 }
    | LPAREN; e = expr; RPAREN { e }
    