%{
open Lexing
open Syntax

exception Empty_binding
exception Invalid_selection
exception Invalid_rec_func

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


%left SEMI
%right FN RARROW LET
%right WRITE
%right COLEQ
%nonassoc IF THEN ELSE
%left EQ
%left PLUS MINUS OR
%left AND
%right BANG MALLOC
%left DOT 
%nonassoc TRUE FALSE NUM ID STRING READ LPAREN

%start <expr> prog
%type <expr> expr
%type <decl> decl
%%

%inline mkexp(symb): symb { mkexp ~loc:$sloc $1 }
%inline mkdcl(symb): symb { mkdcl ~loc:$sloc $1 }

prog:
    | cexp; EOF { $1 }
cexp:
    | expr { $1 }
    | COMMENT; cexp { $2 }
    | cexp; COMMENT { $1 }
expr:
    | aexpr { $1 }
    | mkexp ( e1 = cexp; e2 = aexpr { App (e1, e2) }) { $1 }
    | mkexp(left = cexp; op = bop; right = cexp { Bop (op, left, right) }) { $1 }
    | mkexp(e1 = cexp; SEMI; e2 = expr { Seq (e1, e2) }) { $1 }
    | mkexp (e1 = cexp; COLEQ; e2 = cexp {Assign (e1, e2) }) { $1 }
    | mkexp(e = cexp; DOT; n = INT { select (e, n) }) { $1 }
%inline bop:
    | PLUS { Add }
    | MINUS { Sub }
    | EQ { Eq }
    | AND { And }
    | OR { Or }
aexpr:
    | LPAREN; e = cexp; RPAREN { e }
    | mkexp(n = INT { Const (Int n) }) { $1 }
    | mkexp(s = STRING { Const (String s) }) { $1 }
    | mkexp(TRUE { Const (Bool true) }) { $1 }
    | mkexp(FALSE { Const (Bool false) }) { $1 }
    | mkexp(x = ID { Var x }) { $1 }
    | mkexp(READ { Read }) { $1 }
    | mkexp(FN; param = ID; RARROW; body = cexp { Fn (param, body) }) { $1 }
    | mkexp(LET; decls = list(decl); IN; body = cexp; END { desugar_let (decls, body) }) { $1 }
    | mkexp(IF; pred = cexp; THEN; con = cexp; ELSE; alt = cexp { If (pred, con, alt) }) { $1 }
    | mkexp(WRITE; e = cexp { Write e }) { $1 }
    | mkexp(MALLOC; e = cexp { Malloc e }) { $1 }
    | mkexp(BANG; e = cexp { Deref e }) { $1 }
    | mkexp(LPAREN; e1 = cexp; COMMA; e2 = cexp; RPAREN { Pair (e1, e2) }) { $1 }
decl:
    | mkdcl(VAL; x = ID; EQ; e = cexp { Val (x, e) }) { $1 }
    | mkdcl(REC; f = ID; EQ; e = cexp { 
        match e.desc with Fn (x, e_body) -> Rec (f, x, e_body) | _ -> raise Invalid_rec_func
     }) { $1 }
    