%{
open Syntax

exception Empty_binding
exception Invalid_selection

let rec desugar_let = function
  | [], _ -> raise Empty_binding
  | [ x ], e -> Let (x, e)
  | x :: xs, e -> Let (x, desugar_let (xs, e))

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
prog:
    | expr; EOF { $1 }
expr:
    | apply { $1 }
    | FN; param = ID; RARROW; body = expr { Fn (param, body) }
    | LET; decls = list(decl); IN; body = expr; END { desugar_let (decls, body) }
    | IF; pred = expr; THEN; con = expr; ELSE; alt = expr { If (pred, con, alt) }
    | e1 = expr; COLEQ; e2 = expr { Assign (e1, e2) }
    | e = expr; DOT; n = INT { select (e, n) }
    | e1 = expr; SEMI; e2 = expr { Seq (e1, e2) }
    | BANG; e = expr { Deref e }
    | READ { Read }
    | left = expr; op = bop; right = expr { Bop (op, left, right) }
%inline bop:
    | EQ { Eq }
    | AND { And }
    | OR { Or }
    | PLUS { Add }
    | MINUS { Sub }
decl:
    | VAL; x = ID; EQ; e = expr { Val (x, e) }
    | REC; f = ID; EQ; FN; x = ID; RARROW; e = expr { Rec (f, x, e) }
apply:
    | atom { $1 }
    | f = apply; x = atom { App (f, x) }
    | WRITE; e = atom { Write e }
    | MALLOC; e = atom { Malloc e }
atom:
    | TRUE { Const (Bool true) }
    | FALSE { Const (Bool false) }
    | n = INT { Const (Int n) }
    | s = STRING { Const (String s) }
    | x = ID { Var x }
    | LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN { Pair (e1, e2) }
    | LPAREN; e = expr; RPAREN { e }
