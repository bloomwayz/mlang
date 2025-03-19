{
open Lexing
open Parser

exception SyntaxError of string

let comment_depth = ref 0

let comment_spos = ref dummy_pos
let comment_epos = ref dummy_pos

let keywords =
  String_dict.of_alist_exn
    [
      ("true", TRUE);
      ("false", FALSE);
      ("and", AND);
      ("or", OR);
      ("if", IF);
      ("then", THEN);
      ("else", ELSE);
      ("let", LET);
      ("in", IN);
      ("end", END);
      ("fn", FN);
      ("read", READ);
      ("rec", REC);
      ("write", WRITE);
      ("malloc", MALLOC);
      ("val", VAL)
    ]

let to_int s =
  if s.[0] = '~' then -int_of_string (String.sub s 1 (String.length s - 1))
  else int_of_string s

let open_comment depth acc comment lexbuf =
  incr depth;
  comment acc lexbuf

let close_comment depth acc comment lexbuf =
  decr depth;
  if !depth > 0 then comment acc lexbuf
  else
    (comment_epos := lexbuf.lex_curr_p;
    String.concat "" (List.rev acc))
}

let blank = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let digit = ['0'-'9']
let int = '~'? digit+

let str = '"' [^ '"' '\n']* '"'

rule read =
  parse
  | blank    { read lexbuf }
  | newline  { new_line lexbuf; read lexbuf }
  | int as n { INT (to_int n) }
  | id as s  { match String_dict.find keywords s with Some s -> s | None -> ID s }
  | str as s { STRING s }
  | "(*"     {
                comment_depth := 1;
                comment_spos := lexbuf.lex_start_p;
                let c = comment ["(*"] lexbuf in
                COMMENT (c, !comment_spos, !comment_epos)
              }
  | "=>"     { RARROW }
  | ":="     { COLEQ }
  | '='      { EQ }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '!'      { BANG }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '.'      { DOT }
  | ','      { COMMA }
  | ';'      { SEMI }
  | eof      { EOF }
  | _        { raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf)) }

and comment acc =
  parse
  | "(*"    { open_comment comment_depth ("(*" :: acc) comment lexbuf }
  | "*)"    { close_comment comment_depth ("*)" :: acc) comment lexbuf }
  | newline { new_line lexbuf; comment ("\n" :: acc) lexbuf }
  | eof     { raise (SyntaxError "EOF in comment") }
  | _       { comment (lexeme lexbuf :: acc) lexbuf }
  