{
  open Lexing
  open Parser
  open Printf

  let lexing_error _ msg = failwith msg

  let special_char = function
    | 'n' -> '\n'
    | 't' -> '\t'
    | '\\' -> '\\'
    | '"' -> '"'
    | _ -> failwith "Lexer.special_char"
}

let digits = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let white = ['\t' ' ']
let newline = '\r' | '\n' | "\r\n"
let special_char = ['n' 't' '\\' '"']

rule lexer = parse
  | "//" _* newline? { new_line lexbuf; lexer lexbuf }
  | "/*" { comment 1 lexbuf }
  | '"' { string (Buffer.create 16) lexbuf }
  | eof { EOF }
  | newline { new_line lexbuf; lexer lexbuf }
  | white+ { lexer lexbuf }
  | "while" { WHILE }
  | "for" { FOR }
  | "to" { TO }
  | "break" { BREAK }
  | "let" { LET }
  | "in" { IN }
  | "end" { END }
  | "function" { FUNCTION }
  | "var" { VAR }
  | "type" { TYPE }
  | "array" { ARRAY }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "do" { DO }
  | "of" { OF }
  | "nil" { NIL }
  | "," { COMMA }
  | ":" { COLON}
  | ";" { SEMI }
  | "." { DOT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "&" { AND }
  | "|" { OR }
  | "=" { EQ }
  | "<>" { NE }
  | "<" { LT }
  | "<=" { LE }
  | ">" { GT }
  | ">=" { GE }
  | ":=" { ASSIGN }
  | (digits | '_')+ as n { INT (int_of_string n) }
  | alpha (alpha | digits | '_')* as v { ID v }

and string buf = parse
  | '"' {
      STR (Buffer.contents buf)
    }
  | '\n' {
      new_line lexbuf;
      Buffer.add_char buf '\n';
      string buf lexbuf
    }
  | _ as c {
      Buffer.add_char buf c;
      string buf lexbuf
    }
  | '\\' (special_char as c) {
    Buffer.add_char buf (special_char c);
    string buf lexbuf
    }
  | '\\' (digits digits digits) as code {
    Buffer.add_char buf (Char.chr (int_of_string code));
    string buf lexbuf
    }
  | '\\' (white | newline) {
    string_ignore buf lexbuf
    }
  | eof {
      lexing_error
        (Location.mk (lexeme_start_p lexbuf) (lexeme_start_p lexbuf))
        "Non closed string"
    }

and string_ignore buf = parse
  | '\\' { string buf lexbuf }
  | newline { new_line lexbuf; string buf lexbuf }
  | white { string_ignore buf lexbuf }
  | _ as c {
    lexing_error
      (Location.mk (lexeme_start_p lexbuf) (lexeme_start_p lexbuf))
      (sprintf "Illegal character %c in a formatting sequence" c)
    }

and comment depth = parse
  | "/*" { comment (depth + 1) lexbuf }
  | "*/" {
    if depth = 1 then lexer lexbuf
    else comment (depth - 1) lexbuf
    }
  | eof {
      lexing_error
        (Location.mk (lexeme_start_p lexbuf) (lexeme_start_p lexbuf))
        "Non closed commentary"
    }
  | newline { new_line lexbuf; comment depth lexbuf }
  | _ { comment depth lexbuf }