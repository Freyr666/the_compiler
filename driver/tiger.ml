

let () =
  let filename = Sys.argv.(1) in
  let lexbuf = Lexing.from_channel (open_in filename) in
  try
    let ptree = Parsing.Parser.program Parsing.Lexer.lexer lexbuf in
    Printf.printf "Compiled\nGot a parsetree:\n%s" (Parsing.Parsetree.show_exp ptree)
  with exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      Printf.printf "Error on line: %d; char: %d; token: %s\n" line cnum tok;
      Printf.printf "%s\n" (Printexc.to_string exn)
    end
