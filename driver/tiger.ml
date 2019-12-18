

let () =
  let filename = Sys.argv.(1) in
  let lex = Lexing.from_channel (open_in filename) in
  let ptree = Parsing.Parser.program Parsing.Lexer.lexer lex in
  Printf.printf "Compiled\nGot a parsetree:\n%s" (Parsing.Parsetree.show_exp ptree)
