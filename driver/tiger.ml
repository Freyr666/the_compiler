let with_open_file filename f =
  let in_chan = open_in filename in
  Fun.protect
    (fun () -> f in_chan)
    ~finally:(fun () -> close_in in_chan)

exception Parsing_error of { line : int
                           ; pos : int
                           ; token : string
                           }

let parse in_chan =
  let lexbuf = Lexing.from_channel in_chan in
  try
    Parsing.Parser.program Parsing.Lexer.lexer lexbuf
  with _exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let pos = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let token = Lexing.lexeme lexbuf in
      raise (Parsing_error { line; pos; token })
    end

let typecheck parsetree =
  let open Middle_end in
  let res = Semant.trans_expr Types.Env.base_values Types.Env.base_types parsetree in
  Printf.printf "Got expr of type %s" (Types.to_string res.typ);
  res

let () =
  let filename = Sys.argv.(1) in
  try with_open_file filename (fun chan ->
          parse chan
          |> typecheck
          |> ignore)
  with
  | Parsing_error { line; pos; token } ->
     Printf.printf "Parsing error: line %d pos %d token %s\n" line pos token
  | Middle_end.Semant.Type_unknown (msg, (ls, le)) ->
     Printf.printf "Type unknown at [%d:%d]..[%d:%d]: %s\n"
       ls.pos_lnum (ls.pos_cnum - ls.pos_bol) le.pos_lnum (le.pos_cnum - le.pos_bol) msg
  | Middle_end.Semant.Type_mismatch (msg, (ls, le)) ->
     Printf.printf "Type mismatch at [%d:%d]..[%d:%d]: %s\n"
       ls.pos_lnum (ls.pos_cnum - ls.pos_bol) le.pos_lnum (le.pos_cnum - le.pos_bol) msg
  | Failure s ->
     print_endline "Error:";
     print_endline s
  | exn ->
     print_endline (Printexc.to_string exn)
