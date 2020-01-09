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
  failwith
    (Printf.sprintf "Typecheck is not implemented\nGot a parsetree:\n%s" (Parsing.Parsetree.show_exp parsetree))

let () =
  let filename = Sys.argv.(1) in
  try with_open_file filename (fun chan ->
          parse chan
          |> typecheck
          |> ignore)
  with
  | Parsing_error { line = _; pos = _; token = _ } ->
     ()
  | Failure s ->
     print_endline "Error:";
     print_endline s
