open Lexing

type location = { loc_start : Lexing.position
                ; loc_end : Lexing.position
                }

type 'a loc = { v : 'a
              ; loc : location
              }
