type t = Lexing.position * Lexing.position

type 'a loc = { data : 'a
              ; loc : t
              }

let pp_loc pp fmt x = pp fmt x.data

let show_loc sh x = sh x.data

let mk st en : t = (st, en)

let mkloc ~loc data = {data;loc}

let map ~f { data; loc } =
  { data = f data; loc }
