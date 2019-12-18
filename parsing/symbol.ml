
type t = int * string

let pp ppf (id, name) =
  Format.fprintf ppf "%s/%d" name id

let show (id, name) =
  Printf.sprintf "%s/%d" name id

let symb_table : (string, int) Hashtbl.t = Hashtbl.create 1024

let symb_ind = ref (-1)

let symbol name =
  match Hashtbl.find_opt symb_table name with
  | Some ind -> ind, name
  | None ->
     incr symb_ind;
     Hashtbl.add symb_table name !symb_ind;
     !symb_ind, name

let name : t -> string = snd

module Table = Map.Make (struct
                   type nonrec t = t
                   let compare (n1,_) (n2,_) = Int.compare n1 n2
                 end)
