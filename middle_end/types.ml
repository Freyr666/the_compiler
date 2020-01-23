type unique = int
   
let unique =
  let i = ref (-1) in
  fun () -> incr i; !i 

type t =
  | Tnil
  | Tint
  | Tstring
  | Tunit
  | Tarray of t * unique
  | Trecord of (Symbol.t * t) list * unique
  | Tunknown_yet of Symbol.t * t option ref

let rec to_string = function
  | Tnil -> "nil"
  | Tint -> "int"
  | Tstring -> "string"
  | Tunit -> "unit"
  | Tarray (t,_) -> "array of " ^ (to_string t)
  | Trecord (fields,_) ->
     let content =
       fields
       |> List.map (fun (n,t) -> (Symbol.name n) ^ " : " ^ (to_string t))
       |> String.concat ", "
     in "{ " ^ content ^ " }"
  | Tunknown_yet (_, { contents = Some t}) ->
     to_string t
  | Tunknown_yet (s, { contents = None}) ->
     "rec type " ^ (Symbol.name s)

let rec coerce typ to_typ =
  if typ = to_typ
  then Some typ
  else
    match typ, to_typ with
    | Tnil, Trecord _ -> Some to_typ
    | Trecord _, Tnil -> Some typ
    | Tunknown_yet (_, {contents = Some t}), _ -> coerce t to_typ
    | _, Tunknown_yet (_, {contents = Some t}) -> coerce typ t
    | _ -> None

module Env = struct
  type nonrec typ = t
  
  include Symbol.Table

  type value = Var of typ
             | Fun of typ list * typ

  type types = typ t
  type values = value t
                    
  let base_types =
    let types = [ Symbol.symbol "int", Tint
                ; Symbol.symbol "string", Tstring
                ; Symbol.symbol "unit", Tunit
                ]
    in
    List.fold_left (fun acc (n,t) -> add n t acc) empty types
  
  let base_values = empty

  let enter k v map =
    try
      Some (update k (function None -> raise_notrace Exit
                             | Some _ -> Some v)
              map)
    with Exit -> None

  let replace k v map =
    update k (fun _ -> Some v) map
  
end

