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
  | Tunknown_yet of Symbol.t * t option

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
  | Tunknown_yet (_,Some t) ->
     to_string t
  | Tunknown_yet (s, None) ->
     "rec type " ^ (Symbol.name s)

module Env = struct
  type nonrec typ = t
  
  include Symbol.Table

  type value = Var of typ
             | Fun of typ list * typ

  type types = typ t
  type values = value t
                    
  let base_types = empty
  let base_values = empty
        
end

