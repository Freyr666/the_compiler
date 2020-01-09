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
