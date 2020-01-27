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
  | Talias of Symbol.t * t option ref

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
  | Talias (s, { contents = Some _t}) ->
     "rec type " ^ (Symbol.name s)
  (*to_string t*)
  | Talias (s, { contents = None}) ->
     "rec type " ^ (Symbol.name s)

(*
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
 *)
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
  
  let base_values =
    let funcs = [ Symbol.symbol "chr", Fun ([Tint],Tstring)
                ; Symbol.symbol "concat", Fun ([Tstring;Tstring],Tstring)
                ; Symbol.symbol "exit", Fun ([Tint],Tunit)
                ; Symbol.symbol "flush", Fun ([],Tunit)
                ; Symbol.symbol "getchar", Fun ([],Tstring)
                ; Symbol.symbol "not", Fun ([Tint],Tint)
                ; Symbol.symbol "ord", Fun ([Tstring],Tint)
                ; Symbol.symbol "print", Fun ([Tstring],Tunit)
                ; Symbol.symbol "print_err", Fun ([Tstring],Tunit)
                ; Symbol.symbol "print_int", Fun ([Tint],Tunit)
                ; Symbol.symbol "size", Fun ([Tstring],Tunit)
                ; Symbol.symbol "strcmp", Fun ([Tstring;Tstring],Tint)
                ; Symbol.symbol "streq", Fun ([Tstring;Tstring],Tint)
                ; Symbol.symbol "substring", Fun ([Tstring;Tint;Tint],Tstring)
                ]
    in
    List.fold_left (fun acc (n,t) -> add n t acc) empty funcs

  let enter k v map =
    try
      Some (update k (function None -> raise_notrace Exit
                             | Some _ -> Some v)
              map)
    with Exit -> None

  let replace k v map =
    update k (fun _ -> Some v) map

  let print_typenv =
    iter (fun symb typ ->
        Printf.printf "type %s = %s\n"
          (Symbol.name symb) (to_string typ))

  let print_valenv =
    let val_to_string = function
      | Var typ ->
         to_string typ
      | Fun (args, res) ->
         let args' = String.concat ", " (List.map to_string args) in
         "(" ^ args' ^ ") -> " ^ (to_string res)
    in
    iter (fun symb v ->
        Printf.printf "val %s : %s\n"
          (Symbol.name symb) (val_to_string v))
  
end

let rec real_type env = function
  | Talias (name, {contents=None}) ->
     real_type env (Env.find name env)
  | Talias (_, {contents=Some t}) ->
     real_type env t
  | t -> t

let equal env l r =
  match real_type env l, real_type env r with
  | Trecord _, Tnil | Tnil, Trecord _ ->
     true
  | l', r' -> l' = r'
