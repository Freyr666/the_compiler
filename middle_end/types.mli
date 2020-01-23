type unique

val unique : unit -> unique

type t =
  | Tnil
  | Tint
  | Tstring
  | Tunit
  | Tarray of t * unique
  | Trecord of (Symbol.t * t) list * unique
  | Tunknown_yet of Symbol.t * t option ref

val to_string : t -> string

val coerce : t -> t -> t option

module Env : sig
  type nonrec typ = t

  include module type of Symbol.Table
                       
  type value = Var of typ
             | Fun of typ list * typ

  type types = typ t
  type values = value t
                    
  val base_types : types
  val base_values : values
  
  val enter : key -> 'a -> 'a t -> 'a t option
  val replace : key -> 'a -> 'a t -> 'a t

end
