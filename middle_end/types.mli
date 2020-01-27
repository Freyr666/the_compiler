type unique

val unique : unit -> unique

type t =
  | Tnil
  | Tint
  | Tstring
  | Tunit
  | Tarray of t * unique
  | Trecord of (Symbol.t * t) list * unique
  | Talias of Symbol.t * t option ref

val to_string : t -> string

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

  val print_typenv : typ t -> unit
  val print_valenv : value t -> unit
  
end

val real_type : t Env.t -> t -> t

val equal : t Env.t -> t -> t -> bool
