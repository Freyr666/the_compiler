open Location

type dec =
  | Pdec_typ of typdec loc list
  | Pdec_fun of fundec loc list
  | Pdec_var of vardec loc (* Vars can't be mutually recursive *)
  [@@deriving show]

and typdec = { typ_name : Symbol.t
             ; typ  : typ
             }

and typ =
  | Ptyp_alias of Symbol.t loc
  | Ptyp_record of field loc list
  | Ptyp_array of Symbol.t loc

and fundec = { fun_name : Symbol.t loc
             ; fun_params : field loc list
             ; fun_result : Symbol.t loc option
             ; fun_body : exp loc
             }

and field = { field_name : Symbol.t loc
            ; field_escape : bool ref
            ; field_typ : Symbol.t loc
            }

and vardec = { var_name : Symbol.t loc
             ; var_escape : bool ref
             ; var_typ : Symbol.t loc option
             ; var_init : exp loc
             }

and exp =
  | Pexp_var of var loc
  | Pexp_nil of unit loc
  | Pexp_int of int loc
  | Pexp_string of string loc
  | Pexp_apply of { func : Symbol.t loc; args : exp loc list }
  | Pexp_binop of { op : op loc; left : exp loc; right : exp loc }
  | Pexp_record of { fields : (Symbol.t loc * exp loc) list
                   ; typ : Symbol.t loc
                   }
  | Pexp_seq of exp loc list
  | Pexp_assign of { var : var; exp : exp loc }
  | Pexp_if of { test : exp loc
               ; true_branch : exp loc
               ; false_branch : exp loc option
               }
  | Pexp_while of { test : exp loc; body : exp loc }
  | Pexp_for of { var : Symbol.t loc
                ; escape : bool ref
                ; lo : exp loc
                ; hi : exp loc
                ; body : exp loc
                }
  | Pexp_break of unit loc
  | Pexp_let of { decs : dec list; body : exp loc }
  | Pexp_array of { typ : Symbol.t loc; size : exp loc; init : exp loc }

and op = Pop_plus | Pop_minus | Pop_times | Pop_divide | Pop_and | Pop_or
         | Pop_eq | Pop_ne | Pop_lt | Pop_le | Pop_gt | Pop_ge

and var =
  | Pvar_simple of Symbol.t loc
  | Pvar_field of (var * Symbol.t) loc
  | Pvar_subscript of (var * exp) loc
               
