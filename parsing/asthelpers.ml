open Parsetree

let symb ~loc s = Location.mkloc ~loc (Symbol.symbol s)

let mkloc = Location.mkloc

module Exp = struct

  let var ~loc v = Pexp_var (Location.mkloc ~loc v)
  let nil ~loc = Pexp_nil (Location.mkloc ~loc ())
  let int ~loc v = Pexp_int (Location.mkloc ~loc v)
  let string ~loc v = Pexp_string (Location.mkloc ~loc v)
  let apply func args = Pexp_apply { func; args }
  let binop op left right = Pexp_binop { op; left; right }
  let record fields typ = Pexp_record { fields; typ }
  let seq exps = Pexp_seq exps
  let assign var exp = Pexp_assign { var; exp }
  let _if test true_branch f = Pexp_if { test; true_branch; false_branch=Some f }
  let _if_single test true_branch = Pexp_if { test; true_branch; false_branch = None }
  let _while test body = Pexp_while { test; body }
  let _for ?(esc=false) var lo hi body = Pexp_for { var; escape = ref esc; lo; hi; body }
  let break ~loc = Pexp_break (Location.mkloc ~loc ())
  let _let decs body = Pexp_let { decs; body }
  let array typ size init = Pexp_array { typ; size; init }
  
end

module Op = struct

  let plus ~loc = Location.mkloc ~loc Pop_plus
  let minus ~loc = Location.mkloc ~loc Pop_minus
  let times ~loc = Location.mkloc ~loc Pop_times
  let divide ~loc = Location.mkloc ~loc Pop_divide
  let _and ~loc = Location.mkloc ~loc Pop_and
  let _or ~loc = Location.mkloc ~loc Pop_or
  let eq ~loc = Location.mkloc ~loc Pop_eq
  let ne ~loc = Location.mkloc ~loc Pop_ne
  let lt ~loc = Location.mkloc ~loc Pop_lt
  let le ~loc = Location.mkloc ~loc Pop_le
  let gt ~loc = Location.mkloc ~loc Pop_gt
  let ge ~loc = Location.mkloc ~loc Pop_ge
  
end

module Var = struct

  let simple ~loc name = Pvar_simple (symb ~loc name)
  let field ~loc var name = Pvar_field (Location.mkloc ~loc (var, Symbol.symbol name))
  let subscript ~loc var exp = Pvar_subscript (Location.mkloc ~loc (var, exp))
  
end

module Dec = struct

  let func ~loc ?fun_result fun_name fun_params fun_body =
    Pdec_fun [ mkloc ~loc ({ fun_name; fun_result; fun_body; fun_params }) ]
  let var ~loc ?(esc=false) ?var_typ var_name var_init =
    Pdec_var (mkloc ~loc { var_name; var_escape = ref esc; var_typ = var_typ; var_init })
  let typ_alias ~loc typ_name name =
    Pdec_typ [ mkloc ~loc { typ_name = Symbol.symbol typ_name
                          ; typ = Ptyp_alias name
                 }
      ]
  let typ_record ~loc typ_name fields =
    Pdec_typ [ mkloc ~loc { typ_name = Symbol.symbol typ_name
                          ; typ = Ptyp_record fields
                 }
      ]
  let typ_array ~loc typ_name name =
    Pdec_typ [ mkloc ~loc { typ_name = Symbol.symbol typ_name
                          ; typ = Ptyp_array name
                 }
      ]
  let field ~loc ?(esc=false) field_name field_typ =
    mkloc ~loc { field_name; field_typ; field_escape = ref esc }

end
