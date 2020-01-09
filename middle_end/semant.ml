open Types
open Parsing
open Parsetree

exception Type_unknown of string
exception Type_mismatch of string * string * Location.t
   
type types = Env.types
           
type values = Env.values

type typed_exp = { expr : Translate.t
                 ; typ : Types.t
                 }

let rec trans_expr vals types = function
  | Pexp_var { data = v; _ } ->
     trans_var vals types v
  | Pexp_nil _ ->
     { expr = (); typ = Tnil }
  | Pexp_int { data = _; _ } ->
     { expr = (); typ = Tint }
  | Pexp_string { data = _; _ } ->
     { expr = (); typ = Tstring }
  | Pexp_apply { func; args } ->
     let typ = check_apply vals types func args in
     { expr = (); typ }
  | Pexp_binop { op; left; right } ->
     let left' = trans_expr vals types left in
     let right' = trans_expr vals types right in
     let typ = check_binop vals types op left' right' in
     { expr = (); typ }
  | Pexp_record { fields; typ } ->
     let typ = check_record vals types fields typ in
     { expr = (); typ }
  | Pexp_seq exps ->
     List.fold_left
       (fun _ exp ->
         trans_expr vals types exp.Location.data)
       { expr = (); typ = Tunit }
       exps
  | Pexp_assign { var; exp } ->
     let typ = check_assign vals vars var exp in
     { expr = (); typ }
  | Pexp_if { test; true_branch; false_branch } ->
     let typ = check_if
                 vals types
                 test true_branch false_branch
     in
     { expr = (); typ }

and trans_var vals types var = failwith ""
