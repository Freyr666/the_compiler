open Types
open Parsing
open Parsetree

exception Type_unknown of string * Location.t
exception Type_mismatch of string * Location.t

type types = Env.types
           
type values = Env.values

type typed_exp = { expr : Translate.t
                 ; typ : Types.t
                 }

let type_unknown : type a. Location.t -> (a, unit, string, 'b) format4 -> a =
  fun loc ->
  Printf.ksprintf (fun msg -> raise (Type_unknown (msg, loc)))

let type_mismatch : type a. Location.t -> (a, unit, string, 'b) format4 -> a =
  fun loc ->
  Printf.ksprintf (fun msg -> raise (Type_mismatch (msg, loc)))

let rec trans_expr vals types exp =
  let open Parsing.Location in
  match exp with
  (* Variable *)
  | Pexp_var { data = v; _ } ->
     trans_var vals types v
  (* Basic Types *)
  | Pexp_nil _ ->
     { expr = (); typ = Tnil }
  | Pexp_int { data = _; _ } ->
     { expr = (); typ = Tint }
  | Pexp_string { data = _; _ } ->
     { expr = (); typ = Tstring }
  (* Function application *)
  | Pexp_apply { func; args } ->
     let args = List.map
                  (map ~f:(trans_expr vals types))
                  args
     in
     let typ = check_apply vals func args in
     { expr = (); typ }
  (* Basic operators *)
  | Pexp_binop { op; left; right } ->
     let left' = trans_expr vals types left.data in
     let right' = trans_expr vals types right.data in
     let typ = check_binop op left' right' in
     { expr = (); typ }
  (* Record constructors *)
  | Pexp_record { fields; typ } ->
     let fields = List.map (fun (name, field) ->
                      name, map ~f:(fun x -> trans_expr vals types x) field)
                    fields
     in
     let typ = check_record types fields typ in
     { expr = (); typ }
  (* Sequences *)
  | Pexp_seq exps ->
     List.fold_left
       (fun _ exp ->
         trans_expr vals types exp.Location.data)
       { expr = (); typ = Tunit }
       exps
  (* Assign *)
  | Pexp_assign { var; exp } ->
     let var = trans_var vals types var in
     let exp = map ~f:(trans_expr vals types) exp in
     check_type var.typ exp;
     { expr = (); typ = Tunit }
  (* If cond *)
  | Pexp_if { test; true_branch; false_branch } ->
     let typ = check_if vals types test true_branch false_branch in
     { expr = (); typ }
  (* While loop *)
  | Pexp_while { test; body } ->
     let test = map ~f:(trans_expr vals types) test in
     let body = map ~f:(trans_expr vals types) body in
     check_type Tint test;
     check_type Tunit body;
     { expr = (); typ = Tunit }
  | Pexp_for { var; escape; lo; hi; body } ->
     let index =
       { var_name = var
       ; var_escape = escape
       ; var_typ = None
       ; var_init = lo
       }
     in
     let bound_name = mkloc ~loc:var.loc (Symbol.symbol "upper_bound") in
     let bound =
       { var_name = bound_name
       ; var_escape = ref false
       ; var_typ = None
       ; var_init = hi
       }
     in
     let decs = [ Pdec_var (mkloc ~loc:var.loc index)
                ; Pdec_var (mkloc ~loc:var.loc bound)
                ]
     in
     let left = Pexp_var (mkloc ~loc:var.loc (Pvar_simple var)) in
     let right = Pexp_var (mkloc ~loc:var.loc (Pvar_simple bound_name)) in
     let test = Pexp_binop { op = mkloc ~loc:var.loc Pop_le
                           ; left = mkloc ~loc:var.loc left
                           ; right = mkloc ~loc:var.loc right
                  }
     in
     let letbody = Pexp_while { test = mkloc ~loc:var.loc test; body } in
     let letexp = Pexp_let { decs; body = mkloc ~loc:body.loc letbody } in
     trans_expr vals types letexp
  | Pexp_break _ ->
     { expr = (); typ = Tunit }
  | Pexp_array { typ; size; init } ->
     let size = map ~f:(trans_expr vals types) size in
     let init = map ~f:(trans_expr vals types) init in
     let typ = check_array types typ.data init in
     check_type Tint size;
     { expr = (); typ }
  | Pexp_let { decs; body } ->
     let vals', types' = trans_decs vals types decs in
     trans_expr vals' types' body.data

and trans_var vals types var =
  match var with
  | Pvar_simple { data = symb; loc } -> begin
      match Env.find_opt symb vals with
      | Some (Env.Var typ) -> { expr = (); typ }
      | _ -> type_unknown loc "Expected a simple var"
    end
  | Pvar_field { data = (var, symb); loc } -> begin
     let parent = trans_var vals types var in
     match parent with
     | { expr = (); typ = Trecord (fields, _) } ->
        (match List.assoc_opt symb fields with
         | Some typ -> { expr = (); typ }
         | None -> type_mismatch loc "Record has no such field %s" (Symbol.name symb))
     | _ -> type_mismatch loc "Not a record"
    end
  | Pvar_subscript { data = (var, exp); loc } -> begin
      let parent = trans_var vals types var in
      let index = trans_expr vals types exp in
      match parent, index with
      | { expr = (); typ = Tarray (typ, _) },
        { expr = (); typ = Tint } ->
         { expr = (); typ }
      | _ -> type_mismatch loc "Expected to find array"
    end

and trans_decs vals types = function
  | [] -> vals, types
  | (Pdec_var _vardec)::_tl ->
     failwith "TODO"
  | _ -> failwith "TODO"

and check_apply vals func args =
  let open Parsing.Location in
  match Env.find_opt func.data vals with
  | Some (Fun (argtyps, rtyp))
       when (List.length argtyps = List.length args) ->
     List.iter2 (fun typ exp ->
         let exp_typ = exp.data.typ in
         if typ <> exp_typ
         then type_mismatch exp.loc "Function argument type mismatch, expected %s, got %s"
                (Types.to_string typ) (Types.to_string exp_typ))
       argtyps
       args;
     rtyp
  | Some (Fun (argtyps,_)) ->
     type_mismatch func.loc "Argument number mismatch, expected %d, got %d"
       (List.length argtyps) (List.length args)
  | _ ->
     type_mismatch func.loc "Callee is not a function"
                   
and check_binop op left right =
  let kind = function
    | Pop_plus | Pop_minus | Pop_times | Pop_divide -> `Arith
    | Pop_and | Pop_or -> `Logic
    | Pop_lt | Pop_le | Pop_gt | Pop_ge -> `Relat
    | Pop_eq | Pop_ne -> `Eq
  in
  match kind op.data with
  | `Arith | `Logic | `Relat ->
     if left.typ = Tint && left.typ = right.typ
     then Tint
     else type_mismatch op.loc "Arithmetic and logic operations require integer operands"
  | `Eq ->
     if left.typ = right.typ
     then Tint
     else type_mismatch op.loc "Equality requires both operands to have the same type"

and check_record types fields typ =
  match Env.find_opt typ.data types with
  | None ->
     type_mismatch typ.loc "Record type %s is undefined"
       (Symbol.name typ.data)
  | Some (Trecord (ftyps,_) as ret)
       when (List.length ftyps) = (List.length fields) ->
     List.iter2 (fun (field, typ) (name, exp) ->
         let open Parsing.Location in
         let exp_typ = exp.data.typ in
         if typ <> exp_typ
            || (Symbol.name field) <> (Symbol.name name.data)
         then type_mismatch exp.loc "Record field type mismatch, expected %s, got %s"
                 (Types.to_string typ) (Types.to_string exp_typ))
       ftyps
       fields;
     ret
  | Some (Trecord _) -> type_mismatch typ.loc "Record fields mismatch"
  | _ -> type_mismatch typ.loc "Unknown record type %s"
           (Symbol.name typ.data)

and check_if vals types test true_branch false_branch =
  let open Parsing.Location in
  let test_typ = (trans_expr vals types test.data).typ in
  if test_typ <> Tint
  then type_mismatch test.loc "If condition should have type Int, found exp of type %s"
         (Types.to_string test_typ);
  match false_branch with
  | None ->
     (trans_expr vals types true_branch.data).typ
  | Some false_branch ->
     let true_typ = (trans_expr vals types true_branch.data).typ
     and false_typ = (trans_expr vals types false_branch.data).typ
     in if true_typ = false_typ
        then true_typ
        else type_mismatch false_branch.loc
               "If branches should be of the same type, found %s %s"
               (Types.to_string true_typ) (Types.to_string false_typ)

and check_array types typ exp =
  match Env.find_opt typ types with
  | Some (Tarray (t,_) as res) ->
     check_type t exp;
     res
  | _ -> type_unknown exp.loc "No such array type %s"
           (Symbol.name typ)

and check_type typ typed_exp =
  if typ <> typed_exp.data.typ
  then type_mismatch typed_exp.loc "Expected %s, got %s"
         (Types.to_string typ) (Types.to_string typed_exp.data.typ)
