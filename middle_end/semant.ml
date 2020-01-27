open Types
open Parsing
open Parsetree
open Location

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
     let typ = check_apply vals types func args in
     { expr = (); typ }
  (* Basic operators *)
  | Pexp_binop { op; left; right } ->
     let left' = trans_expr vals types left.data in
     let right' = trans_expr vals types right.data in
     let typ = check_binop types op left' right' in
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
     check_type types var.typ exp;
     { expr = (); typ = Tunit }
  (* If cond *)
  | Pexp_if { test; true_branch; false_branch } ->
     let typ = check_if vals types test true_branch false_branch in
     { expr = (); typ }
  (* While loop *)
  | Pexp_while { test; body } ->
     let test = map ~f:(trans_expr vals types) test in
     let body = map ~f:(trans_expr vals types) body in
     check_type types Tint test;
     check_type types Tunit body;
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
     check_type types Tint size;
     { expr = (); typ }
  | Pexp_let { decs; body } ->
     let vals', types' =
       List.fold_left
         (fun (vals, types) dec -> trans_decs vals types dec)
         (vals, types)
         decs
     in
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
      let typ = Types.real_type types parent.typ in
      match typ with
      | Trecord (fields, _)->
         (match List.assoc_opt symb fields with
          | Some typ -> { expr = (); typ }
          | None -> type_mismatch loc "Record has no such field %s" (Symbol.name symb))
      | _ -> type_mismatch loc "Not a record"
    end
  | Pvar_subscript { data = (var, exp); loc } -> begin
      let parent = trans_var vals types var in
      let index = trans_expr vals types exp in
      let parent_typ = Types.real_type types parent.typ in
      let index_typ = Types.real_type types index.typ in
      match parent_typ, index_typ with
      | Tarray (typ, _), Tint ->
         { expr = (); typ }
      | _ -> type_mismatch loc "Expected to find array"
    end

and trans_decs vals types = function
  | Pdec_var { data; loc } when data.var_typ = None ->
     let typed_expr = trans_expr vals types data.var_init.data in
     if Types.real_type types typed_expr.typ = Tnil
     then type_mismatch loc "Nil variable requires a type constraint";
     let vals' = Env.replace data.var_name.data (Env.Var typed_expr.typ) vals in
     (* Debug 
     print_endline "Dec var:";
     Env.print_typenv types;
     Env.print_valenv vals'; *)
     vals', types
  | Pdec_var { data; loc } ->
     let constr =
       let typ_name = Option.get data.var_typ in
       match Env.find_opt typ_name.data types with
       | Some t -> t
       | None ->
          type_unknown loc "No such type %s" (Symbol.name typ_name.data)
     in
     let typed_expr = trans_expr vals types data.var_init.data in
     let typ = Types.real_type types typed_expr.typ
     and constr_typ = Types.real_type types constr in
     if typ <> constr_typ
     then 
       type_mismatch loc "Can't coerce %s to %s"
         (Types.to_string typed_expr.typ) (Types.to_string constr)
     else
       let vals' = Env.replace data.var_name.data (Env.Var typ) vals in
       (* Debug
       print_newline ();
       print_endline "Dec var:";
       Env.print_typenv types;
       Env.print_valenv vals'; *)
       vals', types
  | Pdec_typ lst ->
     let types' =
       List.fold_left (fun types typdec ->
           let name = typdec.data.typ_name in
           Env.replace name (Talias (name, ref None)) types)
         types
         lst
     in
     let types'' =
       List.fold_left (fun types typdec ->
           let name = typdec.data.typ_name in
           let typ = typdec.data.typ in
           begin match Env.find_opt name types with
           | Some (Talias (_n,r)) ->
              (*if (not (Symbol.equal n name))*)
              r := Some(trans_typ types typ)
           | _ -> ()
           end;
           types)
         types'
         lst
     in
     (* Debug
     print_endline "Dec type:";
     Env.print_typenv types'';
     Env.print_valenv vals; *)
     vals, types''
  | Pdec_fun lst ->
     let vals' =
       List.fold_left
         (fun vals fundec ->
           let v = type_of_function types fundec.data in
           Env.replace fundec.data.fun_name.data v vals)
         vals
         lst
     in
     let _ = List.map
               (fun fundec ->
                 trans_function vals' types fundec.data)
               lst
     in
     (* print_newline ();
     print_endline "Dec fun:";
     Env.print_typenv types;
     Env.print_valenv vals'; *)
     vals', types

and trans_typ types = function
  | Ptyp_alias { data=symb; loc } ->
     begin match Env.find_opt symb types with
     | None -> type_unknown loc "Can't find type for %s"
                 (Symbol.name symb)
     | Some t -> t
     end
  | Ptyp_array { data=symb; loc } ->
     begin match Env.find_opt symb types with
     | None -> type_unknown loc "Can't find type for %s"
                 (Symbol.name symb)
     | Some t -> Tarray (t, unique ())
     end
  | Ptyp_record fields ->
     let fs = List.map (fun field ->
                  match Env.find_opt field.data.field_typ.data types with
                  | None -> type_unknown field.data.field_typ.loc
                              "Unknown field type %s"
                              (Symbol.name field.data.field_typ.data)
                  | Some _ -> field.data.field_name.data,
                              Talias (field.data.field_typ.data, ref None))
                fields
     in
     Trecord (fs, unique ())

and trans_function vals types fundec =
  let args =
    List.map
      (fun param ->
        let { data=symb; loc } = param.data.field_typ in
        let { data=name; loc=_ } = param.data.field_name in
        match Env.find_opt symb types with
        | None ->
           type_unknown loc "Function parameter type %s is unknown"
             (Symbol.name symb)
        | Some t -> name, t)
      fundec.fun_params
  in
  let vals' = List.fold_left (fun vals (name, typ) ->
                  Env.replace name (Env.Var typ) vals)
                vals
                args
  in
  let body = trans_expr vals' types fundec.fun_body.data in
  body
     
and type_of_function types (fundec : Parsetree.fundec) =
  let args =
    List.map
      (fun param ->
        let { data=symb; loc } = param.data.field_typ in
        match Env.find_opt symb types with
        | None ->
           type_unknown loc "Function parameter type %s is unknown"
             (Symbol.name symb)
        | Some t -> t)
      fundec.fun_params
  in
  let return =
    match fundec.fun_result with
    | None -> Tunit
    | Some { data = symb; loc } ->
       match Env.find_opt symb types with
       | None ->
          type_unknown loc "Function return type %s is unknown"
            (Symbol.name symb)
       | Some typ -> typ
  in
  Env.Fun (args, return)

and check_apply vals types func args =
  match Env.find_opt func.data vals with
  | Some (Fun (argtyps, rtyp))
       when (List.length argtyps = List.length args) ->
     List.iter2 (fun typ exp ->
         let exp_typ = exp.data.typ in
         if Types.real_type types typ <> Types.real_type types exp_typ
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
                   
and check_binop types op left right =
  let kind = function
    | Pop_plus | Pop_minus | Pop_times | Pop_divide -> `Arith
    | Pop_and | Pop_or -> `Logic
    | Pop_lt | Pop_le | Pop_gt | Pop_ge -> `Relat
    | Pop_eq | Pop_ne -> `Eq
  in
  let left_type = Types.real_type types left.typ
  and right_type = Types.real_type types right.typ in
  match kind op.data with
  | `Arith | `Logic | `Relat ->
     if left_type = Tint && left_type = right_type
     then Tint
     else type_mismatch op.loc "Arithmetic and logic operations require integer operands"
  | `Eq ->
     if Types.equal types left_type right_type
     then Tint
     else type_mismatch op.loc "Equality requires both operands to have the same type"

and check_record types fields typ =
  match
    Env.find_opt typ.data types
    |> Option.map (Types.real_type types)
  with
  | None ->
     type_mismatch typ.loc "Record type %s is undefined"
       (Symbol.name typ.data)
  | Some (Trecord (ftyps,_) as ret)
       when (List.length ftyps) = (List.length fields) ->
     List.iter2 (fun (field, typ) (name, exp) ->
         let expected = Types.real_type types typ in
         let exp_typ = Types.real_type types exp.data.typ in
         if not (Types.equal types expected exp_typ)
            || not (Symbol.equal field name.data)
         then type_mismatch exp.loc "Record field type mismatch, expected %s, got %s"
                 (Types.to_string expected) (Types.to_string exp_typ))
       ftyps
       fields;
     ret
  | Some (Trecord _) -> type_mismatch typ.loc "Record fields mismatch"
  | _ -> type_mismatch typ.loc "Unknown record type %s"
           (Symbol.name typ.data)

and check_if vals types test true_branch false_branch =
  let test_typ =
    (trans_expr vals types test.data).typ
    |> Types.real_type types
  in
  if test_typ <> Tint
  then type_mismatch test.loc "If condition should have type Int, found exp of type %s"
         (Types.to_string test_typ);
  match false_branch with
  | None ->
     (trans_expr vals types true_branch.data).typ
     |> Types.real_type types
  | Some false_branch ->
     let true_typ =
       (trans_expr vals types true_branch.data).typ
       |> Types.real_type types
     and false_typ =
       (trans_expr vals types false_branch.data).typ
       |> Types.real_type types
     in if Types.equal types true_typ false_typ
        then true_typ
        else type_mismatch false_branch.loc
               "If branches should be of the same type, found %s %s"
               (Types.to_string true_typ) (Types.to_string false_typ)

and check_array types typ exp =
  match Env.find_opt typ types with
  | Some res ->
     begin match Types.real_type types res with
     | Tarray (typ,_) ->
        check_type types typ exp;
        res
     | _ ->
        type_unknown exp.loc "No such array type %s"
          (Symbol.name typ)
     end
  | _ -> type_unknown exp.loc "No such array type %s"
           (Symbol.name typ)

and check_type types typ typed_exp =
  if Types.real_type types typ <> Types.real_type types typed_exp.data.typ
  then type_mismatch typed_exp.loc "Expected %s, got %s"
         (Types.to_string typ) (Types.to_string typed_exp.data.typ)
