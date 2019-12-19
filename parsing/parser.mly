%{
    open Asthelpers
    open Parsetree

    let add_dec dec = function
      | [] -> [dec]
      | h::tl as l ->
         match dec, h with
         | Pdec_typ ll, Pdec_typ rl -> Pdec_typ (ll @ rl)::tl
         | Pdec_fun ll, Pdec_fun rl -> Pdec_fun (ll @ rl)::tl
         | Pdec_var ll, Pdec_var rl -> Pdec_var (ll @ rl)::tl
         | _ -> dec::l
    
%}

%token <int>           INT
%token <string>        STR
%token <string>        ID
%token                 FOR WHILE BREAK LET IN NIL TO END
%token                 FUNCTION VAR TYPE ARRAY IF THEN ELSE DO OF
%token                 LPAREN "(" RPAREN ")"
%token                 LBRACK "[" RBRACK "]"
%token                 LBRACE "{" RBRACE "}"
%token                 DOT "." COLON ":" COMMA "," SEMI ";"
%token                 PLUS "+" MINUS "-" TIMES "*" DIV "/"
(*%token                 UMINUS *)
%token                 EQ "=" NE "<>"
%token                 LT "<" LE "<=" GT ">" GE ">="
%token                 AND "&" OR "|"
%token                 ASSIGN ":="
%token                 EOF

%nonassoc DO ASSIGN OF
%nonassoc EQ NE LT LE GT GE
%left PLUS MINUS OR AND TIMES DIV
%right THEN ELSE

%start <Parsetree.exp>    program
%%

program:
  | x=exp EOF  { x }

symbol:
  | t=ID       { symb ~loc:$loc t }

exp_loc:
  | e=exp      { mkloc ~loc:$loc e }

exp:
  | x=var
    { Exp.var ~loc:$loc x }
  | NIL
    { Exp.nil ~loc:$loc }
  | x=INT
    { Exp.int ~loc:$loc x }
  | x=STR
    { Exp.string ~loc:$loc x }
  | func=symbol LPAREN a=args RPAREN
    { Exp.apply func a }
  | x=exp_loc o=binop y=exp_loc
    { Exp.binop o x y }
  | t=symbol LBRACE f=record_fields RBRACE
    { Exp.record f t }
  | LPAREN s=exps RPAREN
    { Exp.seq s }
  | v=var ASSIGN e=exp_loc
    { Exp.assign v e }
  | IF test=exp_loc THEN t=exp_loc
    { Exp._if_single test t }
  | IF test=exp_loc THEN t=exp_loc ELSE f=exp_loc
    { Exp._if test t f }
  | WHILE test=exp_loc DO body=exp_loc
    { Exp._while test body }
  | FOR x=symbol ASSIGN l=exp_loc TO h=exp_loc DO b=exp_loc
    { Exp._for x l h b }
  | BREAK
    { Exp.break ~loc:$loc }
  | LET ds=decs IN es=exp_loc END
    { Exp._let ds es }
  | t=symbol LBRACK n=exp_loc RBRACK OF x=exp_loc
    { Exp.array t n x }

%inline binop:
  | PLUS     { Op.plus ~loc:$loc }
  | MINUS    { Op.minus ~loc:$loc }
  | TIMES    { Op.times ~loc:$loc }
  | DIV      { Op.divide ~loc:$loc }
  | AND      { Op._and ~loc:$loc }
  | OR       { Op._or ~loc:$loc }
  | EQ       { Op.eq ~loc:$loc }
  | NE       { Op.ne ~loc:$loc }
  | LT       { Op.lt ~loc:$loc }
  | GT       { Op.gt ~loc:$loc }
  | LE       { Op.le ~loc:$loc }
  | GE       { Op.ge ~loc:$loc }

exps:
  | es=separated_list(SEMI, exp_loc)
    { es }

var:
  | name=ID varend  { $2(Var.simple ~loc:$loc name) }

varend:
/*empty*/ { fun x -> x }
  | DOT name=ID varend
    { fun v -> $3(Var.field ~loc:$loc v name) }
  | LBRACK ind=exp RBRACK varend
    { fun v -> $4(Var.subscript ~loc:$loc v ind) }

args:
  | l=separated_list(COMMA, exp_loc)
    { l }

record_fields:
  | fs=separated_list(COMMA, n=symbol EQ t=exp_loc { (n, t) })
    { fs }

fields:
  | fs=separated_list(COMMA, n=symbol COLON t=symbol { Dec.field ~loc:$loc n t })
    { fs }

decs:
  | /*empty*/   { [] }
  | ds=decs TYPE t=ID EQ ty=symbol
    { add_dec (Dec.typ_alias ~loc:$loc t ty) ds }
  | ds=decs TYPE t=ID EQ LBRACE f=fields RBRACE
    { add_dec (Dec.typ_record ~loc:$loc t f) ds }
  | ds=decs TYPE t=ID EQ ARRAY OF ty=symbol
    { add_dec (Dec.typ_array ~loc:$loc t ty) ds }
  | ds=decs FUNCTION f=symbol LPAREN p=fields RPAREN t=type_constraint EQ b=exp_loc
    { add_dec (Dec.func ~loc:$loc f p b ?fun_result:t) ds }
  | ds=decs VAR v=symbol t=type_constraint ASSIGN e=exp_loc
    { add_dec (Dec.var ~loc:$loc ?var_typ:t v e) ds }

type_constraint:
 | c=option(COLON t=ID { symb ~loc:$loc(t) t })  { c }

