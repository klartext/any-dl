(*
type var_t = Variable.variable
*)


(*
type int_t    = Intlist of int list       | Int of int
type string_t = Stringlist of string list | String of string

type list_t  = Ilist of int | Slist of string
*)



(* internal functions get thier own type; user-functions are more general *)
(*
type function_type = Function of string

  and
*)


(*
type basic_t = String of string | Int of int | Bool of bool
*)


type unary_op  =  CHANGE_SIGN | NOT
type binary_op =  PLUS | MINUS | MULT | DIV | EQUALS | AND | OR | GT | ST


type lnum_t = int (* line number *)

type expr_t =
    | StringValue of string (* terminal string value *)
    | IntValue    of int    (* terminal int value *)
    | BoolValue   of bool   (* terminal bool value *)

    | StringList  of string list (* terminal string value *)
    | IntList     of int    list (* terminal int value *)
    | BoolList    of bool   list (* terminal bool value *)

    | DocumentList of ( string * string ) list (* (document, url) list *)

    | Varname     of string (* variable name (functions will be handled seperatedly) *)
    | Void
    
    | ExprUnary   of unary_op * expr_t
    | ExprBinary  of expr_t * binary_op * expr_t
    | FuncCall    of string * expr_t list (* string: func-name allowed => checking later? *)

    | Error






type param_t = (string * string) list

type statement_t =
    | Assignment of string * expr_t  (* eigentlich nur Varname => weitere Prüfung!*)   (* auch Funktioinsdefinitionen? oder diese separat? *)
    | Condition  of expr_t * statement_t * statement_t option (* expr_t dürfte eigentlich nur BOOL sein! => Typechecking *)
    | Loop    (* noch nicht implementiert, der Kram *)
    | Unconditional of expr_t (* kann eigentlich raus, da expr in imperativer Sprache quatsch. *)



type parse_snippet_t =
    | Statement of statement_t
    | Funcdef   of string * param_t * statement_t list




