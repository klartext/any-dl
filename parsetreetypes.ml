type match_result_t = string array array
type selector_t     = ( match_result_t -> match_result_t ) (* function, that has a certain algorithm to select certain match_result_t *)


type results_t =
  | Document of string (* hier könnte noch der Referrer ergänzt werden *)
  | Url      of string * string  (* needed for Get-command *)
  | Url_list of (string * string) list
  | Dummy_result
  | Match_result of match_result_t
  | Empty


type commands_t =
  | Get_url       of string * string          (* url, referrer *)
  | Get_urls                                  (* get via tmpvar *)
  | Get                                       (* get ONE document via tmpvar (Url-type) *)
  | Match     of string                       (* regexp-pattern-string *)
  | Select    of selector_t                   (* acts as a filter *)
  | Print
  | Print_match
  | Print_string of string
  | Save      of string * string
  | Dummy
  | Setvar    of results_t
  | Showvar




(* Parser( <parser-name>, <url-match-list>, commands-list> *)
type parserdef_t = Parser of ( string * string list * commands_t list )



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


(* OOOOLD


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


*)
