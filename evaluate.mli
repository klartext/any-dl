type col_t = string array
type row_t = string array
type match_result_t = string array array
type results_t =
  Parsetreetypes.results_t =
    Varname of string
  | String of string
  | String_array of string array
  | Document of string * string
  | Document_array of (string * string) array
  | Url of string * string
  | Url_list of (string * string) list
  | Url_array of (string * string) array
  | Dummy_result
  | Match_result of match_result_t
  | Cookies of Nethttp.netscape_cookie list
  | Empty
  | Unit of unit
type specific_selector_t =
  Parsetreetypes.specific_selector_t = {
  tag_sel : string option;
  argkey_sel : string option;
  argval_sel : string option;
}
type selector_t =
  Parsetreetypes.selector_t =
    Selector_any
  | Specific_selector of specific_selector_t list
type single_extractor_t =
    [ `Arg of string
    | `Arg_keys
    | `Arg_pairs
    | `Arg_vals
    | `Data
    | `Data_slurp
    | `Dump
    | `Html_string
    | `Tag ]
type pair_extractor_t = [ `Arg_keys | `Arg_pairs | `Arg_vals ]
type extractor_t =
  Parsetreetypes.extractor_t =
    Single_extr of single_extractor_t list
  | Pair_extr of pair_extractor_t
type command_t =
  Parsetreetypes.command_t =
    Get_url of string * string
  | Get_urls
  | Get
  | Post of results_t list
  | Download of results_t list option
  | Make_url of (results_t * results_t)
  | Make_url_tmpvar
  | Match of string
  | Grep of results_t list
  | Grep_v of results_t list
  | Select of int
  | MSelect of int list
  | Link_extract
  | Link_extract_xml
  | Title_extract
  | Rebase
  | Tag_select of selector_t * extractor_t
  | Paste of results_t list
  | Store of string
  | Recall of string
  | Delete of string
  | Storematch of string
  | Sort
  | Uniq
  | Show_variables
  | List_variables
  | Print
  | Print_args of results_t list
  | Show_match
  | Print_string of string
  | CSV_save_as of results_t list
  | CSV_save
  | CSV_read of results_t list
  | Save
  | Save_as of results_t list
  | Setvar of results_t
  | Show_type
  | ColSelect of int
  | RowSelect of int
  | DropCol of int
  | DropRow of int
  | Select_match of int * string
  | I_Select_match of int * string * string
  | System
  | Basename
  | Subst of string * string
  | To_string
  | To_matchres
  | Table_to_matchres
  | Append_to of string
  | Transpose
  | Quote
  | Exit_parse
  | Dump
  | Show_tags
  | Show_tags_fullpath
  | Dump_data
  | Html_decode
  | Url_decode
  | Readline of string option
  | Sleep_ms of int
  | Json_prettify
  | Call_macro of string
  | Dummy
  | Empty_dummy
val result_to_string : ?details:bool -> results_t -> string
val arr_len_info : 'a array -> string
val lst_len_info : 'a list -> string
val result_to_string_with_info : results_t -> string
val command_to_string : command_t -> string
type cmd_list = command_t list
type statements_t =
  Parsetreetypes.statements_t =
    Command of command_t
  | Assignment of string * command_t
  | Conditional of statements_t list * statements_t list *
      statements_t list option
  | Loop of statements_t list * statements_t list
val statement_type_to_string : statements_t -> string
type parserdef_t =
  Parsetreetypes.parserdef_t = {
  parsername : string;
  urllist : string list;
  statements : statements_t list;
}
type macrodef_t = string * statements_t list
type lang_t =
  Parsetreetypes.lang_t =
    Parserdef of parserdef_t
  | Macrodef of macrodef_t
exception NOT_IMPLEMENTED_SO_FAR
exception Value_conversion_unknown
exception No_document_found
exception Tagselect_empty_list
exception No_Match
exception No_Matchresult_available
exception No_Matchable_value_available
exception Html_decode_error
exception Wrong_tmpvar_type
exception Wrong_argument_type
exception Conversion_error
exception Invalid_Row_Index
exception Invalid_Col_Index
exception Extractor_list_failure
exception Variable_not_found of string
exception Parse_exit
exception Csv_read_error of string
module Varmap :
  sig
    module type Variablemap_slim =
      sig
        type key = String.t
        type 'a t = 'a Map.Make(String).t
        val empty : 'a t
        val mem : key -> 'a t -> bool
        val add : key -> 'a -> 'a t -> 'a t
        val remove : key -> 'a t -> 'a t
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val find : key -> 'a t -> 'a
      end
    module Variablemap : Variablemap_slim
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val remove : key -> 'a t -> 'a t
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val exists : Variablemap.key -> 'a Variablemap.t -> bool
    val find : Variablemap.key -> results_t Variablemap.t -> results_t
    val find_excdef :
      Variablemap.key -> results_t Variablemap.t -> results_t -> results_t
  end
type varmap_t = results_t Varmap.Variablemap.t
type command_fun_res_t = results_t * varmap_t
val boil_down : results_t -> results_t
val to_string : results_t -> varmap_t -> string
val urlify : results_t -> varmap_t -> results_t
val interactive_string_select : string array -> string -> string
val paste_arglist_to_string : results_t list -> varmap_t -> string
val default_application :
  results_t -> (string -> string) -> varmap_t -> results_t
val var_is_empty : results_t -> varmap_t -> bool
val post_document :
  string ->
  string ->
  (string * string) list ->
  varmap_t -> (string * string * results_t Varmap.t) option
val get_document :
  string ->
  string -> varmap_t -> (string * string * results_t Varmap.t) option
val download :
  string -> string -> string -> varmap_t -> results_t Varmap.t option
val get_document_list :
  (string * string) list -> varmap_t -> (string * string) list * varmap_t
val evaluate_statement :
  statements_t list ->
  macrodef_t list -> results_t -> varmap_t -> results_t * varmap_t
val command :
  command_t list ->
  macrodef_t list -> results_t -> varmap_t -> results_t * varmap_t
val evaluate_statement_list :
  statements_t list -> macrodef_t list -> results_t * varmap_t
