module E = Evaluate
exception AutoTry_success
exception No_parser_found_for_this_url
exception Unknown_parser
val parse_parser_definitions_from_files :
  string list -> Parsetreetypes.lang_t list
val parsername_lookup_by_url : string -> (string * string) list -> string
val get_parserdef :
  string ->
  (string * string) list -> (string, 'a) Hashtbl.t -> string option -> 'a
val invoke_parser_on_url :
  string ->
  (string * string) list ->
  (string, Parsetreetypes.parserdef_t) Hashtbl.t ->
  string option -> Parsetreetypes.macrodef_t list -> unit
val main : unit -> unit
