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
  | Setvar    of results_t
  | Showvar
  | Dummy




let command_to_string cmd = match cmd with
  | Get_url        _ -> "Get_url"
  | Get_urls       _ -> "Get_urls"
  | Match          _ -> "Match"
  | Select         _ -> "Select"
  | Print            -> "Print"
  | Print_match      -> "Print_match"
  | Print_string   _ -> "Print_string"
  | Save           _ -> "Save"
  | Setvar         _ -> "Setvar"
  | Showvar        _ -> "Showvar"
  | Dummy            -> "Dummy"
  




(* Parser( <parser-name>, <url-match-list>, commands-list> *)
(* ------------------------------------------------------- *)
(*
type parserdef_t = ( string * string list * commands_t list )
*)
type parserdef_t_old = ( string * string list * commands_t list )
type parserdef_t = { parsername : string; urllist:  string list; commands:  commands_t list }


