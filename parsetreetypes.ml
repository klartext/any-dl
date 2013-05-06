(*
  any-dl:
  -------
  Generic Media-Downloader for any kind of Online-Mediathek.

  Author / copyright: Oliver Bandel
  Copyleft:           GNU GENERAL PUBLIC LICENSE  v3 (or higher)
*)


type col_t          = string array     (* Hselect as well as VSelec are represented the same way - late think of it as a row *)
type row_t          = string array     (* Hselect as well as VSelec are represented the same way - late think of it as a row *)
type match_result_t = string array array
type selector_t     = ( match_result_t -> match_result_t ) (* function, that has a certain algorithm to select certain match_result_t *)


type results_t =
  | Varname       of string                       (* name of a variable *)
  | String        of string                       (* general purpose string *)
  | String_array  of string array
  | Document      of  string * string             (* (document, url-of-doc) *)
  | Url           of  string * string             (* (url, referrer) needed for Get-command *)
  | Url_list      of (string * string) list
  | Url_array     of (string * string) array
  | Dummy_result
  | Match_result  of match_result_t
  | Empty


type commands_t =
  | Get_url       of string * string          (* url, referrer *)
  | Get_urls  (* can be removed maybe *)      (* get via tmpvar *)
  | Get                                       (* get ONE document via tmpvar (Url-type) *)
  | Make_url      of (results_t * results_t)  (* create URL-Type-Var from argument-strings *)
  | Make_url_tmpvar                           (* create URL-Type-Var from tmpvar (1-val-stack) *)
  | Match             of string               (* regexp-pattern-string *)
  | Grep              of string               (* grep on the pattern-string *)
  | Select            of int                  (* index-list for item-selection *)
  | MSelect           of int list             (* index-list for MULTIPLE item-selection *)
  | Link_extract                              (* extracts html-href's from webpage *)
  | Link_extract_xml                          (* extracts html-href's from (xml-formatted) webpage (e.g. asx-files) *)
  | Title_extract                             (* extracts the title-tag's text part *)
  | Paste             of results_t list       (* paste together strings *)
  | Store             of string               (* Store the tmpvar (1-val-stack) to a named variable *)
  | Recall            of string               (* Recall a named variable and store it back to the tmpvar (1-val-stack) *)
  | Delete            of string               (* Delete a named variable (remove it from the varmap *)
  | Show_variables                            (* print all named variables (their names and their contents) *)
  | List_variables                            (* print only names of named variables *)
  (*
  | Select            of selector_t           (* acts as a filter *) (* old ideas from other tool *)
  *)
  | Print
  | Print_args        of results_t list       (* printing the general args-list *)
  | Show_match
  | Print_string of string
  | Save         of string * string
  | Setvar       of results_t
  | Show_type
  | ColSelect  of int  (* horizontal selection of a matrix (match-result) *)
  | RowSelect  of int  (* vertical   selection of a matrix (match-result) *)
  | Select_match    of int * string  (* selects a match from a match-result-matrix: args: row-index, match-pattern *)
  | I_Select_match  of int * string  (* interactive version of Select_match *)
  | System
  | Basename                                  (* get the basename of a file or url *)
  | Subst      of string * string             (* substitution *)
  | To_string                                 (* convert to string *)
  | Quote                                     (* wraps '"' around the string in tmpvar *)
  | Exit_parse
  | Dump                                      (* Dump html *)
  | Show_tags                                 (* Show Tags *)
  | Dump_data                                 (* Dump only data-part if html, not the tags *)
  | Dummy



let result_to_string res = match res with
    | Varname        _ -> "Varname"
    | String         _ -> "String"
    | String_array   _ -> "String_array"
    | Document       _ -> "Document"
    | Url            _ -> "Url"
    | Url_list       _ -> "Url_list"
    | Url_array      _ -> "Url_array"
    | Dummy_result     -> "Dummy_result"
    | Match_result   _ -> "Match_result"
    | Empty            -> "Empty"



let command_to_string cmd = match cmd with
  | Get_url         _ -> "Get_url"
  | Get_urls        _ -> "Get_urls" (* can be removed maybe *)
  | Get             _ -> "Get"
  | Make_url        _ -> "Make_url"
  | Make_url_tmpvar _ -> "Make_url"
  | Match           _ -> "Match"
  | Grep            _ -> "Grep"
  | Select          _ -> "Select"
  | MSelect         _ -> "MSelect"
  | Link_extract      -> "Link_extract"
  | Link_extract_xml  -> "Link_extract_xml"
  | Paste           _ -> "Paste"
  | Store           _ -> "Store"
  | Recall          _ -> "Recall"
  | Delete          _ -> "Delete"
  | Show_variables  _ -> "Show_variables"
  | List_variables  _ -> "List_variables"
  | Print             -> "Print"
  | Print_args      _ -> "Print_args"
  | Show_match        -> "Show_match"
  | Print_string    _ -> "Print_string"
  | Save            _ -> "Save"
  | Setvar          _ -> "Setvar"
  | Show_type       _ -> "Show_type"
  | ColSelect       _ -> "ColSelect" 
  | RowSelect       _ -> "RowSelect"
  | Select_match    _ -> "Select_match"
  | I_Select_match  _ -> "I_Select_match"
  | System          _ -> "System"
  | Basename        _ -> "Basename"
  | Subst           _ -> "Subst"
  | To_string       _ -> "To_string"
  | Quote           _ -> "Quote"
  | Exit_parse      _ -> "Exit_parse"
  | Dump              -> "Dump"
  | Dump_data         -> "Dump_data"
  | Dummy             -> "Dummy"




(* Parser( <parser-name>, <url-match-list>, commands-list> *)
(* ------------------------------------------------------- *)
type parserdef_t = { parsername : string; urllist:  string list; commands:  commands_t list }


