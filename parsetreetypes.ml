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
(*
type specific_selector_t     = ( match_result_t -> match_result_t ) (* function, that has a certain algorithm to select certain match_result_t *)
*)


type results_t =
  | Varname         of string                       (* name of a variable *)
  | String          of string                       (* general purpose string *)
  | String_array    of string array
  | Document        of  string * string             (* (document, url-of-doc) *)
  | Document_array  of (string * string) array      (* (document, url-of-doc) array *)
  | Url             of  string * string             (* (url, referrer) needed for Get-command *)
  | Url_list        of (string * string) list
  | Url_array       of (string * string) array
  | Dummy_result
  | Match_result    of match_result_t
  (*
  | Doclist         of Nethtml.document list
  *)
  | Empty


type specific_selector_t = { tag_sel: string option; argkey_sel: string option; argval_sel: string option }
type selector_t = Selector_any | Specific_selector of specific_selector_t list
type single_extractor_t = [ `Data | `Data_slurp  | `Arg of string | `Tag | `Arg_keys | `Arg_vals | `Arg_pairs | `Dump | `Html_string (*| `Doclist *) ]
type pair_extractor_t   = [ `Arg_keys | `Arg_vals | `Arg_pairs ]
type extractor_t        = Single_extr of single_extractor_t list | Pair_extr of pair_extractor_t


type commands_t =
  | Get_url       of string * string          (* url, referrer *)
  | Get_urls  (* can be removed maybe *)      (* get via tmpvar *)
  | Get                                       (* get ONE document via tmpvar (Url-type) *)
  | Make_url      of (results_t * results_t)  (* create URL-Type-Var from argument-strings *)
  | Make_url_tmpvar                           (* create URL-Type-Var from tmpvar (1-val-stack) *)
  | Match             of string               (* regexp-pattern-string *)
  | Grep              of string               (* grep on the pattern-string *)
  | Grep_v            of string               (* grep -v on the pattern-string *)
  | Select            of int                  (* index-list for item-selection *)
  | MSelect           of int list             (* index-list for MULTIPLE item-selection *)
  | Link_extract                              (* extracts html-href's from webpage *)
  | Link_extract_xml                          (* extracts html-href's from (xml-formatted) webpage (e.g. asx-files) *)
  | Title_extract                             (* extracts the title-tag's text part *)
  | Rebase                                    (* rebases the tmpvar to STARTURL as base *)
  | Tag_select        of selector_t * extractor_t (* extracts tags and parts of it from the document *)
  | Paste             of results_t list       (* paste together strings *)
  | Store             of string               (* Store the tmpvar (1-val-stack) to a named variable *)
  | Recall            of string               (* Recall a named variable and store it back to the tmpvar (1-val-stack) *)
  | Delete            of string               (* Delete a named variable (remove it from the varmap *)
  | Uniq                                      (* throw out multiple data *)
  | Show_variables                            (* print all named variables (their names and their contents) *)
  | List_variables                            (* print only names of named variables *)
  (*
  | Select            of specific_selector_t           (* acts as a filter *) (* old ideas from other tool *)
  *)
  | Print
  | Print_args        of results_t list       (* printing the general args-list *)
  | Show_match
  | Print_string of string
  | CSV_save_as       of results_t list               (* save data as csv-file *)
  | Save                                      (* save data (filename derived from document-url) *)
  | Save_as           of results_t list       (* save data, explicit filename *)
  | Setvar       of results_t
  | Show_type
  (* -------------- *)
  | ColSelect  of int  (* horizontal selection of a matrix (match-result) *)
  | RowSelect  of int  (* vertical   selection of a matrix (match-result) *)
  (* -------------- *)
  | DropCol    of int  (* drop a column from a matchresult *)
  | DropRow    of int  (* drop a row from a m atchresult *)
  (* -------------- *)
  | Select_match    of int * string  (* selects a match from a match-result-matrix: args: row-index, match-pattern *)
  | I_Select_match  of int * string  (* interactive version of Select_match *)
  | System
  | Basename                                  (* get the basename of a file or url *)
  | Subst      of string * string             (* substitution *)
  | To_string                                 (* convert to string *)
  | To_matchres                               (* convert to matchresult (array) *)
  | Transpose                                 (* transposes a Match-result (array-array) *)
  | Quote                                     (* wraps '"' around the string in tmpvar *)
  | Exit_parse
  | Dump                                      (* Dump html *)
  | Show_tags                                 (* Show Tags *)
  | Show_tags_fullpath                        (* Show Tags with full tag-path *)
  | Dump_data                                 (* Dump only data-part if html, not the tags *)
  | Html_decode                               (* decode HTML-quotings back to "normal" chars *)
  (* -------------- *)
  | Readline of string option                 (* Read line from stdin *)
  (* -------------- *)
  | Sleep_ms    of int                        (* sleep a certain number of milli-seconds *)
  | Dummy



let result_to_string res = match res with
    | Varname        _ -> "Varname"
    | String         _ -> "String"
    | String_array   _ -> "String_array"
    | Document       _ -> "Document"
    | Document_array _ -> "Document_array"
    | Url            _ -> "Url"
    | Url_list       _ -> "Url_list"
    | Url_array      _ -> "Url_array"
    | Dummy_result     -> "Dummy_result"
    | Match_result   _ -> "Match_result"
    (*
    | Doclist        _ -> "Doclist"
    *)
    | Empty            -> "Empty"



let command_to_string cmd = match cmd with
  | Get_url         _ -> "Get_url"
  | Get_urls          -> "Get_urls" (* can be removed maybe *)
  | Get               -> "Get"
  | Make_url        _ -> "Make_url"
  | Make_url_tmpvar   -> "Make_url"
  | Match           _ -> "Match"
  | Grep            _ -> "Grep"
  | Grep_v          _ -> "Grep_v"
  | Select          _ -> "Select"
  | MSelect         _ -> "MSelect"
  | Link_extract      -> "Link_extract"
  | Link_extract_xml  -> "Link_extract_xml"
  | Title_extract     -> "Title_extract"
  | Rebase            -> "Rebase"
  | Tag_select      _ -> "Tag_select"
  | Paste           _ -> "Paste"
  | Store           _ -> "Store"
  | Recall          _ -> "Recall"
  | Delete          _ -> "Delete"
  | Show_variables    -> "Show_variables"
  | List_variables    -> "List_variables"
  | Print             -> "Print"
  | Print_args      _ -> "Print_args"
  | Show_match        -> "Show_match"
  | Print_string    _ -> "Print_string"
  | CSV_save_as     _ -> "CSV_save_as"
  | Save              -> "Save"
  | Save_as         _ -> "Save_as"
  | Setvar          _ -> "Setvar"
  | Show_type         -> "Show_type"
  | ColSelect       _ -> "ColSelect" 
  | RowSelect       _ -> "RowSelect"
  | DropCol         _ -> "DropCol"
  | DropRow         _ -> "DropRow"
  | Select_match    _ -> "Select_match"
  | I_Select_match  _ -> "I_Select_match"
  | System            -> "System"
  | Basename          -> "Basename"
  | Subst           _ -> "Subst"
  | To_string         -> "To_string"
  | To_matchres       -> "To_matchres"
  | Quote             -> "Quote"
  | Exit_parse        -> "Exit_parse"
  | Dump              -> "Dump"
  | Show_tags         -> "Show_tags"
  | Show_tags_fullpath -> "Show_tags_fullpath"
  | Dump_data         -> "Dump_data"
  | Html_decode       -> "Html_decode"        (* decode HTML-quotings back to "normal" chars *)
  | Readline        _ -> "Readline"
  | Sleep_ms        _ -> "Sleep_ms"
  | Dummy             -> "Dummy"




(* Parser( <parser-name>, <url-match-list>, commands-list> *)
(* ------------------------------------------------------- *)
type parserdef_t = { parsername : string; urllist:  string list; commands:  commands_t list }


