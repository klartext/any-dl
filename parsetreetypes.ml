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
  | Cookies         of Nethttp.netscape_cookie list (* cookies from server / to server *)
  (*
  | Doclist         of Nethtml.document list
  *)
  | Empty
  | Unit            of unit (* type for unit-type operations *)


type specific_selector_t = { tag_sel: string option; argkey_sel: string option; argval_sel: string option }
type selector_t = Selector_any | Specific_selector of specific_selector_t list
type single_extractor_t = [ `Data | `Data_slurp  | `Arg of string | `Tag | `Arg_keys | `Arg_vals | `Arg_pairs | `Dump | `Html_string (*| `Doclist *) ]
type pair_extractor_t   = [ `Arg_keys | `Arg_vals | `Arg_pairs ]
type extractor_t        = Single_extr of single_extractor_t list | Pair_extr of pair_extractor_t


type command_t =
  | Get_url       of string * string          (* url, referrer *)
  | Get_urls  (* can be removed maybe *)      (* get via tmpvar *)
  | Get                                       (* get ONE document via tmpvar (Url-type) *)
  | Post          of results_t list           (* get ONE document via http-post; tmpvar: url; arguments: post-variables *)
  | Download      of results_t list option    (* Download: get-and-save - data will be written to file directly *)
  | Make_url      of (results_t * results_t)  (* create URL-Type-Var from argument-strings *)
  | Make_url_tmpvar                           (* create URL-Type-Var from tmpvar (1-val-stack) *)
  | Match             of string               (* regexp-pattern-string *)
  | Grep              of results_t list       (* grep on the string, constructed by the argument-list *)
  | Grep_v            of results_t list       (* grep -v on the string, constructed by the argument-list *)
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
  | Storematch        of string               (* Store the tmpvar (1-val-stack) as Storematch to a named variable, as MyName.(col).(row) *)
  | Sort                                      (* sort data *)
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
  | CSV_save                                          (* save data as csv-file - filename will be created automatically *)
  | CSV_read          of results_t list               (* read data as csv-data from csv-file *)
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
  | I_Select_match  of int * string * string      (* interactive version of Select_match *)
  | System
  | Basename                                  (* get the basename of a file or url *)
  | Subst      of string * string             (* substitution *)
  | To_string                                 (* convert to string *)
  | To_matchres                               (* convert to matchresult (array) *)
  | Table_to_matchres                         (* parse html-table and create Matcch_reslt from it *)
  | Append_to     of string                   (* Append tmpvar to a known variable (denoted by string (varname)) *)
  | Transpose                                 (* transposes a Match-result (array-array) *)
  | Quote                                     (* wraps '"' around the string in tmpvar *)
  | Exit_parse
  | Dump                                      (* Dump html *)
  | Show_tags                                 (* Show Tags *)
  | Show_tags_fullpath                        (* Show Tags with full tag-path *)
  | Dump_data                                 (* Dump only data-part if html, not the tags *)
  | Html_decode                               (* decode HTML-quotings back to "normal" chars *)
  | Url_decode                                (* decode URL-quoting back to "normal" chars *)
  (* -------------- *)
  | Readline of string option                 (* Read line from stdin *)
  (* -------------- *)
  | Sleep_ms    of int                        (* sleep a certain number of milli-seconds *)
  | Json_prettify                             (* pretty-print a json-string *)
  | Call_macro  of string                     (* call a macro *)
  | Dummy
  | Empty_dummy                               (* dummy-command that creates Empty as result *)



let result_to_string ?(details=false) res =
  let opt_with_details str length = if details then str ^ (" (with " ^ string_of_int length ^ " elements)") else str
  in
  match res with
    | Varname        _ -> "Varname"
    | String         _ -> "String"
    | String_array   a -> opt_with_details "String_array" (Array.length a)
    | Document       _ -> "Document"
    | Document_array a -> opt_with_details "Document_array" (Array.length a)
    | Url            _ -> "Url"
    | Url_list       l -> opt_with_details "Url_list" (List.length l)
    | Url_array      a -> opt_with_details "Url_array"  (Array.length a)
    | Dummy_result     -> "Dummy_result"
    | Match_result   _ -> "Match_result"
    | Cookies        _ -> "Cookies"
    (*
    | Doclist        _ -> "Doclist"
    *)
    | Empty            -> "Empty"
    | Unit           _ -> "Unit"



let arr_len_info arr = Printf.sprintf "(len = %d)" (Array.length arr)
let lst_len_info lst = Printf.sprintf "(len = %d)" (List.length lst)

let result_to_string_with_info  res = match res with
    | Varname        _   -> "Varname"
    | String         _   -> "String"
    | String_array   arr -> "String_array" ^ ( arr_len_info arr )
    | Document       _   -> "Document"
    | Document_array arr -> "Document_array"
    | Url            _   -> "Url"
    | Url_list       lst -> "Url_list" ^ ( lst_len_info lst )
    | Url_array      arr -> "Url_array" ^  ( arr_len_info arr )
    | Dummy_result       -> "Dummy_result"
    | Match_result   mat -> "Match_result" ^ ( Printf.sprintf "(len: %d)" (Array.length mat) )
    | Cookies        _   -> "Cookies"
    (*
    | Doclist        _   -> "Doclist"
    *)
    | Empty              -> "Empty"
    | Unit           _   -> "Unit"



let command_to_string cmd = match cmd with
  | Get_url         _ -> "Get_url"
  | Get_urls          -> "Get_urls" (* can be removed maybe *)
  | Get               -> "Get"
  | Post            _ -> "Post"
  | Download        _ -> "Download"
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
  | Storematch      _ -> "Storematch"
  | Sort              -> "Sort"
  | Uniq              -> "Uniq"
  | Show_variables    -> "Show_variables"
  | List_variables    -> "List_variables"
  | Print             -> "Print"
  | Print_args      _ -> "Print_args"
  | Show_match        -> "Show_match"
  | Print_string    _ -> "Print_string"
  | CSV_save_as     _ -> "CSV_save_as"
  | CSV_save          -> "CSV_save"
  | CSV_read        _ -> "CSV_read"
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
  | Table_to_matchres -> "Table_to_matchres"
  | Append_to       _ -> "Append_to"
  | Transpose         -> "Transpose"
  | Quote             -> "Quote"
  | Exit_parse        -> "Exit_parse"
  | Dump              -> "Dump"
  | Show_tags         -> "Show_tags"
  | Show_tags_fullpath -> "Show_tags_fullpath"
  | Dump_data         -> "Dump_data"
  | Html_decode       -> "Html_decode"        (* decode HTML-quotings back to "normal" chars *)
  | Url_decode        -> "Url_decode"         (* decode url-quoting back to "normal" chars   *)
  | Readline        _ -> "Readline"
  | Sleep_ms        _ -> "Sleep_ms"
  | Json_prettify     -> "Json_prettify"
  | Call_macro      _ -> "Call_macro"
  | Dummy             -> "Dummy"
  | Empty_dummy       -> "EmptyDummy"



type cmd_list = command_t list

type statements_t = Command of command_t | Assignment of string * command_t | Conditional of statements_t list * statements_t list * statements_t list option  | Loop of statements_t list * statements_t list

let statement_type_to_string stmt =
  match stmt with
    | Command     _ -> "Command"
    | Assignment  _ -> "Assignment"
    | Conditional _ -> "Conditional"
    | Loop        _ -> "Loop"


(* Parser( <parser-name>, <url-match-list>, <commands-list> *)
(* ------------------------------------------------------- *)
type parserdef_t = { parsername : string; urllist:  string list; statements:  statements_t list }


(* Macro( <macro-name>, <commands-list> *)
(* ------------------------------------------------------- *)
(* type macrodef_t = { macroname : string; macro_commands:  command_t list } *)
type macrodef_t = string * statements_t list


(* Definitoion of the returnvalue of the Parser-main-function *)
(* ---------------------------------------------------------- *)
type lang_t = Parserdef of parserdef_t | Macrodef of macrodef_t

