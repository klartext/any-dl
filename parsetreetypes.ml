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
  | String    of string                       (* general purpose string *)
  | Document  of string * string              (* (document, url-of-doc) *)
  | Url       of string * string              (* (url, referrer) needed for Get-command *)
  | Url_list  of (string * string) list
  | Url_array of (string * string) array
  | Dummy_result
  | Match_result      of match_result_t
  | Col               of col_t
  | Row               of row_t
  (*
  | Result_selection  of row_t
  *)
  | Empty


type commands_t =
  | Get_url       of string * string          (* url, referrer *)
  | Get_urls                                  (* get via tmpvar *)
  | Get                                       (* get ONE document via tmpvar (Url-type) *)
  | Match             of string               (* regexp-pattern-string *)
  | Select            of int list             (* index-list for item-selection *) (* im Moment wie ColSelect *)
  | Link_extract                              (* extracts html-href's from webpage *)
  | Link_extract_xml                          (* extracts html-href's from (xml-formatted) webpage (e.g. asx-files) *)
  | Paste                                     (* paste together strings *)
  (*
  | Select            of selector_t           (* acts as a filter *)
  *)
  | Print
  | Show_match
  | Print_string of string
  | Save         of string * string
  | Setvar       of results_t
  | Showtype
  | ColSelect  of int  (* horizontal selection of a matrix (match-result) *)
  | RowSelect  of int  (* vertical   selection of a matrix (match-result) *)
  | Dummy



let result_to_string res = match res with
    | String         _ -> "String"
    | Document       _ -> "Document"
    | Url            _ -> "Url"
    | Url_list       _ -> "Url_list"
    | Url_array      _ -> "Url_array"
    | Dummy_result     -> "Dummy_result"
    | Match_result   _ -> "Match_result"
    | Col            _ -> "Col"
    | Row            _ -> "Row"
    | Empty            -> "Empty"



let command_to_string cmd = match cmd with
  | Get_url        _ -> "Get_url"
  | Get_urls       _ -> "Get_urls"
  | Match          _ -> "Match"
  | Select         _ -> "Select"
  | Link_extract     -> "Link_extract"
  | Link_extract_xml -> "Link_extract_xml"
  | Print            -> "Print"
  | Show_match       -> "Show_match"
  | Print_string   _ -> "Print_string"
  | Save           _ -> "Save"
  | Setvar         _ -> "Setvar"
  | Showtype       _ -> "Showtype"
  | ColSelect      _ -> "ColSelect" 
  | RowSelect      _ -> "RowSelect"
  | Dummy            -> "Dummy"
  




(* Parser( <parser-name>, <url-match-list>, commands-list> *)
(* ------------------------------------------------------- *)
type parserdef_t = { parsername : string; urllist:  string list; commands:  commands_t list }


