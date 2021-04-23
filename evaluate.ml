(*
  any-dl:
  -------
  Generic Media-Downloader for any kind of Online-Mediathek.
  Attempts to be the general tool, doing things like
  youtube-dl, zdf-dl, arte-dl and so on.


  Author / copyright: Oliver Bandel
  Copyleft: GNU GENERAL PUBLIC LICENSE  v3 (or higher)
*)


include Parsetreetypes
open Tools


exception NOT_IMPLEMENTED_SO_FAR (* for planned, but not already implemented functionality *)

exception Value_conversion_unknown  (* type conversion, that can't handle this special item (similar to "Wrong_tmpvar_type") *)

exception No_document_found             (* a dcoument could not be retrieved *)
exception Tagselect_empty_list          (* tagselect gives back an empty list *)
exception No_Match                      (* if a match was tried, but no match could be found *)
exception No_Matchresult_available      (* if Select is used, but there is no match-result available as tmpvar *)
exception No_Matchable_value_available  (* if Match is used, but there is no matchable tmpvar *)

exception Html_decode_error             (* A problem occured while trying to use html-decode *)

exception Wrong_tmpvar_type             (* if tmpvar has just the wrong type... without more detailed info *)
exception Wrong_argument_type           (* e.g. Show_match on non-match *)

exception Conversion_error              (* for example: string (correct type) for json_prettify is not a json-string (wrong content) *)

exception Invalid_Row_Index             (* indexing a row that does not exist *)
exception Invalid_Col_Index             (* indexing a col that does not exist *)

exception Extractor_list_failure

exception Variable_not_found of string  (* a variable-name lookup in the Varname-map failed *)


exception Parse_exit                    (* user exit of a parse *)

exception Csv_read_error of string            (* csv_read-command: error *)





(* Module for Variables *)
(* -------------------- *)
module Varmap =
  struct
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

    module Variablemap = ( Map.Make( String ) : Variablemap_slim )

    include Variablemap

    (* these must be included in output signature
    let empty  = Varmap.empty
    let add    = Varmap.add
    let remove = Varmap.remove
    let iter   = Varmap.iter
    *)

    let exists key map =
      match key with
        | "NOW" -> true
        | _     -> Variablemap.mem key map

    let find varname varmap =
      match varname with
        | "NOW" -> let open Unix in String( time () |> int_of_float |> string_of_int )
        | _     -> try Variablemap.find varname varmap with Not_found -> raise (Variable_not_found varname)

    (* find with an exception-default value                                               *)
    (* This function allows to set a default value in case the lookup yields in Not_found *)
    (* ---------------------------------------------------------------------------------- *)
    let find_excdef varname varmap default =
      match varname with
        | "NOW" -> let open Unix in String( string_of_float ( time() ) )
        | _     -> try Variablemap.find varname varmap with Not_found -> default

  end

type varmap_t = results_t Varmap.Variablemap.t

type command_fun_res_t = results_t * varmap_t




(* ================================================================ *)
(* this function "boils down" a aggregation-tmpvar to simpler types *)
(* For example: A Url_list of length 1 is transfomed to url         *)
(* For example: A String_list of length 1 is transfomed to String   *)
(* and so forth.                                                    *)
(* ---------------------------------------------------------------- *)
(* boil_down is an un-aggregate                                     *)
(* ================================================================ *)
let boil_down value =
  match value with
    | Document_array  arr        -> if Array.length arr      = 1 then ( Document ((fst arr.(0)), (snd arr.(0))) ) else value
    | String_array    str_arr    -> if Array.length str_arr  = 1 then ( String str_arr.(0) ) else value
    | Url_list        url_list   -> if List.length url_list  = 1  then  ( let (u,r) = List.hd url_list in Url (u,r) ) else value
    | Url_array       url_arr    -> if Array.length url_arr  = 1  then  ( let (u,r) = url_arr.(0) in Url (u,r) ) else value
    | _                          -> value (* value of all other types will not be changed *)


(* ---------------------------------------------- *)
(* functional, not thorough nifty-details printer *)
(* intended to make basic functionality working   *)
(* more fancy converters for other purposes might *)
(* elsewehere be defined                          *)
(* ---------------------------------------------- *)
let rec  to_string  result_value (varmap : varmap_t) =

  let array_string_append ?(sep="")  str_arr = Array.fold_left ( fun left right -> left ^ sep ^ right ) "" str_arr |> String.trim in
  let list_string_append str_lst  = List.fold_left  ( ^ ) "" str_lst in


  let str =
    match result_value with
      | Varname       varname      -> let res = (Varmap.find varname varmap) in
                                      begin
                                        match res with
                                          | String str -> str
                                          | _ as again -> to_string again varmap
                                      end
      | String        str          -> str 
      | Document      (doc, url)   -> url ^ ":" ^ doc
      | Document_array  arr        -> let strarr = Array.map ( fun (d,u) -> to_string (Document (d,u)) varmap ) arr in to_string (String_array strarr) varmap
      | String_array  str_arr      -> array_string_append str_arr
      | Match_result  mres         -> let rows = Array.map ( fun columns -> array_string_append ~sep:" " columns ) mres in array_string_append ~sep:"\n" rows
      | Url           (href, ref)  -> href
      | Url_list      url_list     -> List.fold_right ( fun a sofar -> "\"" ^ (fst a) ^ "\" " ^ sofar ) url_list ""
      | Url_array     url_arr      -> to_string  (Url_list ( Array.to_list url_arr)) varmap
      | Empty                      -> ""
      (*
      | Doclist       dl           -> Parsers.convert_doclist_to_htmlstring dl
      *)
      | Cookies        clist       -> list_string_append ( List.map Tools.cookie_to_string clist )
      | Dummy_result               -> ""
      (*
      *)
      | _ -> print_warning "to_string-function found non-convertable type"; raise Wrong_argument_type (* just in case more cases will be added *)

  in
    str


(* ---------------------------------------------- *)
(* Convert to Urls/Url-arrays and so on.          *)
(* Should replace "to_string", if Url is wanted.  *)
(* ---------------------------------------------- *)
let rec  urlify  result_value (varmap : varmap_t) =
  let make_referrer () = to_string ( Varmap.find_excdef "REFERRER" varmap (String "") ) varmap in
  let converted =
    match result_value with
      | Varname       varname      -> let res = (Varmap.find varname varmap) in
                                      urlify res varmap
      | String        str          -> Url(str, make_referrer() )
      | Document      (doc, url)   -> Url ( url, "-" )
      | Document_array  arr        -> raise Value_conversion_unknown
                      (*
                      let strarr = Array.map ( fun (d,u) -> to_string (Document (d,u)) varmap ) arr in to_string (String_array strarr) varmap
                      *)
      | String_array  str_arr      -> Url_array( Array.map (fun str -> (str, make_referrer()) ) str_arr )
      | Match_result  mres         -> let liste = ref [] in
                                      let referrer = make_referrer() in 
                                      Array.iter( fun x -> Array.iter ( fun elem -> liste := (elem, referrer) :: !liste ) x ) mres; (* extract elements to liste *)
                                      Url_list !liste
      | Url           (href, ref)  -> Url (href, ref)
      | Url_list      url_list     -> Url_list      url_list
      | Url_array     url_arr      -> Url_array     url_arr
      | Empty                      -> raise Wrong_argument_type
      | Dummy_result               -> raise Wrong_argument_type
      (*
      *)
      | _ -> print_warning "urlify-function found non-convertable type"; raise Wrong_argument_type (* just in case more cases will be added *)

  in
    boil_down converted


(* Menue to select an item from a string-list; accepts only valid inputs *)
(* The return value is the selected value itself (not an index)          *)
(* --------------------------------------------------------------------- *)
(* If selected option can't be converted to int, the default pattern is  *)
(* selected as answer.                                                   *)
(* --------------------------------------------------------------------- *)
let interactive_string_select str_arr default_pattern =
  let rec loop str_arr = 
    print_string "\n";
    print_string "Please chose one option:\n\n";
    Array.iteri ( fun idx str -> Printf.printf "  %2d.: %s\n" idx str ) str_arr;
    print_string "\n   ===> ? ";
    try
      let value = int_of_string( read_line() ) in
      if value >= 0 && value < Array.length str_arr
      then str_arr.(value)
      else loop str_arr
    with _ -> default_pattern
  in
    loop str_arr


(* Pasting an Argument-list, converting to native string *)
(* ===================================================== *)
(* Used by the paste-command, as well as others.         *)
(* ----------------------------------------------------- *)
let paste_arglist_to_string  argument_list  (varmap : varmap_t) =
  let str_lst = List.map (fun item ->  to_string item varmap) argument_list in (* convert to string  *)
  let pasted  = List.fold_left ( ^ ) "" str_lst in                             (* append all strings *)
  pasted





(* =================================================================== *)
(* apply a function to the value, giving back a value of the same type *)
(* =================================================================== *)
let default_application variable basefunc (varmap : varmap_t) =
  match variable with
    (*
    | Varname         vn         -> to_string (Varmap.find vn varmap) varmap
    *)
    | String          str        -> String ( basefunc str )
    | String_array    strarr     -> String_array ( Array.map basefunc strarr )
    | Document        (doc,url)  -> Document ( basefunc doc, basefunc url )
    | Document_array  docarr     -> Document_array ( Array.map ( fun (doc,url) -> basefunc doc, basefunc url ) docarr )
    | Url             (url, ref) -> Url ( basefunc url, basefunc ref )
    | Url_list        urllist    -> Url_list ( List.map ( fun (u,r) -> basefunc u, basefunc r ) urllist )
    | Url_array       urlarray   -> Url_array ( Array.map ( fun (u,r) -> basefunc u, basefunc r ) urlarray )
    | Dummy_result               -> Empty
    | Match_result    arrarr     -> let res = Array.map (fun arr -> let a = Array.copy arr in Array.map basefunc a ) arrarr in
                                    (Match_result res)
    (*
    | Match_result               -> Empty
    | Cookies                    -> Empty
    *)
    | Empty                     -> Empty
    | _                         -> raise Wrong_argument_type



(* ========================================================================================== *)
(* evaluates to true, if the value is interpreted as being empty, or false, if it's not empty *)
(* ========================================================================================== *)
let rec var_is_empty value (varmap : varmap_t) =
  match value with
    | Varname         vn         -> var_is_empty (Varmap.find vn varmap) varmap
    | String          str        -> if str = "" then true else false
    | String_array    strarr     -> if Array.length strarr   = 0 then true else false
    | Document        (doc,url)  -> if doc = "" then true else false
    | Document_array  docarr     -> if Array.length docarr   = 0 then true else false (* also if all Docs are empty ? *)
    | Url             (url, ref) -> if url = "" then true else false
    | Url_list        urllist    -> if List.length urllist   = 0 then true else false
    | Url_array       urlarray   -> if Array.length urlarray = 0 then true else false
    | Dummy_result               -> false (* Dummy_result is to have a (dummy) result; so it is seen as non-empty. Empty is the opposite *)
    | Match_result    arrarr     -> if Array.length arrarr   = 0 then true else false
    | Cookies         cl         -> if List.length cl        = 0 then true else false
    | Empty                      -> true
    | Unit            _          -> true



(* "post_document"-function, is used by Post-command *)
(* ================================================= *)
let post_document  url referrer post_params (varmap : varmap_t) =

  (* if a cookie already has been received/stored                *)
  (* pick it from the variable-map for sending it back to server *)
  (* ----------------------------------------------------------- *)
  let send_cookie = if Varmap.exists "COOKIES.SEND" varmap
                   then
                     begin match  Varmap.find "COOKIES.SEND" varmap  with Cookies cook -> Some cook | _ -> None end
                   else
                     None
  in

  (* retrvieve the document *)
  (* ---------------------- *)
  let document_and_cookies =  Network.Pipelined.post url (Some referrer) post_params send_cookie in

  begin
   match document_and_cookies with
     | None                -> None
     | Some (doc, cookies) ->
                              let new_varmap = (Varmap.add "COOKIES.RECEIVED" (Cookies cookies) varmap) in
                              Some (doc, url, new_varmap)
  end



let rec get_document  url referrer (varmap : varmap_t) =

  (* if a cookie already has been received/stored                *)
  (* pick it from the variable-map for sending it back to server *)
  (* ----------------------------------------------------------- *)
  let send_cookie = if Varmap.exists "COOKIES.SEND" varmap
                   then
                     begin match  Varmap.find "COOKIES.SEND" varmap  with Cookies cook -> Some cook | _ -> None end
                   else
                     None
  in

  (* retrvieve the document *)
  (* ---------------------- *)
  let document_and_cookies =  Network.Pipelined.get url (Some referrer) send_cookie in

  begin
   match document_and_cookies with
     | None                -> None
     | Some (doc, cookies) ->
                              let new_varmap = (Varmap.add "COOKIES.RECEIVED" (Cookies cookies) varmap) in
                              Some (doc, url, new_varmap)
  end
  

(* "download"-function, is for downloading big files firectly into a destination-file *)
(* ================================================================================== *)
and download  url referrer  dst_file  (varmap : varmap_t) =

  (* if a cookie already has been received/stored                *)
  (* pick it from the variable-map for sending it back to server *)
  (* ----------------------------------------------------------- *)
  let send_cookie = if Varmap.exists "COOKIES.SEND" varmap
                   then
                     begin match  Varmap.find "COOKIES.SEND" varmap  with Cookies cook -> Some cook | _ -> None end
                   else
                     None
  in

  (* retrvieve the document *)
  (* ---------------------- *)
  let response_cookies = Network.Pipelined.get_download url (Some referrer) send_cookie dst_file in

  begin
   match response_cookies with
     | None         -> None
     | Some cookies ->
                       let new_varmap = (Varmap.add "COOKIES.RECEIVED" (Cookies cookies) varmap) in
                       Some new_varmap
  end


(* ====================================================================================== *)
(* get_document_list: gets a list of documents (bulk-get)                                 *)
(* if the sleep-time is set o values > 0, then each get waits this amoubt of milliseconds *)
(* before the get is executed.                                                            *)
(* ====================================================================================== *)
(* at the moment, varmap is not returned  updated *)
(* ---------------------------------------------- *)
and get_document_list  urls_refs (varmap : varmap_t) =
  let rec aux urllist (result, vmap) =
    if Cli.opt.Cli.ms_sleep > 0 then Sleep.sleep_ms Cli.opt.Cli.ms_sleep; (* call sleep-function if > 0 ms to wait *)
    match urllist with
      | []        -> result, vmap
      | (u,r)::tl -> begin
                       match get_document u r vmap with
                         | None                  -> Printf.eprintf "no document found for %s\n" u;
                                                    aux tl (result, vmap)
                         | Some (doc, ref, new_varmap) -> aux tl ( (doc,ref)::result, new_varmap )
                     end
    in
      aux urls_refs ([], varmap)


(* ======================================================== *)
and evaluate_statement (statement_list : statements_t list) (macrodefs_lst : macrodef_t list) tmpvar (varmap : varmap_t) : results_t * varmap_t =

  (* For -vv print command name to stdout *)
  (* ==================================== *)
  if Cli.opt.Cli.very_verbose then Printf.printf "Length of statement_list: %d\n" (List.length statement_list);

  if Cli.opt.Cli.very_verbose
  then
    begin
      try
        let str = Parsetreetypes.statement_type_to_string (List.hd statement_list) in
        Printf.printf "evaluate_statement: *** %s ***\n" str;
      with Failure _ -> () (* catches List.hd [] *)
    end;
  match statement_list with
    | []            -> tmpvar, varmap
    | statement::tl ->
                      let res, newvarmap =
                        begin
                          match statement with
                            | Command     cmd                                        -> command  [ cmd ] macrodefs_lst tmpvar varmap
                            | Assignment  (varname, cmd)                             -> command  ( cmd :: [ (Store varname) ] ) macrodefs_lst tmpvar varmap

                            | Conditional  (if_stmtlist, then_stmtlist, else_stmtlist_opt) -> let testresult =  fst ( evaluate_statement  if_stmtlist macrodefs_lst tmpvar varmap ) in
                                                                                      (* if the variable has Unit-type, then it is always true-empty *)
                                                                                      if not ( var_is_empty testresult varmap )
                                                                                      then evaluate_statement then_stmtlist macrodefs_lst tmpvar varmap
                                                                                      else
                                                                                      begin
                                                                                        match else_stmtlist_opt with
                                                                                          | None               -> evaluate_statement [] macrodefs_lst tmpvar varmap
                                                                                          | Some else_commands -> evaluate_statement else_commands macrodefs_lst tmpvar varmap
                                                                                      end

                            | Loop        (test_cmdlst, todo_cmdslst)               -> while ( not ( var_is_empty (fst ( evaluate_statement test_cmdlst macrodefs_lst tmpvar varmap )) varmap ) )
                                                                                       do
                                                                                        Unit ( ignore ( evaluate_statement todo_cmdslst macrodefs_lst tmpvar varmap ) ), varmap
                                                                                       done; Unit (), varmap

                                                                                      (* if the variable has Unit-type, then it is always true-empty => possibly endless loop! *) (* TODO ! *)
                        end
                      in
                      evaluate_statement tl  macrodefs_lst res newvarmap (* result of the command-calls is new tmpvar *)


(* ======================================================== *)
and     command commandlist macrodefs_lst tmpvar varmap  :  results_t * varmap_t =
  flush_all();

  (* For -vv print command name to stdout *)
  (* ==================================== *)
  if Cli.opt.Cli.very_verbose
  then
    begin
      try
        let str = Parsetreetypes.command_to_string (List.hd commandlist) in
        print_endline ("_Command_ " ^ str)
      with Failure _ -> () (* catches List.hd [] *)
    end;

  match commandlist with
    | []        -> tmpvar, varmap (* Printf.printf "<========================== BACK. Leave evaluate_statement_list() now!\n"*)
    | cmd::tl   -> begin
                     match cmd with
                       | Post  fname_arglist              -> cmd_post commandlist macrodefs_lst tmpvar varmap cmd tl fname_arglist
                       | Download  fname_arglist_opt      -> cmd_download commandlist macrodefs_lst tmpvar varmap cmd tl fname_arglist_opt
                       | Get_url (url, referrer)          -> cmd_get_url commandlist macrodefs_lst tmpvar varmap cmd tl (url, referrer)
                       | Get                              -> cmd_get commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Get_urls                         -> cmd_get_urls commandlist macrodefs_lst tmpvar varmap cmd tl


                         (* creates url and puts it into tmpvar *)
                       | Make_url_tmpvar -> command tl macrodefs_lst (urlify tmpvar varmap) varmap
                                            (*
                                            let (url, referrer) = (to_string tmpvar varmap, "-") in
                                            command tl macrodefs_lst (Url( url, referrer)) varmap
                                            urlify tmpvar varmap
                                            *)

                       | Make_url (u,r)  -> let (url, referrer) = (to_string u varmap, to_string r varmap) in
                                            command tl macrodefs_lst (Url( url, referrer)) varmap

                       | Match   pattern            -> cmd_match commandlist macrodefs_lst tmpvar varmap cmd tl pattern
                       | Grep pattern_arglist       -> cmd_grep commandlist macrodefs_lst tmpvar varmap cmd tl pattern_arglist
                       | Grep_v pattern_arglist     -> cmd_grep_v commandlist macrodefs_lst tmpvar varmap cmd tl pattern_arglist (* grep -v *)
                       | Select index               -> cmd_select commandlist macrodefs_lst tmpvar varmap cmd tl index
                       | MSelect index_list         -> cmd_mselect commandlist macrodefs_lst tmpvar varmap cmd tl index_list
                       | ColSelect   col_index      -> cmd_colselect commandlist macrodefs_lst tmpvar varmap cmd tl col_index
                       | RowSelect   index          -> cmd_rowselect commandlist macrodefs_lst tmpvar varmap cmd tl index
                       | DropCol   col_index        -> cmd_dropcol commandlist macrodefs_lst tmpvar varmap cmd tl col_index
                       | DropRow   index            -> cmd_droprow commandlist macrodefs_lst tmpvar varmap cmd tl index
                       | Select_match ( col_idx, matchpat) -> cmd_select_match commandlist macrodefs_lst tmpvar varmap cmd tl ( col_idx, matchpat)
(*
- iselectmatch with 3 parameters would be good:
   * selection-source
   * selection-match-string
   * default-selection
*)
                       | I_Select_match ( col_idx, matchpat, default_pattern )
                                                           -> cmd_i_select_match commandlist macrodefs_lst tmpvar varmap cmd tl ( col_idx, matchpat, default_pattern )

                       | To_matchres                -> cmd_to_matchres commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Table_to_matchres          -> cmd_table_to_matchres commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Append_to  varname         -> cmd_append_to commandlist macrodefs_lst tmpvar varmap cmd tl varname
                       (*
                       | Transpose                  -> cmd_transpose commandlist macrodefs_lst tmpvar varmap cmd tl
                       *)
                       | Link_extract               -> cmd_link_extract commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Link_extract_xml           -> cmd_link_extract_xml commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Rebase                     -> cmd_rebase commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Title_extract              -> cmd_title_extract commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Tag_select (selector, extractor )  -> cmd_tag_select commandlist macrodefs_lst tmpvar varmap cmd tl (selector, extractor )
                       | Paste paste_list            -> let res = paste_arglist_to_string  paste_list  varmap in
                                                        command tl macrodefs_lst (String res) varmap
                       | Print_args prt_args         ->
                                                        ignore ( command [ Paste( prt_args ); Print ] macrodefs_lst Empty varmap ); (* use the Paste-command and the print-command *)
                                                        command tl macrodefs_lst tmpvar varmap (* just next command without changed tmpvar *)

                       | Print                      -> cmd_print commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Show_match                 -> cmd_show_match commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Print_string str           -> print_string str;  command tl macrodefs_lst tmpvar varmap
                       (*
                       | CSV_save_as argument_list  -> cmd_csv_save_as commandlist macrodefs_lst tmpvar varmap cmd tl argument_list
                       | CSV_save                   -> cmd_csv_save commandlist macrodefs_lst tmpvar varmap cmd tl
                       | CSV_read  filename_arglist -> cmd_csv_read commandlist macrodefs_lst tmpvar varmap cmd tl filename_arglist
                       *)
                       | Save_as      argument_list -> cmd_save_as commandlist macrodefs_lst tmpvar varmap cmd tl argument_list
                       | Save                       -> cmd_save commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Setvar var                 -> command tl macrodefs_lst var varmap (* sets the argument of setvar as new tmpvar *)
                       | Store  varname             -> verbose_printf "Store tmpvar in varname \"%s\"\n" varname;
                                                       command tl macrodefs_lst tmpvar (Varmap.add varname tmpvar varmap)  (* stores tmpvar as named variable *)

                       | Recall varname             -> verbose_printf "Recall variable: \"%s\"\n" varname;
                                                       let varcontents = Varmap.find varname varmap in
                                                       command tl macrodefs_lst varcontents varmap

                       | Delete varname             -> verbose_printf "Delete variable \"%s\"\n" varname;
                                                       command tl macrodefs_lst tmpvar (Varmap.remove varname varmap)  (* removes variable varname *)

                       | Storematch  varname        -> cmd_storematch commandlist macrodefs_lst tmpvar varmap cmd tl varname
                       | Sort                       -> cmd_sort commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Uniq                       -> cmd_uniq commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Show_variables             -> cmd_show_variables commandlist macrodefs_lst tmpvar varmap cmd tl
                       | List_variables             -> cmd_list_variables commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Show_type                  -> cmd_show_type commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Basename                   -> cmd_basename commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Subst (from_re, to_str)    -> cmd_subst commandlist macrodefs_lst tmpvar varmap cmd tl (from_re, to_str)
                       | Quote                      -> cmd_quote commandlist macrodefs_lst tmpvar varmap cmd tl
                       | To_string                  -> command tl macrodefs_lst (String (to_string tmpvar varmap)) varmap
                       | Dump                       -> cmd_dump commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Show_tags                  -> cmd_show_tags commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Show_tags_fullpath         -> cmd_show_tags_fullpath commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Dump_data                  -> cmd_dump_data commandlist macrodefs_lst tmpvar varmap cmd tl
                       | System                     -> cmd_system commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Exit_parse                 -> flush stdout; raise Parse_exit
                       | Html_decode                -> cmd_html_decode commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Url_decode                -> let newvar = default_application tmpvar Netencoding.Url.decode varmap in command tl macrodefs_lst newvar varmap
                       | Readline  arg_opt          -> cmd_readline commandlist macrodefs_lst tmpvar varmap cmd tl arg_opt
                       | Sleep_ms  milliseconds     -> Sleep.sleep_ms milliseconds;   command tl macrodefs_lst tmpvar varmap
                       | Json_prettify              -> cmd_json_prettify commandlist macrodefs_lst tmpvar varmap cmd tl
                       | Call_macro     macro_name  -> cmd_call_macro commandlist macrodefs_lst tmpvar varmap cmd tl macro_name
                       | Dummy                      -> command tl macrodefs_lst tmpvar varmap (* does nothing; just a Dummy (NOP) *)
                       | Empty_dummy                -> command tl macrodefs_lst Empty  varmap (* gives back Empty as result/tmpvar *)

                   end

(* COMMANDS-functions are followinf below *)

(*
macrodefs_lst  wird in command benutzt. => Parameterübergabe?
*)
(* "command"-function is the main commands-parser/evaluator *)
(* ======================================================== *)
and     cmd_post commandlist macrodefs_lst tmpvar varmap cmd tl fname_arglist  :  results_t * varmap_t =

          let create_keyval_pair        key         = (key, to_string (Varmap.find key varmap) varmap ) in
          let create_post_argumentlist  varnamelist = List.map create_keyval_pair varnamelist in

          let post_arglist = create_post_argumentlist (List.map (fun x -> to_string x varmap) fname_arglist)  in (* the argument-list for POST-call *)

          begin
            match tmpvar with
              | Url (url,referrer) ->
                             begin
                             match post_document  url referrer post_arglist  varmap with
                               | Some ( doc, url, new_varmap ) -> command tl macrodefs_lst (Document (doc, url)) new_varmap (* $URL *)
                               | None                          -> raise No_document_found
                             end

              | _ -> raise Wrong_tmpvar_type
          end



and     cmd_download commandlist macrodefs_lst tmpvar varmap cmd tl fname_arglist_opt  :  results_t * varmap_t =
          begin
            match tmpvar with
              | Url (u,r)          -> let filename =
                                        match fname_arglist_opt with
                                          | None -> Parsers.url_to_filename u (* auto-filename, from URL    *)
                                          | Some arg_list -> paste_arglist_to_string arg_list varmap  (* filename given as argument *)
                                      in

                                      let new_varmap_opt = try download  u r filename varmap
                                                                with Network.Pipelined.Get_error status -> Printf.eprintf "Download failed because of Get_error ( URL: %s )\n" "" ; Some varmap
                                                            in
                                      begin
                                        match new_varmap_opt with
                                          | None       -> command tl macrodefs_lst tmpvar varmap
                                          | Some newvm -> command tl macrodefs_lst tmpvar newvm
                                      end
              | Url_list ul        ->
                                      (* Download on Url_list necessarily use auto-filenaming *)
                                      Unit ( List.iter ( fun (url,ref) -> ignore ( command [ Download None ] macrodefs_lst (Url (url, ref)) varmap );
                                                                          if Cli.opt.Cli.ms_sleep > 0 then Sleep.sleep_ms Cli.opt.Cli.ms_sleep ) ul ) , varmap
              | Url_array ua        ->
                                      (* Download on Url_array necessarily use auto-filenaming *)
                                      Unit ( Array.iter ( fun (url,ref) -> ignore ( command [ Download None ] macrodefs_lst (Url (url, ref)) varmap );
                                                                           if Cli.opt.Cli.ms_sleep > 0 then Sleep.sleep_ms Cli.opt.Cli.ms_sleep ) ua ) , varmap

              | _ -> raise Wrong_tmpvar_type
            end



and     cmd_get_url commandlist macrodefs_lst tmpvar varmap cmd tl (url, referrer)  :  results_t * varmap_t =
          if Neturl.extract_url_scheme url = "file"

           then     (* FILE://... *)
             begin

               (* extract the filename from the url *)
               (* --------------------------------- *)
               let url_strlen = String.length url in
               let scheme_strlen = String.length "file:///" in
               let scheme_short_strlen = String.length "file://" in

               let filename = if String.sub url 0 scheme_strlen = "file:///"
                              then String.sub url scheme_short_strlen (url_strlen - scheme_short_strlen )
                              else (prerr_endline "Get_url - file-urlscheme problem! "; flush stderr; raise No_document_found) (* other exception better? *)
               in

               (* read file and go on with Document from file as tmpvar *)
               (* ----------------------------------------------------- *)
               let contents   = Tools.read_file filename in
               command tl macrodefs_lst (Document (contents, "-")) varmap

             end

           else     (* HTTP://... *)
             begin
             match get_document  url referrer varmap with
               | Some ( doc, url, new_varmap ) -> command tl macrodefs_lst (Document (doc, url)) new_varmap (* $URL *)
               | None                          -> raise No_document_found
             end



and     cmd_get commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          (* This is not directly downloading the data; just inserting    *)
          (* the appropriate downloader-Tokens into the Tokenlist / "AST" *)
          (* ------------------------------------------------------------ *)
          begin
            match tmpvar with
              | Url (u,r)          -> command (Get_url (u,r) :: tl) macrodefs_lst tmpvar varmap
              | Url_list  urllist  -> if List.length urllist > 1
                                      then command (Get_urls :: tl) macrodefs_lst tmpvar varmap
                                      else
                                        begin
                                          let (u,r) = List.hd urllist in
                                          command (Get_url (u,r) :: tl) macrodefs_lst tmpvar varmap
                                        end

              | Url_array urlarray -> if Array.length urlarray > 1
                                      then command (Get_urls :: tl) macrodefs_lst tmpvar varmap
                                      else
                                        begin
                                          let (u,r) = urlarray.(0) in
                                          command (Get_url (u,r) :: tl) macrodefs_lst tmpvar varmap
                                        end
              (* MATCHRES ???
              *)
              | _ -> raise Wrong_tmpvar_type
            end



and     cmd_get_urls commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          (*
          (* two functions for creating the action-list from the url-list *)
          (* ------------------------------------------------------------ *)
          let get_save_url (url,ref) = [ Get_url (url, ref); Save; Sleep_ms Cli.opt.Cli.ms_sleep ] in
          let create_actionlist  url_liste = List.flatten ( List.map get_save_url (List.rev url_liste) ) in
          *)


          (* Remark: The last Sleep added by this process is not necessary... (one sleep too much) *)

          (* ----------------------------------------------------------------------- *)
          (* here the download-actions will be done, either directly or via creating *)
          (* a tokenlist that will do Get-Sleep-Save action-triplets, and prepends   *)
          (* these actions before the tail of the former command-list.               *)
          (* ----------------------------------------------------------------------- *)
          begin
            match tmpvar with
              | Url_list  urllist  -> prerr_endline "Should now get Documents!";
                                      let docs, vm = get_document_list  urllist varmap in
                                      command tl macrodefs_lst (Document_array (Array.of_list docs)) vm

              | Url_array urlarray -> prerr_endline "Should now get Documents!";
                                      let docs, vm = get_document_list  (Array.to_list urlarray) varmap in
                                      command tl macrodefs_lst (Document_array (Array.of_list docs)) vm

              | _                -> raise Wrong_tmpvar_type
          end



and     cmd_match commandlist macrodefs_lst tmpvar varmap cmd tl pattern  :  results_t * varmap_t =
          verbose_printf "MATCH-PATTERN: \"%s\"\n" pattern; (* devel-debug-info *)

          let str =
            begin
              match tmpvar with
                | Document (doc, url) -> doc
                | String    s         -> s
                | String_array strarr -> Array.fold_left ( fun collect str       -> collect ^ str) "" strarr
                | Document_array arr  -> Array.fold_left ( fun collect (doc,url) -> collect ^ doc) "" arr
                (* match also on other types?? Does matching an URL for example makes sense? *)
                | _            -> raise No_Matchable_value_available (* this is a type-error Wrong_tmpvar_type *)
            end
          in
          let match_res = Parsers.if_match_give_group_of_groups str ~regexp_str:pattern (* flags here *) in
          let matched =
            begin
              match match_res with
                | None   -> raise No_Match
                | Some res -> res
            end
          in
          command tl macrodefs_lst (Match_result matched) varmap



and     cmd_grep commandlist macrodefs_lst tmpvar varmap cmd tl pattern_arglist  :  results_t * varmap_t =
          let pattern = paste_arglist_to_string  pattern_arglist  varmap in (* create pattern-string from argument-list *)
          (*
           if Pcre.pmatch ~pat:".any-dl.rc: No such file or directory" msg
          *)

          (* for each line of a string apply a grep-on-pattern and return list of matching lines *)
          (* ----------------------------------------------------------------------------------- *)
          let grep_lines_from_string  str =              (* partially application to the pattern *)
           let lines = Tools.lines_of_string str in
           List.filter ( fun line -> test_pattern_match_on_string pattern line ) lines
          in

          let test_match_on_string = test_pattern_match_on_string  pattern in (* partially application to the pattern *)


          (* do the grep *)
          (* ----------- *)
          let grepped =
            begin
              match tmpvar with
                | Document (doc, url) -> let grepped_lines = grep_lines_from_string doc in
                                         String_array (Array.of_list grepped_lines )

                | Document_array docarr ->
                                           let res =
                                             Array.fold_left ( fun sofar (d,r) -> Array.append sofar (Array.of_list (grep_lines_from_string d)) ) [||] docarr in
                                           String_array res

                | String_array str_arr -> String_array( Array2.filter test_match_on_string str_arr)

                | Url_array    url_arr -> Url_array (Array2.filter ( fun (url,ref) -> test_match_on_string url ||
                                                                                      test_match_on_string ref ) url_arr )

                | Url_list     url_arr -> Url_list (List.filter ( fun (url,ref) -> test_match_on_string url ||
                                                                                     test_match_on_string ref ) url_arr )

                | Match_result mres -> Match_result ( Array2.filter_row_by_colmatch test_match_on_string mres )

                | _            -> prerr_endline "Grep: nothing to match"; raise No_Matchresult_available
            end
          in
           command tl macrodefs_lst grepped varmap



and     cmd_grep_v commandlist macrodefs_lst tmpvar varmap cmd tl pattern_arglist  :  results_t * varmap_t =
          let pattern = paste_arglist_to_string  pattern_arglist  varmap in (* create pattern-string from argument-list *)

          let test_match_on_string = test_pattern_match_on_string  pattern in (* partially application to the pattern *)
          let test_nonmatch_on_string str = not (test_match_on_string str)  in (* negation of test_match_on_string    *)

          (*
           if Pcre.pmatch ~pat:".any-dl.rc: No such file or directory" msg
          *)
          let grepped =
            begin
              match tmpvar with
                | Document (doc, url) -> let lines = Tools.lines_of_string doc in
                                         let selected_lines = ( List.filter test_nonmatch_on_string lines ) in
                                         String_array (Array.of_list selected_lines )

                | String_array str_arr -> String_array( Array2.filter test_nonmatch_on_string str_arr)
                | Url_array    url_arr -> Url_array (Array2.filter ( fun (url,ref) -> test_nonmatch_on_string url &&
                                                                                           test_nonmatch_on_string ref ) url_arr )

                | Url_list     url_arr -> Url_list (List.filter ( fun (url,ref) -> test_nonmatch_on_string url &&
                                                                                           test_nonmatch_on_string ref ) url_arr )

                | Match_result mres -> Match_result ( Array2.filter_row_by_colmatch test_nonmatch_on_string mres )
                | _            -> prerr_endline "Grep_v: nothing to match"; raise No_Matchresult_available
            end
          in
           command tl macrodefs_lst grepped varmap



and     cmd_select commandlist macrodefs_lst tmpvar varmap cmd tl index  :  results_t * varmap_t =
          begin
            match tmpvar with
              | Document (doc, url)   -> let res = if index = 1 then Url (url, "-") else String doc in
                                         command tl macrodefs_lst res varmap
              | Document_array docarr -> command tl macrodefs_lst ( Document (fst docarr.(index), snd docarr.(index)) ) varmap
              | String_array rowitems -> command tl macrodefs_lst (String(rowitems.(index))) varmap
              | Url_array    rowitems -> command tl macrodefs_lst (Url( fst(rowitems.(index)), snd(rowitems.(index)))) varmap
              | Url_list     rowitems -> command tl macrodefs_lst (Url( fst(List.nth rowitems index), snd(List.nth rowitems index))) varmap
              | _            -> prerr_endline "Select: nothing to match"; raise No_Matchresult_available
          end



and     cmd_mselect commandlist macrodefs_lst tmpvar varmap cmd tl index_list  :  results_t * varmap_t =
          begin
            match tmpvar with
              | String_array rowitems -> command tl macrodefs_lst (String_array(item_selection rowitems index_list)) varmap
              | Url_array    rowitems -> command tl macrodefs_lst (Url_array(item_selection rowitems index_list)) varmap
              | _            -> prerr_endline "MSelect: nothing to match"; raise No_Matchresult_available
          end



and     cmd_colselect commandlist macrodefs_lst tmpvar varmap cmd tl col_index  :  results_t * varmap_t =
          begin
            match tmpvar with
              | Match_result mres ->
                                     let outer_maxidx = Array.length mres     - 1 in (* outer: row *)
                                     let inner_maxidx = Array.length mres.(0) - 1 in (* inner: col *)
                                     let res          = Array.make (Array.length mres) mres.(0).(0) in
                                     begin
                                       if col_index >= 0 && col_index <= inner_maxidx
                                       then
                                         begin
                                           for idx = 0 to outer_maxidx
                                           do
                                             res.(idx) <- mres.(idx).(col_index)
                                           done;
                                           command tl macrodefs_lst (String_array res) varmap
                                         end
                                       else
                                         raise Invalid_Col_Index
                                     end
              | _ -> print_warning "ColSelect: wrong type!!!"; raise Wrong_tmpvar_type
          end



and     cmd_rowselect commandlist macrodefs_lst tmpvar varmap cmd tl index  :  results_t * varmap_t =
          let res = ref Empty in
          begin
            match tmpvar with
              | Match_result mres ->
                                     begin
                                       if index >= 0 && index <= Array.length ( mres ) - 1
                                       then
                                         res := String_array ( mres.(index) )
                                       else
                                         raise Invalid_Row_Index
                                     end
              | _ -> print_warning "RowSelect: wrong type!!!"; raise Wrong_tmpvar_type
          end;
          command tl macrodefs_lst !res varmap



                       (* Drops a column from a matchres *)
                       (* ------------------------------ *)
and     cmd_dropcol commandlist macrodefs_lst tmpvar varmap cmd tl col_index  :  results_t * varmap_t =
          begin
            match tmpvar with
              | Match_result mres ->
                         let dropres = Array.copy mres in
                         Array.iteri ( fun idx the_row ->
                                                dropres.(idx) <- array_drop the_row col_index (* !!! *)
                                     ) mres;
                         command tl macrodefs_lst (Match_result dropres) varmap
              | _ -> raise Wrong_argument_type (* wrong tmpvar type *)
          end


                       (* Drops a row from a matchres *)
                       (* --------------------------- *)
and     cmd_droprow commandlist macrodefs_lst tmpvar varmap cmd tl index  :  results_t * varmap_t =
          let res =
            begin
              match tmpvar with
                | Match_result mres -> Match_result (array_drop mres index)
                | _                 -> print_warning "DropRow: wrong type!!!"; raise Wrong_tmpvar_type
            end
          in
           command tl macrodefs_lst res varmap



                       (* select match is a row-select, where the index *)
and     cmd_select_match commandlist macrodefs_lst tmpvar varmap cmd tl ( col_idx, matchpat)  :  results_t * varmap_t =
          (* first match wins *)
           begin
             match tmpvar with
               | Match_result mres ->

                      (*let max_row_idx = Array.length ( mres ) - 1 in*)
                      (*let max_col_idx = Array.length ( mres.(0) ) - 1 in*)

                      let rows     = Array.to_list mres in

                      (* here is the selection: via string match of the lookup-pattern *)
                      let selected = List.filter ( fun item -> test_pattern_match_on_string matchpat item.(col_idx)  ) rows in
                      if List.length selected = 0 then raise No_Match;

                      verbose_printf "found: %d items \n" (List.length selected);
                      command tl macrodefs_lst (String_array (List.hd selected)) varmap (* first match wins *)

               | _ -> print_warning "RowSelect: wrong type!!!"; raise Wrong_tmpvar_type
           end



(*
- iselectmatch with 3 parameters would be good:
   * selection-source
   * selection-match-string
   * default-selection
*)
and     cmd_i_select_match commandlist macrodefs_lst tmpvar varmap cmd tl ( col_idx, matchpat, default_pattern )  :  results_t * varmap_t =
          (* select match is a row-select, where the index *)
            (* first match wins *)
             begin
               match tmpvar with
                 | Match_result mres ->
                        let max_row_idx = Array.length ( mres ) - 1 in
                        let max_col_idx = Array.length ( mres.(0) ) - 1 in

                        let rows     = Array.to_list mres in

                        (* column selection from the match-result *)
                        (* -------------------------------------- *)
                        let col = Array.make (Array.length mres) mres.(0).(0) in
                        begin
                          if col_idx >= 0 && col_idx <= max_col_idx
                          then
                            begin
                              for idx = 0 to max_row_idx
                              do
                                col.(idx) <- mres.(idx).(col_idx)
                              done
                            end
                          else
                            raise Invalid_Col_Index
                        end;

                        (* select the match-pattern: either interactively, *)
                        (* or use the default from the parser-definition.  *)
                        (* ----------------------------------------------- *)
                        let match_pattern =
                          if
                            Cli.opt.Cli.interactive = true
                          then
                            interactive_string_select col default_pattern
                          else
                            default_pattern
                        in
                          verbose_printf "selected pattern: \"%s\"\n" match_pattern;

                          let selected = List.filter ( fun item -> test_pattern_match_on_string match_pattern item.(col_idx)  ) rows in
                          if List.length selected = 0 then raise No_Match;

                          verbose_printf "found: %d items \n" (List.length selected);

                          command tl macrodefs_lst (String_array (List.hd selected)) varmap (* first match wins *)

                 | _ -> print_warning "RowSelect: wrong type!!!"; raise Wrong_tmpvar_type
             end



                       (* ------------------------------------------------------------------------------ *)
                       (* to matchres does conbverts to a matchres-like result (matrix (array of array)) *)
                       (* Urls will be converted in a way, that the link and the referrer will pop up as *)
                       (* seperate coulmns in each rows. With dropcol() they can be kiekced out!         *)
                       (* ------------------------------------------------------------------------------ *)
and     cmd_to_matchres commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          let pair_to_arr pair = [| fst pair; snd pair |] in

          let new_var =
            begin
              match tmpvar with
                 (*
                 | Varname         vn         -> Match_result [| [| vn |] |]
                 *)

                 | String          s          -> Match_result [| [| s |] |]
                 | String_array    str_arr    -> Match_result [| str_arr |]
                 | Document        (doc, url) -> Match_result [| [| doc; url |] |]
                 | Document_array  doc_arr    -> Match_result (Array.map pair_to_arr doc_arr)
                 | Url             (url, ref) -> Match_result [| [| url; ref  |] |]

                 | Url_list        url_list   -> let url_array = Array.of_list url_list in
                                                 Match_result (Array.map pair_to_arr url_array)

                 | Url_array       url_array  -> Match_result (Array.map pair_to_arr url_array)
                 | Dummy_result               -> Match_result [| [| "DUMMY_A"; "DUMMY_B" |]; [| "DUMMY_C"; "DUMMY_D"|] |]
                 | Match_result    match_res  -> Match_result match_res  (* stays the same *)
                 | Empty                      -> Match_result [| [| "" |] |]
                 | _ -> raise Wrong_argument_type
            end;
            in
            command tl macrodefs_lst new_var varmap



and     cmd_table_to_matchres commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          let matchres =
            match tmpvar with
              | String   str -> let dl = Parsers.conv_to_doclist str in
                                let u = Parsers.table_unparse dl in
                                Match_result u
              | _ -> raise Wrong_argument_type
          in
          command tl macrodefs_lst matchres varmap



and     cmd_append_to commandlist macrodefs_lst tmpvar varmap cmd tl varname  :  results_t * varmap_t =
          (* append tmpvar to a Matchres, adressed by varname *)
          (* if that variable is unknown, create it anew.     *)

          let extract_matchres value = match value with Match_result mr -> mr | _ -> raise Wrong_argument_type in

          let tmp_arr = extract_matchres tmpvar in

          (* if known, extract array; if unknown, create empty array *)
          let known_arr = if Varmap.mem varname varmap
                          then extract_matchres (Varmap.find varname varmap)
                          else [| |]
          in

          let res = Match_result ( Array.append known_arr tmp_arr ) in (* append the tmpvar-array to the known array *)

          let new_varmap = Varmap.add varname res varmap in

          command tl macrodefs_lst tmpvar new_varmap (* tmpvar is not touched *)



(*
and     cmd_transpose commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          let result =
            begin
              match tmpvar with
                 | Match_result    match_res  -> Match_result ( Tools.transpose match_res )
                 | _ -> raise Wrong_argument_type
            end;
            in
            command tl macrodefs_lst result varmap
*)



and     cmd_link_extract commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          begin

            (* extracted urls can be rebased with this function *)
            let rebase_urls   url_list  parent_url =
                List.fold_right ( fun lnk sofar -> match Parsers.Rebase.rebase_url ~verbose:Cli.opt.Cli.very_verbose parent_url lnk with
                                                     | Some rebased -> (rebased,  parent_url) :: sofar
                                                     | None         -> sofar
                                ) url_list []
            in

            (* extract urls and rebase these extracted urls *)
            let extract_and_rebase document url =
                let extracted_urls = Parsers.linkextract_str document in
                List.iter (fun x -> very_verbose_printf "---extracted url: %s\n" x) extracted_urls;
                rebase_urls extracted_urls url
            in


            verbose_printf "%s" "Link_extract\n";

            match tmpvar with
              | Document (doc, url) ->
                        let rebased_urls = extract_and_rebase doc url               in
                        let links        = Url_array ( Array.of_list rebased_urls ) in
                       command tl macrodefs_lst links varmap

              | Document_array doc_url_array ->
                   let rebased = List.map ( fun (doc,url) -> extract_and_rebase doc url ) (Array.to_list doc_url_array) in
                   let rebased = List.flatten rebased in
                     let links        = Url_array ( Array.of_list rebased ) in
                     command tl macrodefs_lst links varmap


              | _ -> print_warning "Link_extract found non-usable type"; raise Wrong_tmpvar_type
          end



and     cmd_link_extract_xml commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          begin
            match tmpvar with
              | Document(doc, url)-> let urls   = Array.of_list (Parsers.xml_get_href_from_string doc) in
                                     (* the url of the doecument will become the referrer of the extracted url! *)
                                     let links  = Url_array (Array.map ( fun lnk -> (lnk, url) ) urls) in
                                     command tl macrodefs_lst links varmap
              | _ -> print_warning "Link_extract_xml found non-usable type"; raise Wrong_tmpvar_type
          end



and     cmd_rebase commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          let starturl = to_string (Varmap.find "STARTURL" varmap) varmap  in
          let rebase   = Parsers.Rebase.try_rebase ~verbose:Cli.opt.Cli.very_verbose  starturl in

          let result =
            begin
              match tmpvar with
                 | Url             (u,r)      -> Url ( rebase u, r )
                 | Url_list        urllist    -> Url_list ( List.map ( fun (u,r) -> (rebase u, r)) urllist )
                 | String          s          -> String ( rebase s )
                 | String_array    str_arr    -> String_array ( Array.map rebase str_arr )
                 | Match_result    match_res  -> Match_result ( Array.map ( fun x -> Array.map rebase x ) match_res )

                 | _ -> print_warning "Rebase found non-usable type"; raise Wrong_tmpvar_type
            end
          in
          command tl macrodefs_lst result varmap



and     cmd_title_extract commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          begin
            match tmpvar with
              | Document (doc, url) ->
                        let result = Array.of_list (Parsers.titleextract_str doc) in
                        command (Subst ("\n", "") :: tl) macrodefs_lst (String_array result) varmap

              | Document_array docarr ->  let doc_url_list = (Array.to_list docarr)    in
                                          let titlili      = List.map ( fun (d,u) -> Parsers.titleextract_str d  ) doc_url_list in
                                          let titli        = List.flatten titlili in
                                          let result = Array.of_list titli in
                                          command (Subst ("\n", "") :: tl) macrodefs_lst (String_array result) varmap

              | _ -> print_warning "Title_extract found non-usable type"; raise Wrong_tmpvar_type
          end



and     cmd_tag_select commandlist macrodefs_lst tmpvar varmap cmd tl (selector, extractor )  :  results_t * varmap_t =
          (* --------------------------------------------------------------------- *)
          (* apply "find_elements_by_tag_name" to the doclist with hd as selector  *)
          (* and the resulting doclist is used as input to the next call of        *)
          (* "find_elements_by_tag_name", with the next element from the list then *)
          (* --------------------------------------------------------------------- *)
          let selectloop sel_lst doclist =
            let rec aux sel dl = match sel with
              | hd::tl ->
                          let selector =
                          begin
                                match hd.tag_sel, hd.argkey_sel, hd.argval_sel with

                                  | None,     None,     Some aval -> Parsers.Htmlparse.find_elements_by_argval                aval  (* OK *)
                                  | None,     Some key, None      -> Parsers.Htmlparse.find_elements_by_argkey           key        (* OK *)
                                  | None,     Some key, Some aval -> Parsers.Htmlparse.find_elements_by_argpair          key  aval  (* OK *)
                                  | Some tag, None,     None      -> Parsers.Htmlparse.find_elements_by_tag_name    tag             (* OK *)
                                  | Some tag, None,     Some aval -> Parsers.Htmlparse.find_elements_by_tag_argval  tag       aval  (* OK *)
                                  | Some tag, Some key, None      -> Parsers.Htmlparse.find_elements_by_tag_argkey  tag  key        (* OK *)
                                  | Some tag, Some key, Some aval -> Parsers.Htmlparse.find_elements_by_tag_argpair tag  key  aval  (* OK *)
                                  | None,     None,     None      -> assert false  (* this case makes no sense, and should not occur *)
                          end
                          in
                                 aux tl (selector dl)

              | []     -> dl
            in
              aux sel_lst doclist
          in


          (* ---------------------- *)
          (* tagselection-functions *)
          (* ====================== *)

          (* -------------------------------------- *)
          (* function for tagselection from doclist *)
          (* -------------------------------------- *)
          let select_tags_from_doclist selector doclist =
              match selector with
                | Selector_any                     -> Parsers.Htmlparse.find_any_elements doclist
                | Specific_selector selector_liste -> selectloop selector_liste doclist (* the selected tags *)
          in
          (* ---------------------------------------------- *)
          (* function for tagselection from document-string *)
          (* ---------------------------------------------- *)
          let select_tags_from_document selector doc = select_tags_from_doclist selector (Parsers.conv_to_doclist doc)
          in


          (* select tags from the document in TMPVAR *)
          (* --------------------------------------- *)
          let selected_tags =
            begin
              match tmpvar with
                (* maybe it does make sense for using taglist-command on already extracted doclist?!
                | Doclist   doclist   -> selectloop selector_liste doclist (* the selected tags *)
                *)

                | Document (doc, url) -> select_tags_from_document selector doc
                | Document_array doc_arr ->
                                            (* extract all document-strings *)
                                            let docs = Array.fold_right ( fun docref aggregation -> (fst docref) :: aggregation ) doc_arr [] in

                                            (* create the doclists *)
                                            let doclists = List.map Parsers.conv_to_doclist docs in

                                            (* selector-function: for given selector: fix the selector and extract tags *)
                                            (* ------------------------------------------------------------------------ *)
                                            let select_tags_for_given_selector doclist = select_tags_from_doclist selector doclist
                                            in

                                            (* for given selector extract the tags *)
                                            let extractions = List.map ( fun dl -> select_tags_for_given_selector dl ) doclists in
                                            List.flatten extractions

                | _ -> print_warning "Tag_select found non-usable type"; raise Wrong_tmpvar_type
            end
          in

          (* verbose-optional message to the user: how many matching tags were found *)
          (* ----------------------------------------------------------------------- *)
          verbose_printf "*** Tag_select:  Length of selected_tags-list: %d\n" (List.length selected_tags);

          (* if tagselect() gives back empty list, this means: no document found.                                      *)
          (* if not raising Tagselect_empty_list, this would be a fatal error, because of the following function-calls *)
          (* --------------------------------------------------------------------------------------------------------- *)
          if (List.length selected_tags) = 0
          then
            begin
              prerr_endline "tagselect: nothing found";
              (*
              raise Tagselect_empty_list
              *)
            end;


          (* ------------------------------------------------------------------- *)
          (* basic extractor-functions that extract the DOM-stuff from a doclist *)
          (* ------------------------------------------------------------------- *)
          let extr_data dl      = Parsers.Htmlparse.collect_data_per_doc dl    in
          let extr_dataslurp dl = [ Parsers.Htmlparse.collect_data dl ] in  (* gives back list, for making retval a list, like the other functions *)

          let extr_arg   key dl = let pairs = List.map Parsers.Htmlparse.extract_arg_pairs_from_doc dl in
                                  List.fold_left ( fun sofar pairlst -> try (List.assoc key pairlst) :: sofar with Not_found -> sofar ) [] pairs in

          let extr_tag       dl = Parsers.Htmlparse.extract_tagname_from_topdocs_of_doclist dl in
          let extr_argkeys   dl = Parsers.Htmlparse.extract_arg_keys_from_topdocs_of_doclist dl in
          let extr_argvals   dl = Parsers.Htmlparse.extract_arg_values_from_topdocs_of_doclist dl in
          let extr_argpairs  dl = Parsers.Htmlparse.extract_arg_pairs_from_topdocs_of_doclist dl in

          let extr_dump      dl = Parsers.Htmlparse.dump_html dl; (* dump!!! *)
                                  [ Parsers.convert_doclist_to_htmlstring dl ] in  (* gives back list, for making retval a list, like the other functions *)
          let extr_html_str  dl = Parsers.convert_doclist_to_htmlstring dl in


          (* function, that extracts single-items from the doclist *)
          (* ----------------------------------------------------- *)
          let collect_singles extractor_liste elements =
             List.fold_left ( fun sofar extr ->
                                                 begin
                                                  match extr with
                                                    | `Data        -> let dat        = extr_data      elements in (Array.of_list dat)
                                                    | `Data_slurp  -> let dat        = extr_dataslurp elements in Array.of_list dat
                                                    | `Tag         -> let tagnames   = extr_tag       elements in (Array.of_list tagnames)
                                                    | `Arg key     -> let extracted  = extr_arg key   elements in Array.of_list extracted
                                                    | `Dump        -> let dumped     = extr_dump      elements in Array.of_list dumped
                                                    | `Html_string -> [| (extr_html_str elements) |]
                                                    (*| `Doclist     -> Doclist elements *)
                                                    | _            -> raise Extractor_list_failure
                                                 end :: sofar
             ) [] (List.rev extractor_liste)
          in

          (* function, that extracts paired data from the doclist *)
          (* ---------------------------------------------------- *)
          let extract_pairs item elements =
            begin
             match item with
               | `Arg_pairs   -> let pairs      = extr_argpairs  elements in ( Array.of_list pairs )
               | `Arg_keys    -> let arg_keys   = extr_argkeys   elements in ( Array.of_list arg_keys )
               | `Arg_vals    -> let arg_values = extr_argvals   elements in ( Array.of_list arg_values )
               | _            -> raise Extractor_list_failure
            end
          in


          let result =
            begin
              match extractor with
                | Pair_extr   extr     -> Match_result ( extract_pairs extr selected_tags )
                | Single_extr extr_lst -> Match_result ( Array.of_list ( collect_singles extr_lst selected_tags ) )
            end
          in

          (*
            `Doclist
            is fundamentally different to the other return-values.
            It does not make sense to give it back, and it also is not of type string!!!!!
            So it also does not make sense to collect it tigether with the other items!
          *)

          command tl macrodefs_lst result varmap



and     cmd_print commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          let print_url url =  Printf.printf "%s  # Referrer:  %s\n" (fst url) (snd url) in

          begin
            match tmpvar with
              (* does Varname makes sense at all here? *)
              | Varname  varname  -> Printf.printf "\n\tVarname  varname => varname = \"%s\"\n" varname;
                                     Unit ( ignore (command [Print] macrodefs_lst (Varmap.find varname varmap) varmap) ) (* CHECK FUNCTIONALITY, PLEASE *)

              | String   str      -> Unit ( print_endline str )
              | Document(doc, url)-> Unit( print_endline doc )  (* only print the document, without referrer *)

              | Document_array doc_arr -> (* only print the documents, without referrer *)
                                          Unit(
                                          Array.iter (fun (doc,ref) -> print_endline doc;
                                                                       print_endline "----------------------------------" ) doc_arr
                                          )

              | Match_result mres -> Unit( Array.iter ( fun x -> Array.iter ( fun y -> Printf.printf "\"%s\" ||| " y) x;
                                                           print_newline() ) mres )
              | String_array     str_arr -> Unit ( Array.iter ( fun str -> Printf.printf "\"%s\" \n" str) str_arr )
              | Url (href, ref)    -> Unit ( print_url (href,ref) )
              | Url_list  liste    -> Unit ( List.iter  print_url liste )
              | Url_array arr      -> Unit ( Array.iter print_url arr )
              | Cookies   cooklist -> Unit ( List.iter Network.Cookies.print_cookie cooklist )

              (*
              | Doclist   doclist  -> let string_of_dl dl = Parsers.convert_doclist_to_htmlstring [dl] in
                                      List.iter ( fun doc -> print_endline ( string_of_dl doc ) ) doclist (* one per line *)
              *)

              | _ -> Unit ( print_warning "Print-command found non-printable type" )
          end;
          command tl macrodefs_lst tmpvar varmap



and     cmd_show_match commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          (* prints "real" matches only (and not the fullmatch with index = 0) *)
          begin
            match tmpvar with
              | Match_result mres ->
                         verbose_fprintf stdout "for real matches: show_match: Col 0 is the whole match, all others are the groups\n";
                         Array.iteri ( fun idx x ->
                                                Printf.printf "Row %2d:\n" idx;
                                                Printf.printf "-------\n";
                                                for index = 0 to Array.length x -1
                                                do
                                                  Printf.printf "  Col %2d: \"%s\" \n" index x.(index)
                                                done;
                                                print_newline()
                                     ) mres
              | _ -> raise Wrong_argument_type (* wrong tmpvar type *)
          end;
          command tl macrodefs_lst tmpvar varmap


(*

and     cmd_csv_save_as commandlist macrodefs_lst tmpvar varmap cmd tl argument_list  :  results_t * varmap_t =
          (*  Save the data from a Match_result to a csv-file.                         *)
          (* Data will be made square (equal number of columns per row) before saving! *)
          (* ------------------------------------------------------------------------- *)
          let filename = paste_arglist_to_string  argument_list  varmap in
          begin
            match tmpvar with
              | Match_result mres -> let csv = Csv.of_array mres in
                                     let csvstuff = Csv.square csv in
                                     Csv.save filename csvstuff
              | _ -> raise Wrong_tmpvar_type
          end;
          command tl macrodefs_lst tmpvar varmap



and     cmd_csv_save commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          (*  Save the data from a Match_result to a csv-file.                         *)
          (* Data will be made square (equal number of columns per row) before saving! *)
          (* ------------------------------------------------------------------------- *)
          let url = Parsers.url_to_filename (to_string (Varmap.find "STARTURL" varmap) varmap) in
          ignore ( command [CSV_save_as [String url; String ".csv"] ] macrodefs_lst tmpvar varmap ); (* do the CSV_save with the created filename *)
          command tl macrodefs_lst tmpvar varmap



and     cmd_csv_read commandlist macrodefs_lst tmpvar varmap cmd tl filename_arglist  :  results_t * varmap_t =
          (*  Read data from file <filename> (given as arglist) as csv-data to Match_result *)
          let csv = try
                      Csv.load (paste_arglist_to_string filename_arglist varmap)
                    with Sys_error msg -> raise (Csv_read_error msg)
          in
          let result = Match_result (Csv.to_array csv) in
          command tl macrodefs_lst result varmap

*)


and     cmd_save_as commandlist macrodefs_lst tmpvar varmap cmd tl argument_list  :  results_t * varmap_t =
          let filename = paste_arglist_to_string  argument_list  varmap in
          begin
            match tmpvar with
              | String  str              -> save_string_to_file str filename
              | Document(doc, url)       -> save_string_to_file doc filename
              | Document_array doc_array ->
                                            print_warning "Only the first document is saved with save_as!";
                                            save_string_to_file ( fst doc_array.(0) ) filename
              | _ -> raise Wrong_tmpvar_type
          end;
          command tl macrodefs_lst tmpvar varmap



and     cmd_save commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          let saver (doc, url) = let fname = Parsers.url_to_filename url in
                                 save_string_to_file doc fname
          in

          begin
            match tmpvar with
              | String  str              -> let starturl = to_string (Varmap.find "STARTURL" varmap) varmap  in
                                            let filename = Parsers.url_to_filename starturl in
                                             save_string_to_file str filename

              | Document(doc, url)       -> saver (doc, url)
              | Document_array doc_array -> Array.iter saver doc_array
              | _ -> raise Wrong_tmpvar_type
          end;
          command tl macrodefs_lst tmpvar varmap



and     cmd_storematch commandlist macrodefs_lst tmpvar varmap cmd tl varname  :  results_t * varmap_t =
          verbose_printf "Storematch tmpvar to varname \"%s\"\n" varname;
          let mat = match tmpvar with Match_result mat -> mat | _ ->  raise Wrong_tmpvar_type in
          let mat = Tools.Array2.remove_empty_arrays_from_matrix ~message:true
                                                                 ~msgtxt:"Storematch changed tmpvar! (removed empty cols)"
                                                                 mat     in (* remove empty arrays from matrix *)

          (* generator-function for the name for the named var from idx-values and var-basename *)
          (* ---------------------------------------------------------------------------------- *)
          let create_name  basename rowidx colidx = Printf.sprintf "%s.(%d).(%d)" basename rowidx colidx in

          (* function to add one item to the varmap *)
          (* -------------------------------------- *)
          let add_item_to_map basename rowidx colidx matr map =
              let name = create_name basename rowidx colidx in
              Varmap.add name ( String matr.(rowidx).(colidx) ) map
          in

          (* add_items_to_map adds items to the Variable-map *)
          (* ----------------------------------------------- *)
          let add_items_to_map basename matr map =
            let max_rowidx = Array2.max_row_idx mat in

            let rec aux rowidx colidx map_acc =
              let max_colidx = Array2.max_col_idx_of_row rowidx mat in
              match rowidx, colidx with
                | r, c when r <= max_rowidx && c < max_colidx -> let newmap = add_item_to_map varname rowidx colidx mat map_acc
                                                                 in aux rowidx (colidx + 1) newmap
                | r, c when r <  max_rowidx && c = max_colidx -> let newmap = add_item_to_map varname rowidx colidx mat map_acc
                                                                 in aux (rowidx+1) 0 newmap
                | r, c when r =  max_rowidx && c = max_colidx -> add_item_to_map varname rowidx colidx mat map_acc (* done *)
                | r,c -> Printf.eprintf "r: %d   c: %d\n" r c;
                         raise ( Invalid_argument "Empty matrix - should not never occur (storematch)" )
            in
              aux 0 0 map
          in

          let newmap = add_items_to_map varname mat varmap in

          command tl macrodefs_lst (Match_result mat) newmap (* stores tmpvar as named variable *)



and     cmd_sort commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          let res =
            begin
              match tmpvar with
                | Match_result mres -> Array.sort compare mres; (* in-place sort; unit type *)
                                       Match_result mres        (* give back the sorted array *)

                | Url_list  liste   -> Url_list ( List.sort compare liste )

                | Url_array arr     -> Array.sort compare arr;  (* in-place sort; unit type *)
                                       Url_array arr            (* give back the sorted array *)

                | _ -> raise Wrong_tmpvar_type
            end
          in
            command tl macrodefs_lst res varmap  (* removes multiple data *)



                       (* uniq: make entries unique: ignore multiple entries with same contents *)
and     cmd_uniq commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          let res =
            begin
              match tmpvar with
                | Match_result mres -> let li_of_colarr = Array.to_list mres in
                                       let uniq = Tools.add_item_once li_of_colarr in
                                       Match_result (Array.of_list uniq)

                | Url (href, ref)   -> Url (href, ref)
                | Url_list  liste   -> Url_list ( Tools.add_item_once liste )
                | Url_array arr     -> Url_list ( Tools.add_item_once (Array.to_list arr) )
                | String   str as s -> s

                | String_array  str_arr -> String_array ( Array.of_list ( Tools.add_item_once  (Array.to_list str_arr)) )

                | _ -> raise Wrong_tmpvar_type
            end
          in
            command tl macrodefs_lst res varmap  (* removes multiple data *)



and     cmd_show_variables commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          Varmap.iter ( fun varname value -> Printf.printf "***** \"%s\": " varname;
                                                                           ignore (command [Print; Print_string "\n"] macrodefs_lst value varmap ) ) varmap;
                                        command tl macrodefs_lst tmpvar varmap


and     cmd_list_variables commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          Varmap.iter ( fun varname value -> Printf.printf "***** \"%s\"\n" varname ) varmap;
          command tl macrodefs_lst tmpvar varmap


and     cmd_show_type commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
                                                       Printf.printf "TMPVAR (1-val-stack) contains: %s\n" (Parsetreetypes.result_to_string ~details:true tmpvar);
                                                       command tl macrodefs_lst tmpvar varmap


and     cmd_basename commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          begin
            match tmpvar with
              | String filename -> command tl macrodefs_lst (String(Filename.basename filename)) varmap
              | Url (href, ref) -> command tl macrodefs_lst (String(Filename.basename href)) varmap
              | _ -> raise Wrong_argument_type
          end


and     cmd_subst commandlist macrodefs_lst tmpvar varmap cmd tl (from_re, to_str)  :  results_t * varmap_t =
          verbose_printf "Subst: \"%s\" -> \"%s\"\n" from_re to_str;
          let replacer instring = Pcre.replace ~pat:from_re ~templ:to_str instring in
          begin
          match tmpvar with
            | String str           -> command tl macrodefs_lst (String (replacer str)) varmap

            | String_array str_arr -> let replaced = Array.map replacer str_arr in
                                      command tl macrodefs_lst (String_array replaced) varmap

            | Url (href, ref)      -> command tl macrodefs_lst ( Url (replacer href, replacer ref) ) varmap

            | Url_array   url_arr  -> let changed = Array.map ( fun (u,r) -> (replacer u, replacer r) ) url_arr in
                                       command tl macrodefs_lst ( Url_array changed ) varmap

            | Url_list    url_lst  -> let changed = List.map ( fun (u,r) -> (replacer u, replacer r) ) url_lst in
                                       command tl macrodefs_lst ( Url_list changed ) varmap

            | Document(doc, ref)   -> let newdoc = Document( replacer doc, replacer ref ) in
                                      command tl macrodefs_lst (newdoc) varmap

            | Document_array doc_arr ->
                       let new_docarr = Array.map ( fun (doc,ref) -> (replacer doc, replacer ref) ) doc_arr in
                       command tl macrodefs_lst (Document_array new_docarr) varmap

            | Match_result  arrarr -> let res = Array.map (fun arr -> let a = Array.copy arr in Array.map replacer a ) arrarr in
                                      command tl macrodefs_lst (Match_result res) varmap

            | _ -> raise Wrong_argument_type
          end



and     cmd_quote commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          let str = to_string tmpvar varmap in
          let quoted = "\"" ^ str ^ "\"" in
          command tl macrodefs_lst (String (quoted)) varmap



and     cmd_dump commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          begin
          match tmpvar with
            | Document(doc, url)-> Parsers.dump_html_from_string doc
            | Document_array doc_arr -> let docs = Array.map fst doc_arr in
                                        Array.iter Parsers.dump_html_from_string docs
            | _ -> raise Wrong_argument_type
          end;
          command tl macrodefs_lst tmpvar varmap



and     cmd_show_tags commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          begin
          match tmpvar with
            | Document(doc, url)-> Parsers.show_tags_from_string doc
            | _ -> raise Wrong_argument_type
          end;
          command tl macrodefs_lst tmpvar varmap



and     cmd_show_tags_fullpath commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          begin
          match tmpvar with
            | Document(doc, url)-> Parsers.show_tags_fullpath_from_string doc
            | _ -> raise Wrong_argument_type
          end;
          command tl macrodefs_lst tmpvar varmap



and     cmd_dump_data commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          begin
          match tmpvar with
            | Document(doc, url)-> Parsers.dump_html_data_from_string doc
            | _ -> raise Wrong_argument_type
          end;
          command tl macrodefs_lst tmpvar varmap



and     cmd_system commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          begin
            match tmpvar with
              | String syscmd -> (* verbosity-message *)
                                 (* ----------------- *)
                                 verbose_printf "System-cmd: %s" syscmd;

                                 (* do the work *)
                                 (* ----------- *)
                                 if Cli.opt.Cli.safe = false
                                 then
                                    ignore( Sys.command syscmd )
                                 else
                                   (Printf.fprintf stderr "*** Command not invoked: %s\n" syscmd)
              | _ -> raise Wrong_argument_type
          end;
          command tl macrodefs_lst tmpvar varmap



and     cmd_html_decode commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          let sd str = Tools.select_decoding_scheme str in
          let newvar =
            begin
            match tmpvar with
              | String str            -> String ( Tools.html_decode str )
              | String_array strarr   -> String_array ( Array.map Tools.html_decode strarr )
              | Document(doc, url)    -> let inenc = sd doc in
                                         begin
                                           try
                                             Document( Tools.html_decode ~inenc:inenc doc, Tools.html_decode ~inenc:inenc url )
                                           with _ -> raise Html_decode_error
                                         end
              | Document_array docarr -> Document_array( Array.map (fun (d,u) -> (Tools.html_decode d, Tools.html_decode u) ) docarr )
              | Url  (url, ref)       -> Url( Tools.html_decode url, Tools.html_decode ref )
              | Url_list    urllist   -> Url_list( List.map (fun (u,r) -> (Tools.html_decode u, Tools.html_decode r) ) urllist)
              | Url_array urlarr      -> Url_array( Array.map (fun (u,r) -> (Tools.html_decode u, Tools.html_decode r) ) urlarr )


              | Match_result  arrarr -> let res = Array.map (fun arr -> let a = Array.copy arr in Array.map Tools.html_decode a ) arrarr in
                                        (Match_result res)

              | _ -> raise Wrong_argument_type
            end
          in
          command tl macrodefs_lst newvar varmap



and     cmd_readline commandlist macrodefs_lst tmpvar varmap cmd tl arg_opt :  results_t * varmap_t =
          let read_line = String ( read_line() ) in
          begin
            match arg_opt with
              | None         -> command tl macrodefs_lst read_line varmap
              | Some varname -> command tl macrodefs_lst tmpvar ( Varmap.add varname read_line varmap )
          end



and     cmd_json_prettify commandlist macrodefs_lst tmpvar varmap cmd tl :  results_t * varmap_t =
          let newval =
            match tmpvar with
              | String str ->
                              begin
                                try
                                  String (Yojson.Safe.prettify str)
                                with
                                  Yojson.Json_error _ -> (* if conversion fails string has wrong contents *)
                                                         prerr_endline "Json_prettify failed";
                                                         raise Conversion_error
                              end
              | _ -> raise Wrong_argument_type
          in
            command tl macrodefs_lst newval varmap



and     cmd_call_macro commandlist macrodefs_lst tmpvar varmap cmd tl macro_name  :  results_t * varmap_t =
          (* evaluating the commands of the macro, and afterwards the following commands   *)
          (* that means: prepend the commands of the macro to the tail of the command-list *)
          (* ----------------------------------------------------------------------------- *)
          let macro_commandlist = (List.assoc macro_name macrodefs_lst) in
          let stmts_tl = List.map ( fun cmd -> Command cmd ) tl in
          evaluate_statement ( List.append  macro_commandlist  stmts_tl ) macrodefs_lst tmpvar varmap




(* ------------- INSERT FUNCTION ABOVE -------------------------------------------- *)




(* ================================================== *)
(* This function is the entry-point to the functions, *)
(* that perform the statement-/command-evaluation.    *)
(* ================================================== *)
let evaluate_statement_list (stmtlst : statements_t list) (macrodefs_lst : macrodef_t list) = 
      evaluate_statement stmtlst macrodefs_lst Empty Varmap.empty (* hier geht's los *)



