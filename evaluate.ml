(*
  any-dl:
  -------
  Generic Media-Downloader for any kind of Online-Mediathek.
  Attempts to be the general tool, doing things like
  youtube-dl, zdf-dl, arte-dl and so on.


  Author / copyright: Oliver Bandel
  Copyleft: GNU GENERAL PUBLIC LICENSE  v3 (or higher)
*)


open Parsetreetypes
open Tools


exception NOT_IMPLEMENTED_SO_FAR (* for planned, but not already implemented functionality *)

exception Value_conversion_unknown  (* type conversion, that can't handle this special item (similar to "Wrong_tmpvar_type") *)

exception No_document_found             (* a dcoument could not be retrieved *)
exception Tagselect_empty_list          (* tagselect gives back an empty list *)
exception No_Match                      (* if a match was tried, but no match could be found *)
exception No_Matchresult_available      (* if Select is used, but there is no match-result available as tmpvar *)
exception No_Matchable_value_available  (* if Match is used, but there is no matchable tmpvar *)

exception Wrong_tmpvar_type             (* if tmpvar has just the wrong type... without more detailed info *)
exception Wrong_argument_type           (* e.g. Show_match on non-match *)

exception Invalid_Row_Index             (* indexing a row that does not exist *)
exception Invalid_Col_Index             (* indexing a col that does not exist *)

exception Extractor_list_failure

exception Variable_not_found of string  (* a variable-name lookup in the Varname-map failed *)





(* Module for Variables *)
(* -------------------- *)
module Varmap =
  struct
    module Variablemap = Map.Make( String )

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
        | "NOW" -> let open Unix in String( string_of_float ( time() ) )
        | _     -> try Variablemap.find varname varmap with Not_found -> raise (Variable_not_found varname)

    (* find with an exception-default value                                               *)
    (* This function allows to set a default value in case the lookup yields in Not_found *)
    (* ---------------------------------------------------------------------------------- *)
    let find_excdef varname varmap default =
      match varname with
        | "NOW" -> let open Unix in String( string_of_float ( time() ) )
        | _     -> try Variablemap.find varname varmap with Not_found -> default

  end




(* ---------------------------------------------- *)
(* functional, not thorough nifty-details printer *)
(* intended to make basic functionality working   *)
(* more fancy converters for other purposes might *)
(* elsewehere be defined                          *)
(* ---------------------------------------------- *)
let rec  to_string  result_value varmap =

  let array_string_append str_arr = Array.fold_left ( ^ ) "" str_arr in

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
      | Match_result  mres         -> let x = Array.map array_string_append mres in array_string_append x
      | Url           (href, ref)  -> href
      | Url_list      url_list     -> List.fold_right ( fun a sofar -> "\"" ^ (fst a) ^ "\" " ^ sofar ) url_list ""
      | Url_array     url_arr      -> to_string  (Url_list ( Array.to_list url_arr)) varmap
      | Empty                      -> ""
      (*
      | Doclist       dl           -> Parsers.convert_doclist_to_htmlstring dl
      *)
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
let rec  urlify  result_value varmap =
  let make_referrer () = to_string ( Varmap.find_excdef "REFERRER" varmap (String "-") ) varmap in
  let str =
    match result_value with
      | Varname       varname      -> let res = (Varmap.find varname varmap) in
                                      urlify res varmap
      | String        str          -> Url(str, make_referrer() )
      | Document      (doc, url)   -> raise Value_conversion_unknown (* like Wrong_argument_type *)
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
      | _ -> print_warning "to_string-function found non-convertable type"; raise Wrong_argument_type (* just in case more cases will be added *)

  in
    str



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
let paste_arglist_to_string  argument_list  varmap =
  let str_lst = List.map (fun item ->  to_string item varmap) argument_list in (* convert to string  *)
  let pasted  = List.fold_left ( ^ ) "" str_lst in                             (* append all strings *)
  pasted





(* =================================================================== *)
(* apply a function to the value, giving back a value of the same type *)
(* =================================================================== *)
let default_application variable basefunc varmap =
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



(* ------------------------------------------------- *)
(* This function evaluates the list of commands that *)
(* a parser consists of.                             *)
(* this function is doing the main work of any-dl.   *)
(* ------------------------------------------------- *)
let evaluate_command_list cmdlst macrodefs_lst =

  (* "get_document"-function, is used by some of the Get_... commands from "command"-function *)
  (* ======================================================================================== *)
  let rec get_document  url referrer varmap =

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
    let document_and_cookies =  Network.Pipelined.get_raw url (Some referrer) send_cookie in

    begin
     match document_and_cookies with
       | None                -> None
       | Some (doc, cookies) -> 
                                let new_varmap = (Varmap.add "COOKIES.RECEIVED" (Cookies cookies) varmap) in
                                Some (doc, url, new_varmap)
    end
    

  (* "download"-function, is for downloading big files firectly into a destination-file *)
  (* ================================================================================== *)
  and download  url referrer  dst_file  varmap =

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
    let response_cookies = Network.Pipelined.download url (Some referrer) send_cookie dst_file in

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
  and get_document_list  urls_refs varmap =
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



  (* "command"-function is the main commands-parser/evaluator *)
  (* ======================================================== *)
  and     command commandlist tmpvar varmap =
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
      | []        -> () (* Printf.printf "<========================== BACK. Leave evaluate_command_list() now!\n"*)
      | cmd::tl   -> begin
                       match cmd with
                         | Download  fname_arglist_opt      ->
                                                        begin
                                                          match tmpvar with
                                                            | Url (u,r)          -> let filename =
                                                                                      match fname_arglist_opt with
                                                                                        | None -> Parsers.url_to_filename u (* auto-filename, from URL    *)
                                                                                        | Some arg_list -> paste_arglist_to_string arg_list varmap  (* filename given as argument *)
                                                                                    in

                                                                                    let new_varmap_opt = download  u r filename varmap in
                                                                                    begin
                                                                                      match new_varmap_opt with
                                                                                        | None       -> command tl tmpvar varmap
                                                                                        | Some newvm -> command tl tmpvar newvm
                                                                                    end
                                                            | Url_list ul        ->
                                                                                    (* Download on Url_list necessarily use auto-filenaming *)
                                                                                    List.iter ( fun (url,ref) -> command [ Download None ] (Url (url, ref)) varmap ) ul
                                                            | Url_array ua        ->
                                                                                    (* Download on Url_array necessarily use auto-filenaming *)
                                                                                    Array.iter ( fun (url,ref) -> command [ Download None ] (Url (url, ref)) varmap ) ua

                                                            | _ -> raise Wrong_tmpvar_type
                                                          end


                         | Get_url (url, referrer)  -> if Neturl.extract_url_scheme url = "file"
                                                       then
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
                                                           command tl (Document (contents, "-")) varmap

                                                         end
                                                       else
                                                         begin
                                                         match get_document  url referrer varmap with
                                                           | Some ( doc, url, new_varmap ) -> command tl (Document (doc, url)) new_varmap (* $URL *)
                                                           | None                          -> raise No_document_found
                                                         end


                         | Get             -> (* This is not directly downloading the data; just inserting    *)
                                              (* the appropriate downloader-Tokens into the Tokenlist / "AST" *)
                                              (* ------------------------------------------------------------ *)
                                              begin
                                                match tmpvar with
                                                  | Url (u,r)          -> command (Get_url (u,r) :: tl) tmpvar varmap
                                                  | Url_list  urllist  -> command (Get_urls :: tl) tmpvar varmap
                                                  | Url_array urlarray -> command (Get_urls :: tl) tmpvar varmap
                                                  (* MATCHRES ???
                                                  *)
                                                  | _ -> raise Wrong_tmpvar_type
                                                end


                         | Get_urls        ->
                                              (* If a list or array of URLs must be downloaded, this could be done   *)
                                              (* directly, and data be stored in memory.                             *)
                                              (* But if a save-command follows the get-command in the rc-file,       *)
                                              (* then the data of all downloaded documents will be saved later on,   *)
                                              (* when the the save-command will be executed.                         *)
                                              (* Because all documents need to be held in memory until they have     *)
                                              (* been saved, it would also make sense to do the Save operation       *)
                                              (* immediately after the get-operation for all the URLs.               *)
                                              (* But this ruls is incomplete: between the Get and the Save a         *)
                                              (* Sleep_ms must be inserted, because this Sleeping-time is used for   *)
                                              (* Bulk-downloads already; and this feature must work also after       *)
                                              (* Tokenlist-transformation.                                           *)
                                              (*                                                                     *)
                                              (* Because of the change.of-order of the operations, the Tokenlist     *)
                                              (* will be changed ("AST-optimization").                               *)
                                              (*                                                                     *)
                                              (* But this change of the tokenlist is only allowed, if the next       *)
                                              (* command after the Get-command is a Save-command.                    *)
                                              (* So, there must be a LOOKAHEAD of one Token.                         *)
                                              (*                                                                     *)
                                              (* In other words:                                                     *)
                                              (* If the Get command is used on a LIST/ARRAY of URLs,                 *)
                                              (*     AND                                                             *)
                                              (* the token following the Get-Token is a Save-command-token,          *)
                                              (* then transform the tokenlist  such, that Get is followed by         *)
                                              (* Sleep_ms, and this followed by Save, for each of the requested URLs.*)
                                              (* ------------------------------------------------------------------- *)

                                              (* Ermittle und pruefe Lookahead-Token *)
                                              (* ----------------------------------- *)
                                              let lookahead = List.hd tl in
                                              let next_token_is_savecommand = if lookahead = Save then true else false in


                                              (* two functions for creating the action-list from the url-list *)
                                              (* ------------------------------------------------------------ *)
                                              let get_save_url (url,ref) = [ Get_url (url, ref); Save; Sleep_ms Cli.opt.Cli.ms_sleep ] in
                                              let create_actionlist  url_liste = List.flatten ( List.map get_save_url (List.rev url_liste) ) in


                                              (* Remark: The last Sleep added by this process is not necessary... (one sleep too much) *)

                                              (* ----------------------------------------------------------------------- *)
                                              (* here the download-actions will be done, either directly or via creating *)
                                              (* a tokenlist that will do Get-Sleep-Save action-triplets, and prepends   *)
                                              (* these actions before the tail of the former command-list.               *)
                                              (* ----------------------------------------------------------------------- *)
                                              begin
                                                match tmpvar with
                                                  | Url_list  urllist  -> prerr_endline "Should now get Documents!";
                                                                          if next_token_is_savecommand
                                                                          then
                                                                            let actionlist = create_actionlist urllist in
                                                                            command (List.append actionlist tl) tmpvar varmap
                                                                          else
                                                                            let docs, vm = get_document_list  urllist varmap in
                                                                            command tl (Document_array (Array.of_list docs)) vm

                                                  | Url_array urlarray -> prerr_endline "Should now get Documents!";
                                                                          let urllist = Array.to_list urlarray in

                                                                          if next_token_is_savecommand
                                                                          then
                                                                            let actionlist = create_actionlist urllist in
                                                                            command (List.append actionlist tl) tmpvar varmap
                                                                          else
                                                                            let docs, vm = get_document_list  (Array.to_list urlarray) varmap in
                                                                            command tl (Document_array (Array.of_list docs)) vm

                                                  | _                -> raise Wrong_tmpvar_type
                                              end


                           (* creates url and puts it into tmpvar *)
                         | Make_url_tmpvar -> command tl (urlify tmpvar varmap) varmap
                                              (*
                                              let (url, referrer) = (to_string tmpvar varmap, "-") in
                                              command tl (Url( url, referrer)) varmap
                                              urlify tmpvar varmap
                                              *)

                         | Make_url (u,r)  -> let (url, referrer) = (to_string u varmap, to_string r varmap) in
                                              command tl (Url( url, referrer)) varmap



                         | Match   pattern            ->
                                                         verbose_printf "MATCH-PATTERN: \"%s\"\n" pattern; (* devel-debug-info *)

                                                         let str =
                                                           begin
                                                             match tmpvar with
                                                               | Document (doc, url) -> doc
                                                               | String    s         -> s
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
                                                         command tl (Match_result matched) varmap


                         | Grep pattern_arglist       -> let pattern = paste_arglist_to_string  pattern_arglist  varmap in (* create pattern-string from argument-list *)
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

                                                               | Match_result mres -> Match_result ( Array2.filter_row_by_colmatch test_match_on_string mres )

                                                               | _            -> prerr_endline "Grep: nothing to match"; raise No_Matchresult_available
                                                           end
                                                         in
                                                           command tl grepped varmap

                         | Grep_v pattern_arglist     ->  (* grep -v *)

                                                         let pattern = paste_arglist_to_string  pattern_arglist  varmap in (* create pattern-string from argument-list *)

                                                         let test_match_on_string = test_pattern_match_on_string  pattern in (* partially application to the pattern *)
                                                         let test_nonmatch_on_string str = not (test_match_on_string str)  in (* partially application to the pattern *)

                                                         (*
                                                           if Pcre.pmatch ~pat:".any-dl.rc: No such file or directory" msg
                                                         *)
                                                         let grepped = 
                                                           begin
                                                             match tmpvar with
                                                               | Document (doc, url) -> let lines = Tools.lines_of_string doc in
                                                                                        let selected_lines = ( List.filter test_nonmatch_on_string lines ) in
                                                                                        String_array (Array.of_list selected_lines )

                                                               | String_array str_arr -> String_array( Array2.filter ( fun elem -> test_nonmatch_on_string elem ) str_arr)
                                                               | Url_array    url_arr -> Url_array (Array2.filter ( fun (url,ref) -> test_nonmatch_on_string url &&
                                                                                                                                          test_nonmatch_on_string ref ) url_arr )

                                                               | Match_result mres -> Match_result ( Array2.filter_row_by_colmatch
                                                                                                         ( fun x -> test_nonmatch_on_string x ) mres )
                                                               | _            -> prerr_endline "Grep_v: nothing to match"; raise No_Matchresult_available
                                                           end
                                                         in
                                                           command tl grepped varmap

                         | Select index               -> 
                                                         begin
                                                           match tmpvar with
                                                             | String_array rowitems -> command tl (String(rowitems.(index))) varmap
                                                             | Url_array    rowitems -> command tl (Url( fst(rowitems.(index)), snd(rowitems.(index)))) varmap
                                                             | _            -> prerr_endline "Select: nothing to match"; raise No_Matchresult_available
                                                         end


                         | MSelect index_list         -> 
                                                         begin
                                                           match tmpvar with
                                                             | String_array rowitems -> command tl (String_array(item_selection rowitems index_list)) varmap
                                                             | Url_array    rowitems -> command tl (Url_array(item_selection rowitems index_list)) varmap
                                                             | _            -> prerr_endline "MSelect: nothing to match"; raise No_Matchresult_available
                                                         end

                                                           

                         | ColSelect   col_index        ->
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
                                                                                          command tl (String_array res) varmap
                                                                                        end
                                                                                      else
                                                                                        raise Invalid_Col_Index
                                                                                    end
                                                             | _ -> print_warning "ColSelect: wrong type!!!"; raise Wrong_tmpvar_type
                                                         end


                         | RowSelect   index            ->
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
                                                         command tl !res varmap


                                                           

                         (* Drops a column from a matchres *)
                         (* ------------------------------ *)
                         | DropCol   col_index        ->
                                                         begin
                                                           match tmpvar with
                                                             | Match_result mres ->
                                                                        let dropres = Array.copy mres in
                                                                        Array.iteri ( fun idx the_row -> 
                                                                                               dropres.(idx) <- array_drop the_row col_index (* !!! *)
                                                                                    ) mres;
                                                                        command tl (Match_result dropres) varmap
                                                             | _ -> raise Wrong_argument_type (* wrong tmpvar type *)
                                                         end


                         (* Drops a row from a matchres *)
                         (* --------------------------- *)
                         | DropRow   index            ->
                                                         let res =
                                                           begin
                                                             match tmpvar with
                                                               | Match_result mres -> Match_result (array_drop mres index)
                                                               | _                 -> print_warning "DropRow: wrong type!!!"; raise Wrong_tmpvar_type
                                                           end
                                                         in
                                                           command tl res varmap

                         | Select_match ( col_idx, matchpat) -> (* select match is a row-select, where the index *)
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
                                                                            command tl (String_array (List.hd selected)) varmap (* first match wins *)

                                                                     | _ -> print_warning "RowSelect: wrong type!!!"; raise Wrong_tmpvar_type
                                                                 end


(*
 - iselectmatch with 3 parameters would be good:
     * selection-source
     * selection-match-string
     * default-selection
*)

                         | I_Select_match ( col_idx, matchpat, default_pattern )

                                                             -> (* select match is a row-select, where the index *)
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

                                                                                command tl (String_array (List.hd selected)) varmap (* first match wins *)
    
                                                                       | _ -> print_warning "RowSelect: wrong type!!!"; raise Wrong_tmpvar_type
                                                                   end



                         (* ------------------------------------------------------------------------------ *)
                         (* to matchres does conbverts to a matchres-like result (matrix (array of array)) *)
                         (* Urls will be converted in a way, that the link and the referrer will pop up as *)
                         (* seperate coulmns in each rows. With dropcol() they can be kiekced out!         *)
                         (* ------------------------------------------------------------------------------ *)
                         | To_matchres                -> 
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
                                                         command tl new_var varmap

                         | Transpose                  ->
                                                         let result =
                                                         begin
                                                           match tmpvar with
                                                              | Match_result    match_res  -> Match_result ( Tools.transpose match_res )
                                                              | _ -> raise Wrong_argument_type
                                                         end;
                                                         in
                                                         command tl result varmap





                         | Link_extract               ->
                                                         begin

                                                           (* extracted urls can be rebased with this function *)
                                                           let rebase_urls   url_list  parent_url =
                                                               List.fold_right ( fun lnk sofar -> match Parsers.Rebase.rebase_url parent_url lnk with
                                                                                                    | Some rebased -> (rebased,  parent_url) :: sofar
                                                                                                    | None         -> sofar
                                                                               ) url_list []
                                                           in

                                                           (* extract urls and rebase these extracted urls *)
                                                           let extract_and_rebase document url =
                                                               let extracted_urls = Parsers.linkextract_str document in
                                                               List.iter (fun x -> verbose_fprintf stdout "---extracted url: %s\n" x) extracted_urls;
                                                               rebase_urls extracted_urls url
                                                           in


                                                           verbose_printf "%s" "Link_extract\n";

                                                           match tmpvar with
                                                             | Document (doc, url) ->
                                                                       let rebased_urls = extract_and_rebase doc url               in
                                                                       let links        = Url_array ( Array.of_list rebased_urls ) in
                                                                      command tl links varmap

                                                             | Document_array doc_url_array ->
                                                                  let rebased = List.map ( fun (doc,url) -> extract_and_rebase doc url ) (Array.to_list doc_url_array) in
                                                                  let rebased = List.flatten rebased in
                                                                    let links        = Url_array ( Array.of_list rebased ) in
                                                                    command tl links varmap


                                                             | _ -> print_warning "Link_extract found non-usable type"; raise Wrong_tmpvar_type
                                                         end


                         | Link_extract_xml           ->
                                                         begin
                                                           match tmpvar with
                                                             | Document(doc, url)-> let urls   = Array.of_list (Parsers.xml_get_href_from_string doc) in
                                                                                    (* the url of the doecument will become the referrer of the extracted url! *)
                                                                                    let links  = Url_array (Array.map ( fun lnk -> (lnk, url) ) urls) in
                                                                                    command tl links varmap
                                                             | _ -> print_warning "Link_extract_xml found non-usable type"; raise Wrong_tmpvar_type
                                                         end

                         | Rebase               ->
                                                         let starturl = to_string (Varmap.find "STARTURL" varmap) varmap  in
                                                         let rebase   = Parsers.Rebase.try_rebase  starturl                in

                                                         let result =
                                                           begin
                                                             match tmpvar with
                                                                | String          s          -> String ( rebase s )
                                                                | String_array    str_arr    -> String_array ( Array.map rebase str_arr )
                                                                | Match_result    match_res  -> Match_result ( Array.map ( fun x -> Array.map rebase x ) match_res )

                                                                | _ -> print_warning "Rebase found non-usable type"; raise Wrong_tmpvar_type
                                                           end
                                                         in
                                                         command tl result varmap

                           | Title_extract            ->
                                                         begin
                                                           match tmpvar with
                                                             | Document (doc, url) ->
                                                                       let result = Array.of_list (Parsers.titleextract_str doc) in
                                                                       command (Subst ("\n", "") :: tl) (String_array result) varmap

                                                             | Document_array docarr ->  let doc_url_list = (Array.to_list docarr)    in
                                                                                         let titlili      = List.map ( fun (d,u) -> Parsers.titleextract_str d  ) doc_url_list in
                                                                                         let titli        = List.flatten titlili in
                                                                                         let result = Array.of_list titli in
                                                                                         command (Subst ("\n", "") :: tl) (String_array result) varmap

                                                             | _ -> print_warning "Title_extract found non-usable type"; raise Wrong_tmpvar_type
                                                         end


                         | Tag_select (selector, extractor )  ->
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

                                                         command tl result varmap



                         | Paste paste_list            ->
                                                          let res = paste_arglist_to_string  paste_list  varmap in
                                                          command tl (String res) varmap



                         | Print_args prt_args         ->
                                                          command [ Paste( prt_args ); Print ] Empty varmap; (* use the Paste-command and the print-command *)
                                                          command tl tmpvar varmap (* just next command without changed tmpvar *)

                         | Print                      ->
                                                         begin
                                                           match tmpvar with
                                                             (* does Varname makes sense at all here? *)
                                                             | Varname  varname  -> Printf.printf "\n\tVarname  varname => varname = \"%s\"\n" varname;
                                                                                    command [Print] (Varmap.find varname varmap) varmap (* CHECK FUNCTIONALITY, PLEASE *)

                                                             | String   str      -> print_endline str 
                                                             | Document(doc, url)-> print_endline doc  (* only print the document, without referrer *)

                                                             | Document_array doc_arr -> (* only print the documents, without referrer *)
                                                                                         Array.iter (fun (doc,ref) -> print_endline doc;
                                                                                                                      print_endline "----------------------------------" ) doc_arr

                                                             | Match_result mres -> Array.iter ( fun x -> Array.iter ( fun y -> Printf.printf "\"%s\" ||| " y) x;
                                                                                                          print_newline() ) mres
                                                             | String_array     str_arr -> Array.iter ( fun str -> Printf.printf "\"%s\" \n" str) str_arr
                                                             | Url (href, ref)   -> Printf.printf "%s   # Referrer:  %s\n" href ref
                                                             | Url_list  liste    -> List.iter  ( fun (href, ref) -> Printf.printf "%s  # Referrer:  %s\n" href ref) liste
                                                             | Url_array arr      -> Array.iter ( fun (href, ref) -> Printf.printf "%s  # Referrer:  %s\n" href ref) arr

                                                             (*
                                                             | Doclist   doclist  -> let string_of_dl dl = Parsers.convert_doclist_to_htmlstring [dl] in
                                                                                     List.iter ( fun doc -> print_endline ( string_of_dl doc ) ) doclist (* one per line *)
                                                             *)

                                                             | _ -> print_warning "Print-command found non-printable type"
                                                         end;
                                                         command tl tmpvar varmap


                         | Show_match                -> (* prints "real" matches only (and not the fullmatch with index = 0) *)
                                                         begin
                                                           match tmpvar with
                                                             | Match_result mres ->
                                                                        print_endline "for real matches: show_match: Col 0 is the whole match, all others are the groups\n";
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
                                                         command tl tmpvar varmap


                         | Print_string str           -> print_string str;
                                                         command tl tmpvar varmap


                         | CSV_save_as argument_list -> (*  Save the data from a Match_result to a csv-file.                         *)
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
                                                         command tl tmpvar varmap



                         | CSV_save                  -> (*  Save the data from a Match_result to a csv-file.                         *)
                                                        (* Data will be made square (equal number of columns per row) before saving! *)
                                                        (* ------------------------------------------------------------------------- *)
                                                         let url = Parsers.url_to_filename (to_string (Varmap.find "STARTURL" varmap) varmap) in
                                                         command [CSV_save_as [String url; String ".csv"] ] tmpvar varmap; (* do the CSV_save with the created filename *)
                                                         command tl tmpvar varmap



                         | Save_as      argument_list ->
                                                         let filename = paste_arglist_to_string  argument_list  varmap in
                                                         begin
                                                           match tmpvar with
                                                             | Document(doc, url)       -> save_string_to_file doc filename
                                                             | Document_array doc_array ->
                                                                                           print_warning "Only the first document is saved with save_as!";
                                                                                           save_string_to_file ( fst doc_array.(0) ) filename
                                                             | _ -> raise Wrong_tmpvar_type
                                                         end;
                                                         command tl tmpvar varmap


                         | Save                       ->
                                                         let saver (doc, url) = let fname = Parsers.url_to_filename url in
                                                                             save_string_to_file doc fname
                                                         in

                                                         begin
                                                           match tmpvar with
                                                             | Document(doc, url)       -> saver (doc, url)
                                                             | Document_array doc_array -> Array.iter saver doc_array
                                                             | _ -> raise Wrong_tmpvar_type
                                                         end;
                                                         command tl tmpvar varmap


                         | Setvar var                 ->
                                                         command tl var varmap (* sets the argument of setvar as new tmpvar *)



                         | Store  varname             ->  verbose_printf "Store tmpvar in varname \"%s\"\n" varname;
                                                          command tl tmpvar (Varmap.add varname tmpvar varmap)  (* stores tmpvar as named variable *)


                         | Recall varname             -> verbose_printf "Recall variable: \"%s\"\n" varname;
                                                         let varcontents = Varmap.find varname varmap in
                                                         command tl varcontents varmap

                         | Delete varname             -> verbose_printf "Delete variable \"%s\"\n" varname;
                                                         command tl tmpvar (Varmap.remove varname varmap)  (* removes variable varname *)


                         | Sort                       -> (* sort entries *)
                                                         let res =
                                                           begin
                                                             match tmpvar with
                                                               | Match_result mres -> Array.sort compare mres; (* in-place sort; unit type *)
                                                                                      Match_result mres        (* give back the sorted array *)

                                                               | _ -> raise Wrong_tmpvar_type
                                                           end
                                                         in
                                                           command tl res varmap  (* removes multiple data *)


                         | Uniq                       -> (* uniq: make entries unique: ignore multiple entries with same contents *)
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
                                                           command tl res varmap  (* removes multiple data *)


                         | Show_variables             -> Varmap.iter ( fun varname value -> Printf.printf "***** \"%s\": " varname;
                                                                                            command [Print; Print_string "\n"] value varmap ) varmap;
                                                         command tl tmpvar varmap

                         | List_variables             -> Varmap.iter ( fun varname value -> Printf.printf "***** \"%s\"\n" varname ) varmap;
                                                         command tl tmpvar varmap

                         | Show_type                   -> Printf.printf "TMPVAR (1-val-stack) contains: %s\n" (Parsetreetypes.result_to_string tmpvar);
                                                         command tl tmpvar varmap


                         | Basename                   -> 
                                                         begin
                                                           match tmpvar with
                                                             | String filename -> command tl (String(Filename.basename filename)) varmap
                                                             | Url (href, ref) -> command tl (String(Filename.basename href)) varmap
                                                             | _ -> raise Wrong_argument_type
                                                         end


                         | Subst (from_re, to_str)    -> verbose_printf "Subst: \"%s\" -> \"%s\"\n" from_re to_str;
                                                         let replacer instring = Pcre.replace ~pat:from_re ~templ:to_str instring in
                                                         begin
                                                         match tmpvar with
                                                           | String str           -> command tl (String (replacer str)) varmap

                                                           | String_array str_arr -> let replaced = Array.map replacer str_arr in
                                                                                     command tl (String_array replaced) varmap

                                                           | Url (href, ref)      -> command tl ( Url (replacer href, replacer ref) ) varmap

                                                           | Url_array   url_arr  -> let changed = Array.map ( fun (u,r) -> (replacer u, replacer r) ) url_arr in
                                                                                      command tl ( Url_array changed ) varmap

                                                           | Document(doc, ref)   -> let newdoc = Document( replacer doc, replacer ref ) in
                                                                                     command tl (newdoc) varmap

                                                           | Document_array doc_arr ->
                                                                      let new_docarr = Array.map ( fun (doc,ref) -> (replacer doc, replacer ref) ) doc_arr in
                                                                      command tl (Document_array new_docarr) varmap

                                                           | Match_result  arrarr -> let res = Array.map (fun arr -> let a = Array.copy arr in Array.map replacer a ) arrarr in
                                                                                     command tl (Match_result res) varmap

                                                           | _ -> raise Wrong_argument_type
                                                         end


                         | Quote                      ->
                                                         let str = to_string tmpvar varmap in
                                                         let quoted = "\"" ^ str ^ "\"" in
                                                         command tl (String (quoted)) varmap

                         | To_string                  -> command tl (String (to_string tmpvar varmap)) varmap

                         | Dump                       ->
                                                         begin
                                                         match tmpvar with
                                                           | Document(doc, url)-> Parsers.dump_html_from_string doc
                                                           | Document_array doc_arr -> let docs = Array.map fst doc_arr in
                                                                                       Array.iter Parsers.dump_html_from_string docs
                                                           | _ -> raise Wrong_argument_type
                                                         end;
                                                         command tl tmpvar varmap

                         | Show_tags                  ->
                                                         begin
                                                         match tmpvar with
                                                           | Document(doc, url)-> Parsers.show_tags_from_string doc
                                                           | _ -> raise Wrong_argument_type
                                                         end;
                                                         command tl tmpvar varmap

                         | Show_tags_fullpath         ->
                                                         begin
                                                         match tmpvar with
                                                           | Document(doc, url)-> Parsers.show_tags_fullpath_from_string doc
                                                           | _ -> raise Wrong_argument_type
                                                         end;
                                                         command tl tmpvar varmap

                         | Dump_data                  ->
                                                         begin
                                                         match tmpvar with
                                                           | Document(doc, url)-> Parsers.dump_html_data_from_string doc
                                                           | _ -> raise Wrong_argument_type
                                                         end;
                                                         command tl tmpvar varmap

                         | System                     ->
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
                                                         command tl tmpvar varmap

                         | Exit_parse                 -> flush stdout; prerr_endline "Parse was exited."; command [] tmpvar varmap (* call again with nothing-left-to-do *)


                         | Html_decode                ->
                                                         let sd str = Tools.select_decoding_scheme str in
                                                         let newvar =
                                                           begin
                                                           match tmpvar with
                                                             | String str            -> String ( Tools.html_decode str )
                                                             | String_array strarr   -> String_array ( Array.map Tools.html_decode strarr )
                                                             | Document(doc, url)    -> let inenc = sd doc in
                                                                                        Document( Tools.html_decode ~inenc:inenc doc, Tools.html_decode ~inenc:inenc url )
                                                             | Document_array docarr -> Document_array( Array.map (fun (d,u) -> (Tools.html_decode d, Tools.html_decode u) ) docarr )
                                                             | Url  (url, ref)       -> Url( Tools.html_decode url, Tools.html_decode ref )
                                                             | Url_list    urllist   -> Url_list( List.map (fun (u,r) -> (Tools.html_decode u, Tools.html_decode r) ) urllist)
                                                             | Url_array urlarr      -> Url_array( Array.map (fun (u,r) -> (Tools.html_decode u, Tools.html_decode r) ) urlarr )


                                                             | Match_result  arrarr -> let res = Array.map (fun arr -> let a = Array.copy arr in Array.map Tools.html_decode a ) arrarr in
                                                                                       (Match_result res)

                                                             | _ -> raise Wrong_argument_type
                                                           end
                                                         in
                                                         command tl newvar varmap

                         | Url_decode                -> let newvar = default_application tmpvar Netencoding.Url.decode varmap in
                                                        command tl newvar varmap


                         | Readline  arg_opt          ->
                                                         let read_line = String ( read_line() ) in
                                                         begin
                                                           match arg_opt with
                                                             | None         -> command tl read_line varmap
                                                             | Some varname -> command tl tmpvar ( Varmap.add varname read_line varmap )
                                                         end



                         | Sleep_ms  milliseconds     -> Sleep.sleep_ms milliseconds;
                                                         command tl tmpvar varmap


                         | Call_macro     macro_name  -> (* evaluating the commands of the macro, and afterwards the following commands   *)
                                                         (* that means: prepend the commands of the macro to the tail of the command-list *)
                                                         (* ----------------------------------------------------------------------------- *)
                                                         let macro_commandlist = (List.assoc macro_name macrodefs_lst) in
                                                         command ( List.append  macro_commandlist  tl ) tmpvar varmap


                         | Dummy                      -> command tl tmpvar varmap (* does nothing; just a Dummy (NOP) *)

                     end


    in
      command cmdlst Empty Varmap.empty



