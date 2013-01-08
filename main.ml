(*
  any-dl:
  -------
  Generic Media-Downloader for any kind of Online-Mediathek.
  Attempts to be the general tool, doing things like
  youtube-dl, zdf-dl, arte-dl and so on.


  Author / copyright: Oliver Bandel
  Copyleft: GNU GENERAL PUBLIC LICENSE  v3 (or higher)



  already working:
  ----------------
    - ZDF
    - ORF

  planned for the future:
  -----------------------
    - ARTE
    - VIMEO
    - youtube

    ...

*)


open Parsetreetypes


exception NOT_IMPLEMENTED_SO_FAR (* for planned, but not already implemented functionality *)
exception Command_Sequence_error of string (* for sequences that are not allowed *)

exception No_document_found         (* a dcoument could not be retrieved *)
exception No_Match                  (* if a match was tried, but no match could be found *)
exception No_Matchresult_available  (* if Select is used, but there is no match-result available as tmpvar *)
exception No_Matchable_value_available  (* if Match is used, but there is no matchabe tmpvar *)


exception Wrong_tmpvar_type             (* if tmpvar has just the wrong type... without more detailed info *)
exception Wrong_argument_type           (* e.g. Show_match on non-match *)

exception Invalid_Row_Index             (* indexing a row that does not exist *)

exception No_parser_found_for_this_url (* *)

exception No_String_representation     (* To_string called on a value that has no way conversion so far *)

exception Variable_not_found of string   (* a variable-name lookup in the Varname-map failed *)



(* ------------------------------------------------ *)
(* ------------------------------------------------ *)
(* ------------------------------------------------ *)
let print_warning str = flush stdout; prerr_string "WARNING: "; prerr_endline str

(* ------------------------------------------------ *)
(* select those items from the row_items, which are *)
(* indexed by the values in the index_list          *)
(* ------------------------------------------------ *)
let item_selection row_items index_list =
  let res_len = List.length index_list in
  let res     = Array.make res_len row_items.(0) in
  let index_arr = Array.of_list index_list in

  for res_index = 0 to Array.length index_arr - 1
  do
    res.(res_index) <- row_items.(index_arr.(res_index))
  done;
  res




(* Module for Variables *)
(* -------------------- *)
module Varmap =
  struct
    module Varmap = Map.Make( String )

    let empty = Varmap.empty
    let add   = Varmap.add
    let iter  = Varmap.iter

    let find varname varmap =
      try Varmap.find varname varmap with Not_found -> raise (Variable_not_found varname)

  end



exception Fuck_Match_result

(* ---------------------------------------------- *)
(* functional, not thorough nifty-details printer *)
(* intended to make basic functionality working   *)
(* more fancy converters for other purposes might *)
(* elsewehere be defined                          *)
(* ---------------------------------------------- *)
let rec  to_string  result_value varmap =
  let str =
    match result_value with
      | Varname       varname     -> let res = (Varmap.find varname varmap) in
                                     begin
                                       match res with
                                         | String str -> str
                                         | _ as again -> to_string again varmap
                                     end
      | String        str         -> str 
      | Document      (doc, url)  -> doc ^ url
      | String_array  str_arr     -> Array.fold_left ( ^ ) "" str_arr
      | Match_result  mres        -> raise Fuck_Match_result (* not implemented so far !! *)
      | Url           (href, ref) -> href
      (*
      | Url           (href, ref) -> Printf.sprintf "%s # referrer = %s" href ref
      | Url_list  liste    -> List.iter  ( fun (href, ref) -> Printf.printf "%s  # Referrer:  %s\n" href ref) liste
      | Url_array liste    -> Array.iter ( fun (href, ref) -> Printf.printf "%s  # Referrer:  %s\n" href ref) liste
      | Result_selection str_arr -> Array.iter ( fun str -> print_endline str; print_newline()) str_arr
      | Match_result mres -> Array.iter ( fun x -> Array.iter ( fun y -> Printf.printf "\"%s\" ||| " y) x;
      *)
      | _ -> print_warning "to_string-function found non-convertable type"; raise Fuck_Match_result

  in
    str



(* ------------------------------------------------- *)
(* This function evaluates the list of commands that *)
(* a parser consists of.                             *)
(* this function is doing the main work of any-dl.   *)
(* ------------------------------------------------- *)
let evaluate_command_list cmdlst =
  let rec command commandlist tmpvar varmap =
  (*
  Printf.printf "==========================> ENTER. evaluate_command_list() now!\n";
  *)
  flush_all();
  match commandlist with
    | []        -> () (* Printf.printf "<========================== BACK. Leave evaluate_command_list() now!\n"*)
    | cmd::tl   -> begin
                     match cmd with
                       | Get_url (url, referrer)  -> let document = Network.Curly.get url (Some referrer) in
                                                     begin
                                                       match document with
                                                         | Some doc -> command tl (Document (doc, url)) varmap (* $URL *)
                                                         | None     -> raise No_document_found       
                                                     end


                       | Get             -> let (u,r) = begin match tmpvar with Url (u,r) -> u,r | _ -> raise Wrong_tmpvar_type end in
                                            command (Get_url (u,r) :: tl) tmpvar varmap


                       | Get_urls        -> begin
                                              match tmpvar with
                                                | Url_list urllist -> prerr_endline "Should now get Documents!";
                                                                      List.iter ( fun (u,r) -> Printf.printf "url: %s /// referrer: %s\n" u r) urllist
                                                | _                -> raise Wrong_tmpvar_type
                                            end


                         (* creates url and puts it into tmpvar *)
                       | Make_url_tmpvar -> let (url, referrer) = (to_string tmpvar varmap, "-") in
                                            command tl (Url( url, referrer)) varmap

                       | Make_url (u,r)  -> let (url, referrer) = (to_string u varmap, to_string r varmap) in
                                            command tl (Url( url, referrer)) varmap


                       (* hmhh, str sollte doch aus der tmpvar besser entnommen werden !  !!!!!!!!!!!!!! *)
                       | Match   pattern            ->
                                                       if
                                                        Cli.opt.Cli.verbose
                                                       then
                                                         Printf.fprintf stderr "MATCH-PATTERN: \"%s\"\n" pattern; (* devel-debug-info *)

                                                       let str =
                                                         begin
                                                           match tmpvar with
                                                             | Document (doc, url) -> doc
                                                             | _            -> raise No_Matchable_value_available (* this is a type-error Wrong_tmpvar_type *)
                                                         end
                                                       in
                                                       let match_res = Parsers.if_match_give_group_of_groups str (Pcre.regexp pattern) in
                                                       let matched =
                                                         begin
                                                           match match_res with
                                                             | None   -> raise No_Match
                                                             | Some res -> res
                                                         end
                                                       in
                                                       command tl (Match_result matched) varmap


                       (*
                       | Select selfunc             -> 
                                                       begin
                                                         match tmpvar with
                                                           | Match_result matchres -> command tl (Match_result (selfunc matchres)) varmap
                                                           | _           -> prerr_endline "Select: nothing to match"; raise No_Matchresult_available
                                                       end
                       *)

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
                                                           | _            -> prerr_endline "Select: nothing to match"; raise No_Matchresult_available
                                                       end

                                                         

                       (*   BOT READY, is Print-command so far !!! *)
                       | ColSelect   index            ->
                                                       (*
                                                       begin
                                                         match tmpvar with
                                                           | Match_result mres -> Array.iter ( fun x -> Array.iter ( fun y -> Printf.printf "\"%s\" ||| " y) x;
                                                                                                        print_newline() ) mres
                                                           | _ -> print_warning "HSELECT: wrong type!!!"
                                                       end;
                                                       assert(false);
                                                       *)
                                                       raise NOT_IMPLEMENTED_SO_FAR
                                                       (*
                                                       print_endline "ColSelect";
                                                       command tl tmpvar varmap
                                                       *)

                       (*   BOT READY, is Print-command so far !!! *)
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

                       | Link_extract               ->
                                                       begin
                                                         match tmpvar with
                                                           | Document (doc, url) ->
                                                                     let urls   = Array.of_list (Parsers.linkextract doc) in

                                                                     (* the url of the doecument will become the referrer of the extracted url! *)
                                                                     let links  = Url_array (Array.map ( fun lnk ->
                                                                                                           let base = Network.baseurl url in
                                                                                                           let rebased = Parsers.rebase_aggregated lnk base in
                                                                                                           (rebased, url)
                                                                                                       ) urls) in

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
                                                           | _ -> print_warning "Link_extract found non-usable type"; raise Wrong_tmpvar_type
                                                       end

                       | Print                      ->
                                                       begin
                                                         match tmpvar with
                                                           | Varname  varname  -> Printf.printf "\n\tVarname  varname => varname = \"%s\"\n" varname;
                                                                                  command [Print] (Varmap.find varname varmap) varmap (* CHECK FUNCTIONALITY, PLEASE *)
                                                           (*
                                                           | Varname  varname  -> command [Print] (Varmap.find varname varmap) varmap (* CHECK FUNCTIONALITY, PLEASE *)
                                                           *)
                                                           | String   str      -> print_endline str 
                                                           | Document(doc, url)-> print_endline doc  (* only print the document, without referrer *)
                                                           | Match_result mres -> Array.iter ( fun x -> Array.iter ( fun y -> Printf.printf "\"%s\" ||| " y) x;
                                                                                                        print_newline() ) mres
                                                           | String_array     str_arr -> Array.iter ( fun str -> Printf.printf "\"%s\" \n " str) str_arr
                                                           | Url (href, ref)   -> Printf.printf "%s   # Referrer:  %s\n" href ref
                                                           | Url_list  liste    -> List.iter  ( fun (href, ref) -> Printf.printf "%s  # Referrer:  %s\n" href ref) liste
                                                           | Url_array liste    -> Array.iter ( fun (href, ref) -> Printf.printf "%s  # Referrer:  %s\n" href ref) liste
                                                           (*
                                                           | Result_selection str_arr -> Array.iter ( fun str -> print_endline str; print_newline()) str_arr
                                                           *)
                                                           | _ -> print_warning "Print-command found non-printable type"
                                                       end;
                                                       command tl tmpvar varmap


                       | Show_match                -> (* prints "real" matches only (and not the fullmatch with index = 0) *)
                                                       begin
                                                         match tmpvar with
                                                           | Match_result mres ->
                                                                      print_endline "print_match: match 0 is the whole match, all others are the groups\n";
                                                                      Array.iter ( fun x -> 
                                                                                            for index = 0 to Array.length x -1
                                                                                            do
                                                                                              Printf.printf "%2d: \"%s\" \n" index x.(index)
                                                                                            done;
                                                                                            print_newline()
                                                                                 ) mres
                                                           | _ -> raise Wrong_argument_type (* wrong tmpvar type *)
                                                       end;
                                                       command tl tmpvar varmap


                       | Print_string str           -> print_string str;
                                                       command tl tmpvar varmap


                       | Save   _                   -> print_endline "Save detected"; raise NOT_IMPLEMENTED_SO_FAR
                                                       (*
                                                       command tl tmpvar varmap
                                                       *)

                       | Setvar var                 -> command tl var varmap (* sets the argument of setvar as new tmpvar *)



                       | Store  varname             -> command tl tmpvar (Varmap.add varname tmpvar varmap)  (* stores tmpvar as named variable *)


                       | Recall varname             -> if Cli.opt.Cli.verbose then prerr_endline ("Recall: " ^ varname); flush stderr;
                                                       let varcontents = Varmap.find varname varmap in
                                                       command tl varcontents varmap


                       | Show_variables             -> Varmap.iter ( fun varname value -> Printf.printf "***** \"%s\": " varname; command [Print] value varmap ) varmap;
                                                       command tl tmpvar varmap

                       | Show_type                   -> Printf.printf "TMPVAR (1-val-stack) contains: %s\n" (Parsetreetypes.result_to_string tmpvar);
                                                       command tl tmpvar varmap


                       | Paste paste_list            ->
                                                        (*
                                                        List.iter ( fun x -> command [Show_type] x varmap ) paste_list; print_endline "paste: YYEEAAHH! ShowType done";
                                                        List.iter ( fun x -> command [Print] x varmap ) paste_list; print_endline "paste: YYEEAAHH! Print done";
                                                        *)

                                                        let str_lst = List.map (fun item ->  to_string item varmap) paste_list in
                                                        let res = List.fold_left ( ^ ) "" str_lst in
                                                        (*
                                                        Printf.fprintf stdout "***** Paste-result-String: ====================> %s\n" res;
                                                        *)
                                                        flush stdout;
                                                        command tl (String res) varmap



                       | Basename                   -> begin
                                                         match tmpvar with
                                                           | String filename -> command tl (String(Filename.basename filename)) varmap
                                                           | _ -> raise Wrong_argument_type
                                                       end


                       | To_string                  -> command tl (String (to_string tmpvar varmap)) varmap

                       | System                     -> begin match tmpvar with String syscmd -> Sys.command syscmd end;
                                                       command tl tmpvar varmap

                       | Exit_parse                 -> flush stdout; prerr_endline "Parse was exited."; command [] tmpvar varmap (* call again with nothing-left-to-do *)

                       | Dummy                      -> command tl tmpvar varmap (* does nothing; just a Dummy (NOP) *)

                   end


  in
    command cmdlst Empty Varmap.empty







(* of data option get content if Some data was there, otherwise throw exception *)
(* ---------------------------------------------------------------------------- *)
let extract_some_with_exit_if_none  value messages exc = match value with
  | None       -> List.iter prerr_endline messages; raise exc
  | Some stuff -> stuff



(* try to get web-document, otherwise print err-msg and throw exception *)
(* -------------------------------------------------------------------- *)
let get_document url message_list exc =
  let main_doc_opt = Network.Curly.get url None in
  extract_some_with_exit_if_none  main_doc_opt  message_list  exc


(* mainurl is the baseurl including potential path; suburl is either basurl, or rel-url *)
(* if suburl is rel-ur, then grab baseurl from mainurl and prepend it to suburl         *)
(* ------------------------------------------------------------------------------------ *)
let prepend_baseurl_if_necessary  mainurl  suburl =
  if Parsers.url_is_rel_root suburl then Parsers.url_get_baseurl mainurl ^ suburl else suburl



(* result: list of string-matching href hyperlinks *)
(* ----------------------------------------------- *)
let get_href_from_webdoc_and_match  webdoc  matcher =
  let urls = Parsers.linkextract webdoc in
  List.filter (fun url -> matcher url ) urls




(* read the parser-definitions from the rc-file *)
(* -------------------------------------------- *)
let read_parser_definitions filename_opt =
  if Cli.opt.Cli.verbose then Printf.fprintf stderr "rc-filename: %s\n" Cli.opt.Cli.rc_filename;

  let tokenlist = ref [] in

  let input_channel = match filename_opt with None -> stdin | Some filename -> open_in filename in
  let lexer = Lexing.from_channel input_channel in
  begin
    try
      while true do
        let result = Scriptparser.main Scriptlexer.read_command lexer in
        tokenlist := result :: !tokenlist
      done
    with End_of_file -> if Cli.opt.Cli.verbose
                        then prerr_endline "End of rc-file reached; parser definitions were read."

         (*
         | Not_found -> prerr_string "Variable not known in line ";
                        prerr_int !Scriptlex.linenum;prerr_newline()
                        (*
                        exit 1
                        *)
         *)

         | Parsing.Parse_error -> 
                prerr_string "Parse error in line ";
                prerr_int !Scriptlexer.linenum;
                prerr_newline();
                exit 1

  end
  ;
  close_in input_channel;
  List.rev !tokenlist





let _  =
    Cli.parse(); (* parse the command line *)

    (* parse the parser-definitions *)
    (* ---------------------------- *)
    let parserlist = read_parser_definitions (Some Cli.opt.Cli.rc_filename) in

    (* if cli-switches ask for it, print number of parser-definitions *)
    if Cli.opt.Cli.list_parsers || Cli.opt.Cli.verbose then
      Printf.fprintf stderr "Number of found parser definitions: %d\n" (List.length parserlist);


    (* create and initialize hashes for parser-lookup by name / url *)
    (* ------------------------------------------------------------ *)
    let parser_urlhash  = Hashtbl.create (List.length parserlist) in
    let parser_namehash = Hashtbl.create (List.length parserlist) in
    List.iter ( fun parserdef ->
                                 (* add the parsers to the parser_name-hash (for parser-lookup by name) *)
                                 Hashtbl.add parser_namehash parserdef.parsername parserdef;

                                 (* add the parsers to the parser_url-hash (for parser-lookup by url) *)
                                 List.iter ( fun url -> Hashtbl.add parser_urlhash url parserdef;
                                                        if Cli.opt.Cli.list_parsers || Cli.opt.Cli.verbose
                                                        then
                                                          Printf.fprintf stderr "Init: bound Base-URL %-30s -> parser %s\n" url parserdef.parsername
                                           ) parserdef.urllist;

              ) parserlist;


    flush stdout; (* all init-stuff should be flushed, before evaluation stage is entered! *)


    (* for all the URLs from the command line, do the intended work :-) *)
    (* ---------------------------------------------------------------- *)
    List.iter ( fun url ->
                            (* look up the right parser, either via *)
                            (* ------------------------------------ *)
                            let parserdef =
                                try
                                  begin
                                    match Cli.opt.Cli.parser_selection with
                                      | Some parsername -> Hashtbl.find parser_namehash parsername

                                      | None            -> (* name derived from url *)
                                                           let baseurl = Parsers.url_get_baseurl url in 
                                                           Hashtbl.find parser_urlhash baseurl
                                   end
                                with Not_found         -> prerr_endline ("No parser found for " ^ url); raise No_parser_found_for_this_url
                            in

                            try
                              print_endline "# --------------------";

                              (* ---------------------------------------------------------------- *)
                              (* we evaluate the parse-tree, and start with a first, implicit get *)
                              (* with the url we got from the command line                        *)
                              (* ---------------------------------------------------------------- *)
                              evaluate_command_list (Get_url(url, "-") :: parserdef.commands)


                            with (* handle exceptions from the parse-tree-evaluation *)
                              | Invalid_Row_Index -> prerr_endline "Error in script! Invalid_Row_Index!!\n"
                              | Variable_not_found name -> Printf.fprintf stderr "Variable_not_found: \"%s\"\t This parse exited.\n" name



              ) (List.rev Cli.opt.Cli.url_list)




(* --------------------------------------------------------------------------------------------------------------

  HOW TO DUMP STREAMS:
 ======================

rtmp / rtmpt:
  rtmpdump --resume  -r rtmp://.... -y mp4:....  -o outfile.ext

mms:
  mplayer -dumpstream mms://example.com/Globalplayers/GP_14.wmv -dumpfile ./download/test.wmv 

 ------------------------------------------------------------------------------------------------------------- *)
