

(*
  any-dl:
  -------
  Media-Downloader for any kind of Online-Mediathek

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
exception Wrong_argument_type           (* e.g. Print_match on non-match *)

exception Invalid_Row_Index             (* indexing a row that does not exist *)



(*
exception Could_not_find_mediafile_link of string
exception Could_not_extract_ARTE_Ajax_url
exception Could_not_get_ARTE_xml
exception ARD_Rtmp_url_extraction_error
exception ARD_Mp4_url_extraction_error
exception ARD_mp4_url_extraction_error
exception NDR_url_extraction_error

exception Unknown_Base_Url


type selected_t     = string array list
*)
(*
type selector_t     = ( match_result_t -> match_result_t ) (* function, that has a certain algorithm to select certain match_result_t *)

type url_t = { url: string; referrer: string }
*)


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


let print_warning str = prerr_string "WARNING: "; prerr_endline str



let evaluate_command_list cmdlst =
  let rec command commandlist tmpvar = match commandlist with
    | []        -> ()
    | cmd::tl   -> begin
                     match cmd with
                       | Get_url (url, referrer)  -> let document = Network.Curly.get url (Some referrer) in
                                                     begin
                                                       match document with
                                                         | Some doc -> command tl (Document (doc, url))
                                                         | None     -> raise No_document_found       
                                                     end


                       | Get             -> let (u,r) = begin match tmpvar with Url (u,r) -> u,r | _ -> raise Wrong_tmpvar_type end in
                                            command (Get_url (u,r) :: tl) tmpvar


                       | Get_urls        -> begin
                                              match tmpvar with
                                                | Url_list urllist -> prerr_endline "Should now get Documents!";
                                                                      List.iter ( fun (u,r) -> Printf.printf "url: %s /// referrer: %s\n" u r) urllist
                                                | _                -> raise Wrong_tmpvar_type
                                            end


                       (* hmhh, str sollte doch aus der tmpvar besser entnommen werden !  !!!!!!!!!!!!!! *)
                       | Match   pattern            ->
                                                       let str =
                                                         begin
                                                           match tmpvar with
                                                             | Document (doc, url) -> doc
                                                             | _            -> raise No_Matchable_value_available
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
                                                       command tl (Match_result matched)


                       (*
                       | Select selfunc             -> 
                                                       begin
                                                         match tmpvar with
                                                           | Match_result matchres -> command tl (Match_result (selfunc matchres))
                                                           | _           -> prerr_endline "Select: nothing to match"; raise No_Matchresult_available
                                                       end
                       *)

                       | Select index_list          -> 
                                                       begin
                                                         match tmpvar with
                                                           | Row rowitems -> command tl (Row(item_selection rowitems index_list))
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
                                                       *)
                                                       assert(false);
                                                       raise NOT_IMPLEMENTED_SO_FAR; 
                                                       print_endline "ColSelect";
                                                       command tl tmpvar

                       (*   BOT READY, is Print-command so far !!! *)
                       | RowSelect   index            ->
                                                       let res = ref Empty in
                                                       print_endline "RowSelect";
                                                       begin
                                                         match tmpvar with
                                                           | Match_result mres ->
                                                                                  begin
                                                                                    if index >= 0 && index <= Array.length ( mres ) - 1
                                                                                    then
                                                                                      res := Row ( mres.(index) )
                                                                                    else
                                                                                      raise Invalid_Row_Index
                                                                                  end
                                                           | _ -> print_warning "RowSelect: wrong type!!!"
                                                       end;
                                                       command tl !res

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


                                                                     command tl links



                                                           | _ -> print_warning "Link_extract found non-usable type"; raise Wrong_tmpvar_type
                                                       end


                       | Link_extract_xml           ->
                                                       begin
                                                         match tmpvar with
                                                           | Document(doc, url)-> let urls   = Array.of_list (Parsers.xml_get_href_from_string doc) in
                                                                                  (* the url of the doecument will become the referrer of the extracted url! *)
                                                                                  let links  = Url_array (Array.map ( fun lnk -> (lnk, url) ) urls) in
                                                                                  command tl links
                                                           | _ -> print_warning "Link_extract found non-usable type"; raise Wrong_tmpvar_type
                                                       end

                       | Print                      ->
                                                       begin
                                                         match tmpvar with
                                                           | Document(doc, url)-> print_endline doc  (* only print the document, without referrer *)
                                                           | Match_result mres -> Array.iter ( fun x -> Array.iter ( fun y -> Printf.printf "\"%s\" ||| " y) x;
                                                                                                        print_newline() ) mres
                                                           | Row              str_arr -> Array.iter print_endline str_arr
                                                           | Col              str_arr -> Array.iter ( fun str -> Printf.printf "\"%s\" \n " str) str_arr
                                                           | Url (href, ref)   -> Printf.printf "%s   # Referrer:  %s\n" href ref
                                                           | Url_list  liste    -> List.iter  ( fun (href, ref) -> Printf.printf "%s  # Referrer:  %s\n" href ref) liste
                                                           | Url_array liste    -> Array.iter ( fun (href, ref) -> Printf.printf "%s  # Referrer:  %s\n" href ref) liste
                                                           (*
                                                           | Result_selection str_arr -> Array.iter ( fun str -> print_endline str; print_newline()) str_arr
                                                           *)
                                                           | _ -> print_warning "Print-command found non-printable type"
                                                       end;
                                                       command tl tmpvar


                       | Print_match                -> (* prints "real" matches only (and not the fullmatch with index = 0) *)
                                                       begin
                                                         match tmpvar with
                                                           | Match_result mres -> let lines = Array.to_list mres in
                                                                      print_endline "print_match: match 0 is the whole match, all others are the groups\n";
                                                                      Array.iter ( fun x -> let matched_groups = List.tl (Array.to_list x) in
                                                                                            for index = 0 to Array.length x -1
                                                                                            do
                                                                                              Printf.printf "%2d: \"%s\" \n" index x.(index)
                                                                                            done;
                                                                                            print_newline()
                                                                                 ) mres
                                                           | _ -> raise Wrong_argument_type
                                                       end;
                                                       command tl tmpvar


                       | Print_string str           -> print_endline str;
                                                       command tl tmpvar


                       | Save   _                   -> print_endline "Save detected"; raise NOT_IMPLEMENTED_SO_FAR;
                                                       command tl tmpvar


                       | Dummy                      -> command tl tmpvar (* does nothing; just a Dummy (NOP) *)

                       | Setvar var                 -> command tl var (* sets the argument of setvar as new tmpvar *)

                       | Showtype                   -> begin
                                                         match tmpvar with
                                                           | Document          _ -> print_endline "TMPVAR contains a document"
                                                           | Url               _ -> print_endline "TMPVAR contains a Url"
                                                           | Url_list          _ -> print_endline "TMPVAR contains an Url_list"
                                                           | Dummy_result        -> print_endline "TMPVAR contains Dummy_result"
                                                           | Match_result      _ -> print_endline "TMPVAR contains Match_result"
                                                           | Row               _ -> print_endline "TMPVAR contains Row"
                                                           | Col               _ -> print_endline "TMPVAR contains Col"
                                                           (*
                                                           | Result_selection  _ -> print_endline "TMPVAR contains Match_result"
                                                           *)
                                                           | Empty               -> print_endline "TMPVAR contains EMPTY"
                                                           | _                   -> print_endline "OOOOPS Unknown Type!"; raise Not_found
                                                       end;
                                                       command tl tmpvar
                   end


  in
    command cmdlst Empty







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


exception Mainurl_error
exception Asx_error
exception Stream_error




(* for each URL do try to get the stuff and parse it and evaluate *)
(* -------------------------------------------------------------- *)
let do_new_any_dl parserhash url_list =
  List.iter ( fun url ->
                          let baseurl = Parsers.url_get_baseurl url in
                          try

                            let parserdef = Hashtbl.find parserhash baseurl in (* lookup the parser for the url *)
                            print_endline "# --------------------";

                            (* we evaluate the parse-tree, and start with a first, implicit get *)
                            (* ---------------------------------------------------------------- *)
                            evaluate_command_list ( Get_url(url, "-") :: parserdef.commands)

                          with (* handle exceptions from the parse-tree-evaluation *)
                            | Not_found         -> prerr_endline ("No parser found for " ^ url)
                            | Invalid_Row_Index -> prerr_endline "Error in script! Invalid_Row_Index!!\n"
            ) url_list




let scriptname = Filename.concat (Sys.getenv "HOME") (".any-dl.rc")

let read_parser_definitions filename_opt =
  Printf.printf "Scriptname: %s" scriptname;
    let tokenlist = ref [] in

    let input_channel = match filename_opt with None -> stdin | Some filename -> open_in filename in
    let lexer = Lexing.from_channel input_channel in
    begin
      try
        while true do
          let result = Scriptparser.main Scriptlexer.read_command lexer in
          tokenlist := result :: !tokenlist
        done
      with End_of_file -> prerr_endline "Ende der Eingabe erreicht; Parser-Definitionen gelesen!"

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
                  (*
                  *)
                  prerr_newline();
                  exit 1

    end
    ;
    close_in input_channel;
    List.rev !tokenlist





let example_url =  "http://www.ardmediathek.de/das-erste/polizeiruf-110/eine-andere-welt-fsk-tgl-ab-20-uhr?documentId=12883434"

let _  =
    let tokenlist = read_parser_definitions (Some scriptname)
    in Printf.printf "Number of found parser definitions: %d\n" (List.length tokenlist);


    (* craeate and initialize the Parserdefinition-hash *)
    (* ------------------------------------------------ *)
    (* For looking up parserdefs by URL                 *)
    (* ------------------------------------------------ *)
    let parserhash = Hashtbl.create (List.length tokenlist) in
    List.iter ( fun parserdef ->
                                 List.iter ( fun url -> Hashtbl.add parserhash url parserdef;
                                                        Printf.printf "Init: bound Base-URL %-30s -> parser %s\n" url parserdef.parsername
                                           ) parserdef.urllist;
              ) tokenlist;


    flush stdout; (* all init-stuff should be flushed, before evaluation stage is entered! *)

    do_new_any_dl  parserhash  (List.tl (Array.to_list Sys.argv) )




(* --------------------------------------------------------------------------------------------------------------

  HOW TO DUMP STREAMS:
 ======================

rtmp / rtmpt:
  rtmpdump --resume  -r rtmp://.... -y mp4:....  -o outfile.ext

mms:
  mplayer -dumpstream mms://example.com/Globalplayers/GP_14.wmv -dumpfile ./download/test.wmv 

 ------------------------------------------------------------------------------------------------------------- *)
