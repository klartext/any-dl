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


exception NOT_IMPLEMENTED_SO_FAR (* for planned, but not already implemented functionality *)
exception Command_Sequence_error of string (* for sequences that are not allowed *)

exception No_document_found         (* a dcoument could not be retrieved *)
exception No_Match                  (* if a match was tried, but no match could be found *)
exception No_Matchresult_available  (* if Select is used, but there is no match-result available as tmpvar *)
exception No_Matchable_value_available  (* if Match is used, but there is no matchabe tmpvar *)


exception Wrong_tmpvar_type             (* if tmpvar has just the wrong type... without more detailed info *)
exception Wrong_argument_type           (* e.g. Show_match on non-match *)

exception Invalid_Row_Index             (* indexing a row that does not exist *)
exception Invalid_Col_Index             (* indexing a col that does not exist *)

exception No_parser_found_for_this_url (* *)

exception No_String_representation     (* To_string called on a value that has no way conversion so far *)

exception Variable_not_found of string   (* a variable-name lookup in the Varname-map failed *)

exception Devel (* exception while developing / testing *)


module Array2 =
  struct
    include Array

    let filter filt arr = Array.of_list ( List.filter filt (Array.to_list arr ))
  end


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

    let empty  = Varmap.empty
    let add    = Varmap.add
    let remove = Varmap.remove
    let iter   = Varmap.iter
    let exists = Varmap.mem

    let find varname varmap =
      try Varmap.find varname varmap with Not_found -> raise (Variable_not_found varname)

  end




(* ---------------------------------------------- *)
(* functional, not thorough nifty-details printer *)
(* intended to make basic functionality working   *)
(* more fancy converters for other purposes might *)
(* elsewehere be defined                          *)
(* ---------------------------------------------- *)
let rec  to_string  result_value varmap =
  let str =
    match result_value with
      | Varname       varname      -> let res = (Varmap.find varname varmap) in
                                      begin
                                        match res with
                                          | String str -> str
                                          | _ as again -> to_string again varmap
                                      end
      | String        str          -> str 
      | Document      (doc, url)   -> doc ^ url
      | String_array  str_arr      -> Array.fold_left ( ^ ) "" str_arr
      | Match_result  mres         -> raise Wrong_argument_type (* match-res => arr of arr -> recursion on String_array ! *)
      | Url           (href, ref)  -> href
      | Url_list      url_list     -> List.fold_right ( fun a sofar -> "\"" ^ (fst a) ^ "\" " ^ sofar ) url_list ""
      | Url_array     url_arr      -> let elem = url_arr.(0) in to_string (Url (fst(elem), snd(elem))) varmap (* first Url used *)
                                      (*
                                      Array.iter ( fun (href, ref) -> Printf.printf "%s  # Referrer:  %s\n" href ref) liste
                                      *)
                                      (* concat all urls, but all href's are quoted inside '"' *)

      (*
      | Url_list  liste    -> List.iter  ( fun (href, ref) -> Printf.printf "%s  # Referrer:  %s\n" href ref) liste
      | Result_selection str_arr -> Array.iter ( fun str -> print_endline str; print_newline()) str_arr
      | Match_result mres -> Array.iter ( fun x -> Array.iter ( fun y -> Printf.printf "\"%s\" ||| " y) x;
      *)
      | _ -> print_warning "to_string-function found non-convertable type"; raise Wrong_argument_type

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
                       | Get_url (url, referrer)  ->
                                                     let send_cookie = if Varmap.exists "COOKIES.SEND" varmap
                                                                       then
                                                                         Some ( to_string(Varmap.find "COOKIES.SEND" varmap) varmap )
                                                                       else
                                                                         None
                                                     in
                                                     let document_and_cookies = Network.Curly.get url (Some referrer) send_cookie in
                                                     begin
                                                       match document_and_cookies with
                                                         | Some (doc, cookies) -> let cook = String_array (Array.of_list cookies) in
                                                                                  let new_varmap = (Varmap.add "COOKIES.RECEIVED" cook varmap) in
                                                                                  command tl (Document (doc, url)) new_varmap (* $URL *)

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



                       | Match   pattern            ->
                                                       if
                                                        Cli.opt.Cli.verbose
                                                       then
                                                         Printf.fprintf stderr "MATCH-PATTERN: \"%s\"\n" pattern; (* devel-debug-info *)

                                                       let str =
                                                         begin
                                                           match tmpvar with
                                                             | Document (doc, url) -> doc
                                                             (* match also on other types?? Does matching an URL for example makes sense? *)
                                                             | _            -> raise No_Matchable_value_available (* this is a type-error Wrong_tmpvar_type *)
                                                         end
                                                       in
                                                       let match_res = Parsers.if_match_give_group_of_groups str (Pcre.regexp pattern (* flags here *)) in
                                                       let matched =
                                                         begin
                                                           match match_res with
                                                             | None   -> raise No_Match
                                                             | Some res -> res
                                                         end
                                                       in
                                                       command tl (Match_result matched) varmap


                       | Grep pattern               -> 
                                                       (*
                                                         if Pcre.pmatch ~pat:".any-dl.rc: No such file or directory" msg
                                                       *)
                                                       let grepped = 
                                                         begin
                                                           match tmpvar with
                                                             | String_array str_arr -> String_array( Array2.filter ( fun elem -> Pcre.pmatch ~pat:pattern elem ) str_arr)
                                                             | Url_array    url_arr -> Url_array (Array2.filter ( fun (url,ref) -> Pcre.pmatch ~pat:pattern url ||
                                                                                                                        Pcre.pmatch ~pat:pattern ref ) url_arr )
                                                             | _            -> prerr_endline "Select: nothing to match"; raise No_Matchresult_available
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
                                                           | _            -> prerr_endline "Select: nothing to match"; raise No_Matchresult_available
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
                                                           | _ -> print_warning "RowSelect: wrong type!!!"; raise Wrong_tmpvar_type
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

                       | Select_match ( col_idx, matchpat) -> (* select match is a row-select, where the index *)
                                                              (* first match wins *)
                                                               begin
                                                                 match tmpvar with
                                                                   | Match_result mres ->
                                                                          let max_row_idx = Array.length ( mres ) - 1 in
                                                                          let max_col_idx = Array.length ( mres.(0) ) - 1 in

                                                                          let rows     = Array.to_list mres in

                                                                          (* here is the selection: via string match of the lookup-pattern *)
                                                                          let selected = List.filter ( fun item -> Pcre.pmatch ~pat:matchpat item.(col_idx)  ) rows in
                                                                          if List.length selected = 0 then raise No_Match;

                                                                          if Cli.opt.Cli.verbose = true
                                                                          then Printf.printf "found: %d items \n" (List.length selected);
                                                                          command tl (String_array (List.hd selected)) varmap

                                                                   | _ -> print_warning "RowSelect: wrong type!!!"; raise Wrong_tmpvar_type
                                                               end


                       | I_Select_match ( col_idx, matchpat) -> (* select match is a row-select, where the index *)
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
                                                                                interactive_string_select col matchpat
                                                                              else
                                                                                matchpat
                                                                            in
                                                                              if Cli.opt.Cli.verbose = true then Printf.printf "selected pattern: \"%s\"\n" match_pattern;

                                                                              let selected = List.filter ( fun item -> Pcre.pmatch ~pat:match_pattern item.(col_idx)  ) rows in
                                                                              if List.length selected = 0 then raise No_Match;

                                                                              if Cli.opt.Cli.verbose = true then Printf.printf "found: %d items \n" (List.length selected);

                                                                              command tl (String_array (List.hd selected)) varmap
  
                                                                     | _ -> print_warning "RowSelect: wrong type!!!"; raise Wrong_tmpvar_type
                                                                 end



                       | Link_extract               ->
                                                       begin
                                                         match tmpvar with
                                                           | Document (doc, url) ->
                                                                     let urls   = Parsers.linkextract doc in

                                                                     let rebased_urls =
                                                                         List.fold_right ( fun lnk sofar -> match Parsers.Rebase.rebase_url url lnk with
                                                                                                              | Some rebased -> (rebased, url) :: sofar
                                                                                                              | None         -> sofar
                                                                                         ) urls []
                                                                     in

                                                                     let links  = Url_array ( Array.of_list rebased_urls )
                                                                     in
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

                         (*
                         | Tag_extract  tagname ...   ->
                                                         Printf.eprintf "Tag_extract  tagname: %s\n" tagname;
                                                         begin
                                                           match tmpvar with
                                                             | Document (doc, url) ->
                                                                       let result = Array.of_list (Parsers.tagextract tagname doc) in
                                                                       command tl (String_array result) varmap
                                                             | _ -> print_warning "Tag_extract found non-usable type"; raise Wrong_tmpvar_type
                                                         end
                         *)

                         | Title_extract            ->
                                                       begin
                                                         match tmpvar with
                                                           | Document (doc, url) ->
                                                                     let result = Array.of_list (Parsers.titleextract doc) in
                                                                     command (Subst ("\n", "") :: tl) (String_array result) varmap
                                                           | _ -> print_warning "Tag_extract found non-usable type"; raise Wrong_tmpvar_type
                                                       end


                       | Paste paste_list            ->
                                                        let str_lst = List.map (fun item ->  to_string item varmap) paste_list in (* convert to string  *)
                                                        let res     = List.fold_left ( ^ ) "" str_lst in                          (* append all strings *)
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
                                                           | Match_result mres -> Array.iter ( fun x -> Array.iter ( fun y -> Printf.printf "\"%s\" ||| " y) x;
                                                                                                        print_newline() ) mres
                                                           | String_array     str_arr -> Array.iter ( fun str -> Printf.printf "\"%s\" \n" str) str_arr
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

                       | Delete varname             -> command tl tmpvar (Varmap.remove varname varmap)  (* removes variable varname *)


                       | Show_variables             -> Varmap.iter ( fun varname value -> Printf.printf "***** \"%s\": " varname;
                                                                                          command [Print; Print_string "\n"] value varmap ) varmap;
                                                       command tl tmpvar varmap

                       | Show_type                   -> Printf.printf "TMPVAR (1-val-stack) contains: %s\n" (Parsetreetypes.result_to_string tmpvar);
                                                       command tl tmpvar varmap


                       | Basename                   -> begin
                                                         match tmpvar with
                                                           | String filename -> command tl (String(Filename.basename filename)) varmap
                                                           | Url (href, ref) -> command tl (String(Filename.basename href)) varmap
                                                           | _ -> raise Wrong_argument_type
                                                       end


                       | Subst (from_re, to_str)    -> 
                                                       if Cli.opt.Cli.verbose then Printf.fprintf stderr "Subst: \"%s\" -> \"%s\"\n" from_re to_str;
                                                       let replacer instring = Pcre.replace ~pat:from_re ~templ:to_str instring in
                                                       begin
                                                       match tmpvar with
                                                         | String str           -> command tl (String (replacer str)) varmap
                                                         | String_array str_arr -> let replaced = Array.map replacer str_arr in
                                                                                   command tl (String_array replaced) varmap
                                                         | _ -> raise Wrong_argument_type
                                                       end


                       | Quote                      -> let str = to_string tmpvar varmap in
                                                       let quoted = "\"" ^ str ^ "\"" in
                                                       command tl (String (quoted)) varmap

                       | To_string                  -> command tl (String (to_string tmpvar varmap)) varmap

                       | Dump                       ->
                                                       begin
                                                       match tmpvar with
                                                         | Document(doc, url)-> Parsers.Htmlparse.dump_html doc
                                                         | _ -> raise Wrong_argument_type
                                                       end;
                                                       command tl tmpvar varmap

                       | Dump_data                  ->
                                                       begin
                                                       match tmpvar with
                                                         | Document(doc, url)-> Parsers.Htmlparse.dump_html_data doc
                                                         | _ -> raise Wrong_argument_type
                                                       end;
                                                       command tl tmpvar varmap

                       | System                     -> begin
                                                         match tmpvar with
                                                           | String syscmd -> if Cli.opt.Cli.safe = false
                                                                              then
                                                                                Sys.command syscmd
                                                                              else
                                                                                (Printf.fprintf stderr "*** Command not invoked: %s\n" syscmd; 0)
                                                           | _ -> raise Wrong_argument_type
                                                       end;
                                                       command tl tmpvar varmap

                       | Exit_parse                 -> flush stdout; prerr_endline "Parse was exited."; command [] tmpvar varmap (* call again with nothing-left-to-do *)

                       | Dummy                      -> command tl tmpvar varmap (* does nothing; just a Dummy (NOP) *)

                   end


  in
    command cmdlst Empty Varmap.empty








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



(* lookup parser-name via url *)
(* -------------------------- *)
let parsername_lookup_by_url url lookup_lst =
  let rec aux liste = match liste with
    | []       -> raise Not_found
    | hd :: tl -> let parser_url  = fst hd in
                  let parser_name = snd hd in

                  if Cli.opt.Cli.verbose then
                    Printf.printf "parser-lookup via url: %s\n\t%s  ->  %s\n--\n" url parser_url parser_name;

                  let parser_url_len = String.length parser_url in
                  try
                    if parser_url_len > 0 && parser_url = String.sub url 0 parser_url_len then parser_name else aux tl
                  with Invalid_argument("String.sub") -> aux tl (* this happens if url is shorter than parser_url *)
  in
    aux lookup_lst



let main()  =
    Cli.parse(); (* parse the command line *)

    (* parse the parser-definitions *)
    (* ---------------------------- *)
    let parserlist = read_parser_definitions (Some Cli.opt.Cli.rc_filename) in

    (* if cli-switches ask for it, print number of parser-definitions *)
    if Cli.opt.Cli.list_parsers || Cli.opt.Cli.verbose then
      Printf.fprintf stderr "Number of found parser definitions: %d\n" (List.length parserlist);


    (* if cli-switches ask for it, print number of all commands of the parser-definitions *)
    (* They wll be printed in alphabetical order.                                         *)
    (* ---------------------------------------------------------------------------------- *)
    if Cli.opt.Cli.show_commands || Cli.opt.Cli.verbose then
      begin
        print_endline "Keywords of the parser-definition language:";
        print_endline "-------------------------------------------";
        let kwlist = Hashtbl.fold (fun key value sofar -> key :: sofar ) Scriptlexer.keyword_table [] in
        List.iter ( fun kw -> Printf.printf "keyword   %s\n" kw) (List.sort compare kwlist)
      end;


    (* create and initialize hashes for parser-lookup by name / url *)
    (* ------------------------------------------------------------ *)
    let parser_namehash = Hashtbl.create (List.length parserlist) in
    let parser_urllist_raw  = ref [] in
    List.iter ( fun parserdef ->
                                 (* add the parsers to the parser_name-hash (for parser-lookup by name) *)
                                 Hashtbl.add parser_namehash parserdef.parsername parserdef;

                                 (* add the parsers to the parser_url-list (for parser-lookup by url) *)
                                 List.iter ( fun url -> 
                                                        parser_urllist_raw := (url, parserdef.parsername) :: !parser_urllist_raw;

                                                        if Cli.opt.Cli.list_parsers || Cli.opt.Cli.verbose
                                                        then
                                                          Printf.fprintf stderr "Init: bound Base-URL %-30s -> parser %s\n" url parserdef.parsername

                                           ) parserdef.urllist;

              ) parserlist;

    flush stdout; (* all init-stuff should be flushed, before evaluation stage is entered! *)


    (* longest url-entry will become first entry *)
    (* ----------------------------------------- *)
    (*
      The url -> parsername list is sorted by the length of the lookup-url,
      because a longer URL means a more specific url, hence a less-specific
      url would be a kind of fall-back to a non-match.
      So, the first url-match will be the most-specific url,
      and hence giving the name of the most-specific parser.
    *)
    let parser_urllist = List.sort ( fun elem1 elem2 -> String.length (fst elem2) - String.length (fst elem1) ) !parser_urllist_raw in


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

                                      | None            -> (* parsername looked up via from url *)

                                                           (* comparing the url with the strings in the url-parsername-assoc-list *)
                                                           (* ------------------------------------------------------------------- *)
                                                           let parsername = parsername_lookup_by_url  url  parser_urllist in
                                                           Hashtbl.find parser_namehash  parsername

                                   end
                                with Not_found         -> prerr_endline ("No parser found for " ^ url); raise No_parser_found_for_this_url
                            in

                            try
                              print_endline "# --------------------";

                              (* ---------------------------------------------------------------- *)
                              (* we evaluate the parse-tree, and start with a first, implicit get *)
                              (* with the url we got from the command line                        *)
                              (* ---------------------------------------------------------------- *)
                              evaluate_command_list (Setvar(Url(url,"-")) :: Store "STARTURL" :: Get_url(url, "-") :: Store("BASEDOC") :: parserdef.commands)


                            with (* handle exceptions from the parse-tree-evaluation *)
                              | No_Match                -> prerr_endline "Parser problem: Could not match!\t Parse will be exited\n"
                              | Invalid_Row_Index       -> prerr_endline "Error in script! Invalid_Row_Index!\t Parse exited.\n"
                              | Variable_not_found name -> Printf.eprintf "Variable_not_found: \"%s\"\t This parse exited.\n" name
                              | No_document_found       -> Printf.eprintf "No_document_found for URL %s\n" url



              ) (List.rev Cli.opt.Cli.url_list)


let _ =
  try
    main()
  with Sys_error msg -> if Pcre.pmatch ~pat:".any-dl.rc: No such file or directory" msg
                        then
                          begin
                            Printf.fprintf stderr "The config file is missing. Default place for it is $HOME/.any-dl.rc.";
                            Printf.fprintf stderr " Please provide it there or use -f option\n"
                          end
                        else
                          raise ( Sys_error msg )


(* --------------------------------------------------------------------------------------------------------------

  HOW TO DUMP STREAMS:
 ======================

rtmp / rtmpt:
  rtmpdump --resume  -r rtmp://.... -y mp4:....  -o outfile.ext

mms:
  mplayer -dumpstream mms://example.com/Globalplayers/GP_14.wmv -dumpfile ./download/test.wmv 

 ------------------------------------------------------------------------------------------------------------- *)
