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
(* ??
exception Command_Sequence_error of string (* for sequences that are not allowed *)
*)

exception Value_conversion_unknown  (* type conversion, that can't handle this special item (similar to "Wrong_tmpvar_type") *)

exception No_document_found         (* a dcoument could not be retrieved *)
exception No_Match                  (* if a match was tried, but no match could be found *)
exception No_Matchresult_available  (* if Select is used, but there is no match-result available as tmpvar *)
exception No_Matchable_value_available  (* if Match is used, but there is no matchabe tmpvar *)


exception Wrong_tmpvar_type             (* if tmpvar has just the wrong type... without more detailed info *)
exception Wrong_argument_type           (* e.g. Show_match on non-match *)

exception Invalid_Row_Index             (* indexing a row that does not exist *)
exception Invalid_Col_Index             (* indexing a col that does not exist *)
exception Invalid_Index                 (* indexing a col/row that does not exist *)

exception No_parser_found_for_this_url (* *)

exception AutoTry_success              (* in auto-try mode (switch -a), if successful, this exception will be thrown *)

(* ???
exception No_String_representation     (* To_string called on a value that has no way conversion so far *)
*)

exception Variable_not_found of string   (* a variable-name lookup in the Varname-map failed *)

exception Devel (* exception while developing / testing *)



(* CLI-VERBOSE-dependent print functions ! *)
(* --------------------------------------- *)
let verbose_fprintf channel formatstr =
  let frmt = format_of_string formatstr in
  if Cli.opt.Cli.verbose then Printf.fprintf channel frmt else Printf.ifprintf channel frmt

let verbose_printf  formatstr = verbose_fprintf stdout formatstr
(* -v means: verbose is wanted output, not an error... why then stderr? -> unneeded?
let verbose_eprintf formatstr = verbose_fprintf stderr formatstr
*)


(* save string to file *)
(* ------------------- *)
let save_string_to_file str filename =
  let oc = open_out filename in
  output_string oc str;
  close_out oc


(* -------------------------------------------------------------- *)
(* HTML-decode                                                    *)
(* -------------------------------------------------------------- *)
(* it's hard-coded, based on the assumption, that UTF-8 is usual. *)
(* might be parameterized if necessary.                           *)
(* -------------------------------------------------------------- *)
let html_decode str = Netencoding.Html.decode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 () str





module Array2 =
  struct
    include Array

    let filter filt arr = Array.of_list ( List.filter filt (Array.to_list arr ))

    let exists filt arr = List.exists filt (Array.to_list arr) 


    let filter_row_by_colmatch colmatcher matr =
      filter ( fun arr -> exists colmatcher arr ) matr

  end



module Sleep =
  struct
    open Unix

    (* sleep a certain amount of time (in seconds as float *)
    (* --------------------------------------------------- *)
    let sleep_float  float_seconds =
      ignore( select [] [] [ stdin ] (abs_float float_seconds) )

    (* sleep ms milliseconds *)
    (* --------------------- *)
    let sleep_ms  ms =
      verbose_printf "sleep %d miliseconds\n" ms; (* devel-debug-info *)
      sleep_float (float_of_int ms /. 1000.0)

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


(* =================================================== *)
(* from an array drop the element with the given index *)
(* =================================================== *)
let array_drop arr dropidx =
  let len = Array.length arr             in

  (* Argument checking *)
  (* ----------------- *)
  if dropidx < 0 || dropidx > len - 1 then raise Invalid_Index;


  let res = Array.make (len - 1) arr.(0) in

  let srcidx    = ref 0 in
  let targetidx = ref 0 in

  (* --------------------------------------------------------------------------------- *)
  (* copy any element from src to target, that has different index than the drop-index *)
  (* --------------------------------------------------------------------------------- *)
  while !srcidx < len
  do
    if !srcidx != dropidx
    then
      begin
        res.(!targetidx) <- arr.(!srcidx); (* copy data *)
        incr srcidx;
        incr targetidx
      end
    else
      begin
        incr srcidx;
      end
  done;
  res (* the resulting array *)



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
      | Document      (doc, url)   -> url ^ ":" ^ doc
      | Document_array  arr        -> let strarr = Array.map ( fun (d,u) -> to_string (Document (d,u)) varmap ) arr in to_string (String_array strarr) varmap
      | String_array  str_arr      -> Array.fold_left ( ^ ) "" str_arr
      | Match_result  mres         -> raise Wrong_argument_type (* match-res => arr of arr -> recursion on String_array ! *)
      | Url           (href, ref)  -> href
      | Url_list      url_list     -> List.fold_right ( fun a sofar -> "\"" ^ (fst a) ^ "\" " ^ sofar ) url_list ""
      | Url_array     url_arr      -> let elem = url_arr.(0) in to_string (Url (fst(elem), snd(elem))) varmap (* first Url used *)
      | Empty                      -> ""
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
  let str =
    match result_value with
      | Varname       varname      -> let res = (Varmap.find varname varmap) in
                                      urlify res varmap
      | String        str          -> Url(str, "-")
      | Document      (doc, url)   -> raise Value_conversion_unknown (* like Wrong_argument_type *)
      | Document_array  arr        -> raise Value_conversion_unknown
                      (*
                      let strarr = Array.map ( fun (d,u) -> to_string (Document (d,u)) varmap ) arr in to_string (String_array strarr) varmap
                      *)
      | String_array  str_arr      -> Url_array( Array.map (fun str -> (str, "-") ) str_arr )
      | Match_result  mres         -> let liste = ref [] in
                                      Array.iter( fun x -> Array.iter ( fun elem -> liste := (elem, "-") :: !liste ) x ) mres; (* extract elements to liste *)
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



(* ------------------------------------------------- *)
(* This function evaluates the list of commands that *)
(* a parser consists of.                             *)
(* this function is doing the main work of any-dl.   *)
(* ------------------------------------------------- *)
let evaluate_command_list cmdlst =

  (* "get_document"-function, is used by some of the Get_... commands from "command"-function *)
  (* ---------------------------------------------------------------------------------------- *)
  let rec get_document  url referrer varmap =
    let send_cookie = if Varmap.exists "COOKIES.SEND" varmap
                     then
                       Some ( to_string(Varmap.find "COOKIES.SEND" varmap) varmap )
                     else
                       None
    in
    let document_and_cookies = Network.Curly.get url (Some referrer) send_cookie in
    begin
     match document_and_cookies with
       | None                -> None
       | Some (doc, cookies) -> let cook = String_array (Array.of_list cookies) in
                                let new_varmap = (Varmap.add "COOKIES.RECEIVED" cook varmap) in (* what, if COOKIES.RECEIVED is set and needs to be ADDED? *)
                                Some (doc, url, new_varmap)
    end
    

  (* -------------------------------------------------------------------------------------- *)
  (* get_document_list: gets a list of documents (bulk-get) *)
  (* if the sleep-time is set o values > 0, then each get waits this amoubt of milliseconds *)
  (* before the get is executed.                                                            *)
  (* -------------------------------------------------------------------------------------- *)
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
  (* -------------------------------------------------------- *)
  and     command commandlist tmpvar varmap =
    flush_all();
    match commandlist with
      | []        -> () (* Printf.printf "<========================== BACK. Leave evaluate_command_list() now!\n"*)
      | cmd::tl   -> begin
                       match cmd with
                         | Get_url (url, referrer)  ->
                                                       begin
                                                       match get_document  url referrer varmap with
                                                         | Some ( doc, url, new_varmap ) -> command tl (Document (doc, url)) new_varmap (* $URL *)
                                                         | None                          -> raise No_document_found
                                                       end


                         | Get             ->
                                              begin
                                                match tmpvar with
                                                  | Url (u,r)          -> command (Get_url (u,r) :: tl) tmpvar varmap
                                                  | Url_list  urllist  -> command (Get_urls :: tl) tmpvar varmap
                                                  | Url_array urlarray -> command (Get_urls :: tl) tmpvar varmap
                                                  (* MATCHRES ???
                                                  *)
                                                  | _ -> raise Wrong_tmpvar_type
                                                end


                         | Get_urls        -> begin
                                                match tmpvar with
                                                  | Url_list  urllist  -> prerr_endline "Should now get Documents!";
                                                                          let docs, vm = get_document_list  urllist varmap in
                                                                          command tl (Document_array (Array.of_list docs)) vm

                                                  | Url_array urlarray -> prerr_endline "Should now get Documents!";
                                                                          let docs, vm = get_document_list  (Array.to_list urlarray) varmap in
                                                                          command tl (Document_array (Array.of_list docs)) vm
                                                  (*
                                                  | Url_array urlarray -> prerr_endline "Should now get Documents!";
                                                                          Array.iter ( fun (u,r) -> Printf.printf "url: %s /// referrer: %s\n" u r) urlarray
                                                  *)
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
                                                               | Match_result mres -> Match_result ( Array2.filter_row_by_colmatch ( fun x -> Pcre.pmatch ~pat:pattern x ) mres )

                                                               | _            -> prerr_endline "Select: nothing to match"; raise No_Matchresult_available
                                                           end
                                                         in
                                                           command tl grepped varmap

                         | Grep_v pattern             ->  (* grep -v *)
                                                         (*
                                                           if Pcre.pmatch ~pat:".any-dl.rc: No such file or directory" msg
                                                         *)
                                                         let grepped = 
                                                           begin
                                                             match tmpvar with
                                                               | String_array str_arr -> String_array( Array2.filter ( fun elem -> not (Pcre.pmatch ~pat:pattern elem) ) str_arr)
                                                               | Url_array    url_arr -> Url_array (Array2.filter ( fun (url,ref) -> not (Pcre.pmatch ~pat:pattern url ||
                                                                                                                          Pcre.pmatch ~pat:pattern ref) ) url_arr )

                                                               | Match_result mres -> Match_result ( Array2.filter_row_by_colmatch
                                                                                                         ( fun x -> not (Pcre.pmatch ~pat:pattern x )) mres )
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

                                                                            let max_row_idx = Array.length ( mres ) - 1 in
                                                                            let max_col_idx = Array.length ( mres.(0) ) - 1 in

                                                                            let rows     = Array.to_list mres in

                                                                            (* here is the selection: via string match of the lookup-pattern *)
                                                                            let selected = List.filter ( fun item -> Pcre.pmatch ~pat:matchpat item.(col_idx)  ) rows in
                                                                            if List.length selected = 0 then raise No_Match;

                                                                            verbose_printf "found: %d items \n" (List.length selected);
                                                                            command tl (String_array (List.hd selected)) varmap (* first match wins *)

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
                                                                                verbose_printf "selected pattern: \"%s\"\n" match_pattern;

                                                                                let selected = List.filter ( fun item -> Pcre.pmatch ~pat:match_pattern item.(col_idx)  ) rows in
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
                                                               let extracted_urls = Parsers.linkextract document in
                                                               if Cli.opt.Cli.verbose then List.iter (fun x -> verbose_fprintf stdout "---extracted url: %s\n" x) extracted_urls;
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
                                                                        print_endline "show_match: match 0 is the whole match, all others are the groups\n";
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


                         | Save                       -> (*print_endline "Save detected"; raise NOT_IMPLEMENTED_SO_FAR*)
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


                         | Setvar var                 -> command tl var varmap (* sets the argument of setvar as new tmpvar *)



                         | Store  varname             -> command tl tmpvar (Varmap.add varname tmpvar varmap)  (* stores tmpvar as named variable *)


                         | Recall varname             -> verbose_printf "Recall variable: \"%s\"\n" varname;
                                                         let varcontents = Varmap.find varname varmap in
                                                         command tl varcontents varmap

                         | Delete varname             -> command tl tmpvar (Varmap.remove varname varmap)  (* removes variable varname *)


                         | Show_variables             -> Varmap.iter ( fun varname value -> Printf.printf "***** \"%s\": " varname;
                                                                                            command [Print; Print_string "\n"] value varmap ) varmap;
                                                         command tl tmpvar varmap

                         | List_variables             -> Varmap.iter ( fun varname value -> Printf.printf "***** \"%s\"\n" varname ) varmap;
                                                         command tl tmpvar varmap

                         | Show_type                   -> Printf.printf "TMPVAR (1-val-stack) contains: %s\n" (Parsetreetypes.result_to_string tmpvar);
                                                         command tl tmpvar varmap


                         | Basename                   -> begin
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
                                                           | Document(doc, ref) -> let newdoc = Document( replacer doc, replacer ref ) in
                                                                                   command tl (newdoc) varmap
                                                           | Document_array doc_arr ->
                                                                      let new_docarr = Array.map ( fun (doc,ref) -> (replacer doc, replacer ref) ) doc_arr in
                                                                      command tl (Document_array new_docarr) varmap
                                                           | _ -> raise Wrong_argument_type
                                                         end


                         | Quote                      -> let str = to_string tmpvar varmap in
                                                         let quoted = "\"" ^ str ^ "\"" in
                                                         command tl (String (quoted)) varmap

                         | To_string                  -> command tl (String (to_string tmpvar varmap)) varmap

                         | Dump                       ->
                                                         begin
                                                         match tmpvar with
                                                           | Document(doc, url)-> Parsers.Htmlparse.dump_html_from_string doc
                                                           | _ -> raise Wrong_argument_type
                                                         end;
                                                         command tl tmpvar varmap

                         | Show_tags                  ->
                                                         begin
                                                         match tmpvar with
                                                           | Document(doc, url)-> Parsers.Htmlparse.show_tags_from_string doc
                                                           | _ -> raise Wrong_argument_type
                                                         end;
                                                         command tl tmpvar varmap

                         | Show_tags_fullpath         ->
                                                         begin
                                                         match tmpvar with
                                                           | Document(doc, url)-> Parsers.Htmlparse.show_tags_fullpath_from_string doc
                                                           | _ -> raise Wrong_argument_type
                                                         end;
                                                         command tl tmpvar varmap

                         | Dump_data                  ->
                                                         begin
                                                         match tmpvar with
                                                           | Document(doc, url)-> Parsers.Htmlparse.dump_html_data_from_string doc
                                                           | _ -> raise Wrong_argument_type
                                                         end;
                                                         command tl tmpvar varmap

                         | System                     -> begin
                                                           match tmpvar with
                                                             | String syscmd -> (* verbosity-message *)
                                                                                (* ----------------- *)
                                                                                verbose_printf "System-cmd: %s" syscmd;

                                                                                (* do the work *)
                                                                                (* ----------- *)
                                                                                if Cli.opt.Cli.safe = false
                                                                                then
                                                                                  Sys.command syscmd
                                                                                else
                                                                                  (Printf.fprintf stderr "*** Command not invoked: %s\n" syscmd; 0)
                                                             | _ -> raise Wrong_argument_type
                                                         end;
                                                         command tl tmpvar varmap

                         | Exit_parse                 -> flush stdout; prerr_endline "Parse was exited."; command [] tmpvar varmap (* call again with nothing-left-to-do *)


                         | Html_decode                ->
                                                         verbose_printf "%s" "Html_decode\n";
                                                         let newvar =
                                                           begin
                                                           match tmpvar with
                                                             | String str            -> String ( html_decode str )
                                                             | String_array strarr   -> String_array ( Array.map html_decode strarr )
                                                             | Document(doc, url)    -> Document( html_decode doc, html_decode url )
                                                             | Document_array docarr -> Document_array( Array.map (fun (d,u) -> (html_decode d, html_decode u) ) docarr )
                                                             | Url  (url, ref)       -> Url( html_decode url, html_decode ref )
                                                             | Url_list    urllist   -> Url_list( List.map (fun (u,r) -> (html_decode u, html_decode r) ) urllist)
                                                             | Url_array urlarr      -> Url_array( Array.map (fun (u,r) -> (html_decode u, html_decode r) ) urlarr )
                                                             (*
                                                             *)
                                                             | _ -> raise Wrong_argument_type
                                                           end
                                                         in
                                                         command tl newvar varmap

                         | Dummy                      -> command tl tmpvar varmap (* does nothing; just a Dummy (NOP) *)

                     end


    in
      command cmdlst Empty Varmap.empty








(* ============================================ *)
(* read the parser-definitions from files.      *)
(* -------------------------------------------- *)
(* the filenames are given as list of filenames *)
(* ============================================ *)
let parse_parser_definitions_from_files filenames_list =

  let tokenlist = ref [] in

  List.iter ( fun filename ->
                              let input_channel = open_in filename in

                              let lexer = Lexing.from_channel input_channel in
                                begin
                                  try
                                    while true do
                                      let result = Scriptparser.main Scriptlexer.read_command lexer in
                                      tokenlist := result :: !tokenlist
                                    done
                                  with
                                    | End_of_file         -> verbose_printf "End of rc-file reached; parser definitions were read."
                                    | Parsing.Parse_error -> 
                                                             prerr_string "Parse error in line ";
                                                             prerr_int !Scriptlexer.linenum;
                                                             prerr_newline();
                                                             exit 1

                                     (*
                                     | Not_found -> prerr_string "Variable not known in line ";
                                                    prerr_int !Scriptlex.linenum;prerr_newline()
                                                    (*
                                                    exit 1
                                                    *)
                                     *)

                                end;
                                close_in input_channel
            ) filenames_list;

  List.rev !tokenlist



(* lookup parser-name via url *)
(* -------------------------- *)
let parsername_lookup_by_url url lookup_lst =
  let rec aux liste = match liste with
    | []       -> raise Not_found
    | hd :: tl -> let parser_url  = fst hd in
                  let parser_name = snd hd in

                  verbose_printf "parser-lookup via url: %s\n\t%s  ->  %s\n--\n" url parser_url parser_name;

                  let parser_url_len = String.length parser_url in
                  try
                    if parser_url_len > 0 && parser_url = String.sub url 0 parser_url_len then parser_name else aux tl
                  with Invalid_argument("String.sub") -> aux tl (* this happens if url is shorter than parser_url *)
  in
    aux lookup_lst





(* this function looks up the right parser for a given url and invokes that parser *)
(* =============================================================================== *)

let invoke_parser_on_url  url  parser_urllist  parser_namehash  parser_selection =

  (* look up the right parser, either via CLI-given parsername *)
  (* or via hash-lookup for that url                           *)
  (* --------------------------------------------------------- *)
  let parserdef =
      try
        begin
          match parser_selection with
            | Some parsername -> Hashtbl.find parser_namehash parsername

            | None            -> (* parsername looked up via from url *)

                                 (* comparing the url with the strings in the url-parsername-assoc-list *)
                                 (* ------------------------------------------------------------------- *)
                                 let parsername = parsername_lookup_by_url  url  parser_urllist in
                                 verbose_printf "*** selected parser: %s\n" parsername;
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
    evaluate_command_list ( Setvar(Url(url, Cli.opt.Cli.initial_referrer)) ::
                            Store "STARTURL" ::
                            Get_url(url, Cli.opt.Cli.initial_referrer) ::
                            Store("BASEDOC") ::
                            parserdef.commands )

  with (* handle exceptions from the parse-tree-evaluation *)
    | No_Match                -> Printf.eprintf "Parser problem: Could not match to pattern!\t Parse will be exited for url %s\n" url
    | Invalid_Row_Index       -> prerr_endline "Error in script! Invalid_Row_Index!\t Parse exited.\n"
    | Variable_not_found name -> Printf.eprintf "Variable_not_found: \"%s\"\t This parse exited.\n" name
    | No_document_found       -> Printf.eprintf "No_document_found for URL %s\n" url




(* ############## *)
(*    M A I N     *)
(* ############## *)


let main ()  =
    Cli.parse(); (* parse the command line *)


    (* ------------------------------------------------------------------------------------- *)
    (* Setting the default config-files                                                      *)
    (* ------------------------------------------------------------------------------------- *)
    (* Defaults will only be set, if no config-files have been set via command-line options! *)
    (* ===================================================================================== *)
    if List.length Cli.opt.Cli.rc_filenames = 0
    then
      begin

        (* XDG_CONFIG_HOME - directory *)
        (* --------------------------- *)
        let xdg_config_home =
          try Sys.getenv "XDG_CONFIG_HOME"
          with Not_found -> Filename.concat (Sys.getenv "HOME") (".config") (* fall-back value for undefed env.var *)
        in
        let xdg_config_home =
          if xdg_config_home = ""
          then Filename.concat (Sys.getenv "HOME") (".config") (* fallback-value for empty env.var *)
          else xdg_config_home
        in


        (* systemwide rc-file in /etc/ *)
        let etc_rcfile    = "/etc/any-dl.rc"                                          in

        (* "classical" rc-file (dotfile) in HOME-dir *)
        let home_rcfile   = Filename.concat (Sys.getenv "HOME") (".any-dl.rc")        in

        (* rc-file inside $XDG_CONFIG_HOME *)
        let xdg_config_rcfile = Filename.concat xdg_config_home ("any-dl.rc") in


        (* include those rc-files which do exist *)
        (* ===================================== *)
        if   Sys.file_exists etc_rcfile
        then Cli.opt.Cli.rc_filenames <- etc_rcfile :: Cli.opt.Cli.rc_filenames;

        if   Sys.file_exists home_rcfile
        then Cli.opt.Cli.rc_filenames <- home_rcfile :: Cli.opt.Cli.rc_filenames;

        if   Sys.file_exists xdg_config_rcfile
        then Cli.opt.Cli.rc_filenames <- xdg_config_rcfile :: Cli.opt.Cli.rc_filenames
      end;



    (* CLI-args plausibility checks *)
    (* ---------------------------- *)
    if Cli.opt.Cli.auto_try && Cli.opt.Cli.parser_selection != None
    then begin prerr_endline "Option auto-try and parser selection together make no sense!"; exit 1 end;
    (* ...other checks might follow here... *)

    (* if cli-switches ask for it, print all commands of the parser-language *)
    (* They wll be printed in alphabetical order.                            *)
    (* --------------------------------------------------------------------- *)
    if Cli.opt.Cli.show_commands || Cli.opt.Cli.verbose then
      begin
        print_endline "Keywords of the parser-definition language:";
        print_endline "-------------------------------------------";
        let kwlist = Hashtbl.fold (fun key value sofar -> key :: sofar ) Scriptlexer.keyword_table [] in
        List.iter ( fun kw -> Printf.printf "keyword   %s\n" kw) (List.sort compare kwlist)
      end;
      flush stdout;


    (* parse the parser-definitions *)
    (* ---------------------------- *)
    if Cli.opt.Cli.verbose then
    begin
      print_string "rc-filename(s): ";
      List.iter ( fun str -> Printf.printf "\"%s\" " str ) Cli.opt.Cli.rc_filenames;
      print_newline()
    end;

    let parserlist = parse_parser_definitions_from_files Cli.opt.Cli.rc_filenames in

    (* if cli-switches ask for it, print the number of parser-defintions found *)
    (* ------------------------------------------------------------------------------------ *)
    if Cli.opt.Cli.list_parsers || Cli.opt.Cli.verbose then
      Printf.fprintf stderr "Number of found parser definitions: %d\n" (List.length parserlist);


    (* create and initialize hashes for parser-lookup by name / url *)
    (* ------------------------------------------------------------ *)
    let parser_namehash = Hashtbl.create (List.length parserlist) in
    let parser_urllist_raw  = ref [] in
    List.iter ( fun parserdef ->
                                 (* add the parsers to the parser_name-hash (for parser-lookup by name) *)
                                 Hashtbl.add parser_namehash parserdef.parsername parserdef;

                                 (* add the parsers to the parser_url-list (for parser-lookup by url) *)
                                 (* and also print some information, if according CLI-args were set.  *)
                                 (* ----------------------------------------------------------------- *)
                                 if List.length parserdef.urllist > 0 then
                                 begin
                                   List.iter ( fun url -> 
                                                          (* add entry to list *)
                                                          (* ----------------- *)
                                                          parser_urllist_raw := (url, parserdef.parsername) :: !parser_urllist_raw;


                                                          (* If CLI-switches ask for it, print the URL and the parser's name, it is bound to *)
                                                          (* ------------------------------------------------------------------------------- *)
                                                          if Cli.opt.Cli.list_parsers || Cli.opt.Cli.verbose
                                                          then
                                                            Printf.fprintf stderr "Init: bound Base-URL %-30s -> parser %s\n" url parserdef.parsername

                                             ) parserdef.urllist;
                                 end
                                 else
                                 begin
                                    (* If CLI-switches ask for it, print the parser's-name, mentioning that no url is bound to it *)
                                    (* ------------------------------------------------------------------------------------------ *)
                                    if Cli.opt.Cli.list_parsers || Cli.opt.Cli.verbose then
                                      Printf.fprintf stderr "Init: (unbound to URL)%-30s-> parser %s\n"   ""  parserdef.parsername
                                 end

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
    if Cli.opt.Cli.auto_try
    then
      begin
        prerr_endline "option auto-try: would need to invoke all parsers now...";
        (*
        raise NOT_IMPLEMENTED_SO_FAR;
        *)
        let parsernames = Hashtbl.fold ( fun k v sofar -> k :: sofar) parser_namehash [] in
        (* for each url try the work *)
        (* ------------------------- *)
        List.iter ( fun url ->
                               (* for this url try all parsers *)
                               
                               try
                                 List.iter ( fun parsername -> prerr_endline ("Parser: " ^ parsername);
                                                               try
                                                                 invoke_parser_on_url  url  parser_urllist  parser_namehash  (Some parsername);
                                                                 if Cli.opt.Cli.auto_try_stop then raise AutoTry_success
                                                               with
                                                                 | AutoTry_success -> raise AutoTry_success
                                                                 | _               -> prerr_endline "Parser failed with exception!" (* eats exception *)
                                           ) parsernames
                               with AutoTry_success -> prerr_endline "Parser succeeded." (* catch only a success; any other exceptions igonre here *)

                  ) (List.rev Cli.opt.Cli.url_list)
      end
    else (* non-auto (normal mode) *)
      List.iter ( fun url -> invoke_parser_on_url  url  parser_urllist  parser_namehash  Cli.opt.Cli.parser_selection ) (List.rev Cli.opt.Cli.url_list)


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
