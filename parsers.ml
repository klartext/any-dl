open Nethtml
open Parsetreetypes

(* ------------------------------------------------------------------------ *)
(* Sortiere String-Liste mit Reihenfolge von a nach z; case insensitive *)
let sort stringlist = List.sort ( fun a b -> let al = String.lowercase a and bl = String.lowercase b
                                   in if al < bl then (-1) else if al = bl then 0 else 1)  stringlist






(*
let if_match_give_groups str regexp =
  if Pcre.pmatch ~rex:regexp str
  then
    begin
      let substring_obj = Pcre.exec ~rex:regexp str in
      Some (Pcre.get_substrings substring_obj)
    end
  else
    None

*)


(*
let if_match_give_group_of_groups_2 str regexp =
  if Pcre.pmatch ~rex:regexp str
  then
    begin
      let substring_obj_arr = Pcre.exec_all ~rex:regexp str in
      let lis = Array.to_list (Array.map (fun substring_obj -> Pcre.get_substrings substring_obj) substring_obj_arr ) in
      Some lis
    end
  else
    None


module Match =
  struct
    open Pcre
  end

*)



(* *)
let if_match_give_group_of_groups str regexp =
  if Pcre.pmatch ~rex:regexp str
  then
    begin
      let substring_obj_arr = Pcre.exec_all ~rex:regexp str in
      Some (Array.map (fun substring_obj -> Pcre.get_substrings substring_obj) substring_obj_arr )
    end
  else
    None





let urlmatcher url url_regexp =
  if (Pcre.pmatch ~rex:url_regexp url)
  then
    begin
      let substr_obj = Pcre.exec ~rex:url_regexp url in
        Pcre.get_substring substr_obj 0
    end
  else
    begin
      prerr_endline "url-scheme for baseurl could not be matched";
      url
    end





(* BASE-URL: alles ab einschliesslich "?" weg schnippeln *)
let re_http_base = Pcre.regexp "https{0,1}://[^?#]+"
let re_http_site = Pcre.regexp "https{0,1}://[^?#/]+/{0,1}"

let baseurl url = urlmatcher url re_http_base
let siteurl url = urlmatcher url re_http_site



type urlpath_t =   Absolute_site     (* http://...  http://.../ *)
                 | Absolute_base     (* http://.../...          *)
                 | Relative_root     (* /...                    *)
                 | Same_protocoll    (* //...                   *)
                 | Relative_local    (* ./...   ...             *)
                 | Base_url          (* ./...   ...             *)
                 | Undetected        (* ./...   ...             *)

(* is it a realative path, or an absolute one? *)
(* ------------------------------------------- *)
let re_abs_url_site = Pcre.regexp "^https{0,1}://[^/]+/{0,1}$"
let re_abs_url_base = Pcre.regexp "^https{0,1}://[^/]+/[^/]+"
let re_rel_root   = Pcre.regexp "^/[^/]"
let re_rel_root2  = Pcre.regexp "^//"
let re_rel_misc1  = Pcre.regexp "^[.]{1,2}/"
let re_rel_misc2  = Pcre.regexp "^[^/.]"

let detect_urlpath_type str =
  let res = ref Undetected in
  if Pcre.pmatch ~rex:re_abs_url_site str then res := Absolute_site;
  if Pcre.pmatch ~rex:re_abs_url_base str then res := Absolute_base;
  if Pcre.pmatch ~rex:re_rel_root  str then res := Relative_root;
  if Pcre.pmatch ~rex:re_rel_root2 str then res := Same_protocoll ;
  if Pcre.pmatch ~rex:re_rel_misc1 str then res := Relative_local;
  if Pcre.pmatch ~rex:re_rel_misc2 str then res := Relative_root;
  if Pcre.pmatch ~rex:re_http_site str then res := Absolute_site;
  if Pcre.pmatch ~rex:re_http_base str then res := Absolute_base;
  if "." = str then res := Base_url;
  !res



let re_url_scheme  = Pcre.regexp "^([a-zA-Z]+://[^/]+/{0,1})"
let re_url_scheme  = Pcre.regexp "^([a-zA-Z]+://[^/]+)"

let url_is_rel_root url   = if Pcre.pmatch ~rex:re_rel_root   url then true else false
let url_has_scheme  url   = if Pcre.pmatch ~rex:re_url_scheme url then true else false

let url_get_baseurl url = (Pcre.extract ~rex:re_url_scheme ~flags:[] url).(0)




let url_scheme url =
  Pcre.replace ~pat:"^([^:]+).*" ~itempl:(Pcre.subst "$+") url


let rebase_aggregated extracted_list baseurl =
              List.map ( fun extracted ->
         let newextracteding = 
                                       match detect_urlpath_type extracted with
                                           Absolute_site   -> extracted
                                         | Absolute_base   -> extracted
                                         | Relative_root   -> Filename.concat (siteurl baseurl) extracted
                                         | Same_protocoll  -> (url_scheme baseurl) ^ extracted
                                         | Relative_local  -> Filename.concat (siteurl baseurl) extracted
                                         | Base_url        -> baseurl
                                         | Undetected      -> baseurl
         in
         newextracteding
                                     ) extracted_list
    











(* simple URL-to-Filename converter *)
(* -------------------------------- *)
let suffixes = [".jpg"; ".png"; ".txt"; ".pdf"; ".doc"; ".ps"; ".docx"; ".xls"; ".htm"; ".html"]

let url_to_filename url_string =
  let filename = String.copy url_string in
  for idx = 0 to String.length filename - 1
  do
    match filename.[idx] with
        'a'..'z' | 'A'..'Z' | '0'..'9' | '.' -> ()
      | _ -> filename.[idx] <- '_'
  done;
  filename

let url_to_filename_with_suffix_check  str =
  let sel = List.filter ( fun suffix -> Filename.check_suffix str suffix ) suffixes in
  match sel with
    | []     -> url_to_filename str (* no suffix that must get specially saved: convert completely *)
    | hd::tl -> let chopped = Filename.chop_suffix str hd in
                url_to_filename chopped  ^  hd




let print_times ?(scale=1) character times =
  for idx = 1 to times * scale
    do
      print_char character
    done


(* DUMPING HTML *)

(*
val parse_document : ?dtd:simplified_dtd ->
     ?return_declarations:bool ->
     ?return_pis:bool ->
     ?return_comments:bool -> Lexing.lexbuf -> document list
*)

module Xmlparse =
  struct
    open Xml

    let print_element elem =
      let a = fst elem in
      let second = snd elem in
      let b = fst second in
      let c = snd second in
      Printf.printf "(%s, (%s /// %s))\n" a b c


    let traverse_print xml =
      print_endline "------------------------------------------------";
      let rec aux xml_elem = match xml_elem with
        | Element (tagname, attrl, chldl) -> Printf.printf "Tagname: %s\n" tagname;
                                             List.iter (fun attr -> Printf.printf "\t (attrname,attrval) = (%s,%s)\n"
                                                                    (fst attr) (snd attr)) attrl;
                                             List.iter aux chldl
        | PCData str -> Printf.printf "PCata: \"%s\"\n" str
      in
        aux xml

    let parse_string = parse_string

    (* ggf. String-lowercase bei "href"-Vergleich einsetzen *)
    (* ---------------------------------------------------- *)
    let get_href_from_asx xml = 
      let href_list = ref [] in
      let rec aux xml_elem = match xml_elem with
        | Element (tagname, attrl, chldl) -> (* only select ref *) (* IST DAS ALLGEMEINGUELTIG????? *)
                                             List.iter (fun attr ->
                                                                    if fst attr = "href" then href_list := snd attr :: !href_list
                                                       ) attrl;
                                             List.iter aux chldl
        | PCData str -> () (*Printf.printf "PCata: \"%s\"\n" str*)
      in
        aux xml;
        !href_list

  end



(* ========================================================================================================================= *)
(* ========================================================================================================================= *)
(* ========================================================================================================================= *)

module Htmlparse =
  struct

    let dump_html str =
      let doclist = (Nethtml.parse ~return_declarations:true ~return_pis:true ~return_comments:true (new Netchannels.input_string str)) in

      let rec traverse_aux doclist depth =
        
        match doclist with
        |  hd::tl ->
            begin
              match hd with
                | Element (tag, arg, dl) ->
                                               print_times '_' depth ~scale:2;
                                               Printf.printf "<%s> " tag;
                                               List.iter ( fun a ->
                                                           (*
                                                           print_times '*' depth;
                                                           *)
                                                           Printf.printf "%s=\"%s\"\t" (fst a) (snd a)
                                                         ) arg;
                                           traverse_aux dl (depth+1);
                                           print_times '_' depth ~scale:2;    Printf.printf "</%s>\n" tag

                | Data    data          -> print_newline();print_times '_' depth ~scale:2; Printf.printf "data: %s\n" data 
            end;
            traverse_aux tl depth
        | [] -> ()
      in
        traverse_aux doclist 0




    (* Analysing HTML *)

    let parse_html ?(datamatch="") ?(tagmatch="") ?(subtag=None) ?(matcher=fun (matcher:string) -> true) str =
      let doclist = (Nethtml.parse(new Netchannels.input_string str)) in
      let collection = ref [] in (* string list that will be collected *)

      let traverse doclist tagmatch subtag2 datamatch =

        let rec traverse_aux doclist depth =

          
          match doclist with
          |  hd::tl ->
              begin
                match hd with
                  | Element (tag, arg, dl) ->
                                             traverse_aux dl (depth+1);
                                                 if tag = tagmatch
                                                 then
                                                     List.iter ( fun a ->
                                                                 let st_arg = fst a in

                                                                 match subtag with
                                                                   | None    ->  collection := (snd a) :: !collection
                                                                   | Some st -> if st_arg = st then collection := (snd a) :: !collection
                                                                 
                                                               ) arg
                  | Data    data          -> if data = datamatch then ignore (data :: !collection)   (*pd();Printf.printf "traverse_aux: Data found: \"%s\"\n" data*)
              end;
              traverse_aux tl depth
          | [] -> (*print_endline "=========================================";
                  print_stringlist_endline (Strlist !collection);*)
                  !collection
        in
          traverse_aux doclist 0

      in

      let parsed = traverse  doclist tagmatch subtag datamatch in (* calling the traverser *)
      List.filter matcher parsed                                  (* filter the result     *)

  end (* Htmlparse-Ende *)
(* ========================================================================================================================= *)
(* ========================================================================================================================= *)

open Htmlparse

let linkextract  = parse_html ~tagmatch:"a" ~subtag:(Some "href")
let imageextract = parse_html ~tagmatch:"img" ~subtag:(Some "src")


let xml_get_href  = Xmlparse.get_href_from_asx



(* --------------------------------------------------------------------------- *)
(* parses xml out of a flat string and afterwards parses hrefs out of the xml  *)
(* --------------------------------------------------------------------------- *)
(* or in short: feed in xml-document as plain string, and get out list of href *)
(* --------------------------------------------------------------------------- *)
let xml_get_href_from_string  str = Xmlparse.get_href_from_asx ( Xmlparse.parse_string str )


