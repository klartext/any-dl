(*
  any-dl:
  -------
  Generic Media-Downloader for any kind of Online-Mediathek.

  Author / copyright: Oliver Bandel
  Copyleft:           GNU GENERAL PUBLIC LICENSE  v3 (or higher)
*)
open Nethtml
open Parsetreetypes

(* ------------------------------------------------------------------------ *)
(* Sortiere String-Liste mit Reihenfolge von a nach z; case insensitive *)
let sort stringlist = List.sort ( fun a b -> let al = String.lowercase a and bl = String.lowercase b
                                   in if al < bl then (-1) else if al = bl then 0 else 1)  stringlist




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



(* this stuff can be done with Neturl-module also!!! *)
let re_url_scheme  = Pcre.regexp "^([a-zA-Z]+://[^/]+)"
(* this stuff can be done with Neturl-module also!!! *)
let url_get_baseurl url = (Pcre.extract ~rex:re_url_scheme ~flags:[] url).(0)
(* this stuff can be done with Neturl-module also!!! *)




module Rebase =
struct
  open Neturl

  let rebase_url  url  extracted_link =
    let syntax = Hashtbl.find common_url_syntax "http" in

(*
Printf.printf "url:            %s\n" (url);
Printf.printf "extracted_link  %s\n" (extracted_link);
print_endline "---------";
*)
    try
      let neturl        = parse_url ~accept_8bits:true ~enable_fragment:true  ~base_syntax:syntax  url in
      let extracted_url = parse_url ~accept_8bits:true ~enable_fragment:true  ~base_syntax:syntax  extracted_link in
      let base          = remove_from_url ~path:true ~query:true ~fragment:true neturl in
      let absurl        = ensure_absolute_url ~base:neturl extracted_url in
(*
Printf.printf "neturl:         %s\n" (string_of_url neturl);
Printf.printf "extracted_url:  %s\n" (string_of_url extracted_url);
Printf.printf "base:           %s\n" (string_of_url base);
Printf.printf "absurl:         %s\n" (string_of_url absurl);
print_endline "----------------------------------------------------";
*)
      Some (string_of_url absurl)
    with
      Neturl.Malformed_URL -> None

end




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
                                           print_times '_' depth ~scale:2;
                                           Printf.printf "</%s>\n" tag

                | Data    data          -> print_newline();print_times '_' depth ~scale:2; Printf.printf "data: %s\n" data 
            end;
            traverse_aux tl depth
        | [] -> ()
      in
        traverse_aux doclist 0




    let dump_html_data str =
      let doclist = (Nethtml.parse ~return_declarations:true ~return_pis:true ~return_comments:true (new Netchannels.input_string str)) in

      let rec traverse_aux doclist depth =
        
        match doclist with
        |  hd::tl ->
            begin
              match hd with
                | Element (tag, arg, dl) -> traverse_aux dl (depth+1) (* ignore tags, just traverse deeper *)
                | Data    data          -> print_endline data         (* print the data-part solely *)
            end;
            traverse_aux tl depth
        | [] -> ()
      in
        traverse_aux doclist 0

  



    let collect_subtags (args: (string*string)list) (subtag_select:string option) =
      match subtag_select with
        | None        -> [] (*List.map snd args*)
        | Some subsel -> let selected_pairs = List.filter ( fun pair -> fst pair = subsel ) args in
                         List.map snd selected_pairs


let debug = false

    (* Analysing HTML *)

    let parse_html ?(pickdata=false) ?(tagmatch="") ?(subtag=None) ?(matcher=fun (matcher:string) -> true) str =
if debug then
Printf.printf " ##### TAGMATCH: %s\n" tagmatch;

      let doclist = (Nethtml.parse(new Netchannels.input_string str)) in

        let rec traverse_aux doclist depth collected pick_this_data cur_tag =
          match doclist with
            | []     -> collected (* collected data *)
            | hd::tl ->
                        (* first we work at the head *)
                        (* ------------------------- *)
                        let sample =
                          begin
                            match hd with
                              | Element (tag, args, dl) ->
                                                           if  debug then
                                                           begin
                                                           print_times '-' depth ~scale:4;
                                                           Printf.printf "TAG: <%s>  \n" tag
                                                           end;

                                                           let results_of_subdocs =
                                                             match tag = tagmatch with
                                                               | true  -> traverse_aux dl (depth+1) [] false tag(* only next recursion step might be true *)
                                                               | false -> traverse_aux dl (depth+1) [] false tag
                                                           in

                                                           let result_of_subtags = collect_subtags args subtag in
                                                           List.append results_of_subdocs result_of_subtags

                              | Data    data          ->
                                                        if debug then Printf.printf "pick_this_data = %s\n" (if pick_this_data then "true" else "false");
                                                        if debug then Printf.printf "**** DATA: %s\n" data;
                                                        if debug then Printf.printf "cur_tag: %s\n"   cur_tag;
                                                        if pick_this_data || cur_tag = tagmatch
                                                        then [data]
                                                        else []
                          end
                        in
                        (* then we work at the tail *)
                        (* ------------------------ *)
                        if debug then List.iter (fun str -> Printf.printf "sample ===> %s\n" str) sample;
                        traverse_aux tl (depth+0) (List.append sample  collected) false cur_tag
        in
          traverse_aux doclist 0 [] pickdata ""

  end (* Htmlparse-Ende *)
(* ========================================================================================================================= *)
(* ========================================================================================================================= *)

open Htmlparse

let linkextract    doc = parse_html ~tagmatch:"a"   ~subtag:(Some "href") doc
let imageextract   doc = parse_html ~tagmatch:"img" ~subtag:(Some "src")  doc
let titleextract   doc = parse_html ~tagmatch:"title" ~pickdata:false      doc

let tagextract tag doc = parse_html ~pickdata:true ~tagmatch:tag doc


let xml_get_href  = Xmlparse.get_href_from_asx



(* --------------------------------------------------------------------------- *)
(* parses xml out of a flat string and afterwards parses hrefs out of the xml  *)
(* --------------------------------------------------------------------------- *)
(* or in short: feed in xml-document as plain string, and get out list of href *)
(* --------------------------------------------------------------------------- *)
let xml_get_href_from_string  str = Xmlparse.get_href_from_asx ( Xmlparse.parse_string str )


