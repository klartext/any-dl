
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
                                         List.iter (fun attr -> Printf.printf "(%s,%s)\n" (fst attr) (snd attr)) attrl;
                                         List.iter aux chldl
    | PCData str -> Printf.printf "PCata: \"%s\"\n" str
  in
    aux xml


(* erst mal nur Pseudo-Code:

let orf_thek_get_mmsurl url =
  let main_doc = Network.Curly.get url in
  let asx_url = Network.linkextract main_doc in
  let asx_doc = Network.Curly.get asx_url in
  let xml = parse_string asx_doc in
  traverse_print xml

*)


let get_some_with_exit_if_exit_none  value messages = match value with
  | None       -> List.iter prerr_endline messages; exit 1
  | Some stuff -> stuff


let orf_thek_get_mmsurl url =
  let base_url = "http://tvthek.orf.at" in
  let main_doc_opt = Network.Curly.get url None in

  let doc = get_some_with_exit_if_exit_none main_doc_opt ["Could not retrieve the url "; url; "\n" ]  in
  let urls = Network.linkextract doc in
  let asx_urls = List.filter (fun url -> Filename.check_suffix url ".asx") urls in
  List.iter print_endline asx_urls;

  let asx_url = base_url ^ (List.hd asx_urls) in  (* select just the first one *)

  let asx_doc_opt = Network.Curly.get asx_url None in
  let xml = get_some_with_exit_if_exit_none asx_doc_opt ["Could not retrieve the asx-document via url "; asx_url; "\n"] in
  traverse_print (parse_string xml)



let () =
  (*
  let file1 = "3627079-CLUB-2.asx" in
  let file2 = "ORF-Club-2_Joachim-Gauck-im-Gespraech_3621047.asx" in

  let xml_stuff = List.map parse_file [ file1; file2 ] in
  List.iter traverse_print xml_stuff
  *)
  let gauck_interview_url = "http://tvthek.orf.at/programs/3619175-Joachim-Gauck-im-Gespraech/episodes/3619173-Joachim-Gauck-im-Gespraech" in
  orf_thek_get_mmsurl gauck_interview_url
