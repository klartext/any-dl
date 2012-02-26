exception Could_not_find_mediafile_link of string

let get_some_with_exit_if_exit_none  value messages = match value with
  | None       -> List.iter prerr_endline messages; exit 1
  | Some stuff -> stuff




(* ZDF-/ORF-Mediathek-get *)
(* ---------------------- *)
let orf_zdf_mediathek_get_mmsurl url suffix =

  (* get main-apage via initial URL *)
  (* ------------------------------ *)
  let main_doc_opt = Network.Curly.get url None in
  let doc          = get_some_with_exit_if_exit_none main_doc_opt ["Could not retrieve the url "; url; "\n" ]  in

  let urls = Parsers.linkextract doc in

  (* extract the ASX-file-URL *)
  (* ------------------------ *)
  let asx_urls = List.filter (fun url -> Filename.check_suffix url suffix ) urls in

  (* prepend the baseurl if necessary *)
  let asx_urls = List.map ( fun asxurl -> if Parsers.url_is_rel_root asxurl then Parsers.url_get_baseurl url ^ asxurl else asxurl ) asx_urls in


  if List.length asx_urls > 0 then
  begin
    (* create the URL for the asx-file *)
    (* ------------------------------- *)
    (*
    let asx_url = base_url_to_prepend ^ (List.hd asx_urls) in  (* select just the first one *)
    *)
    let asx_url = List.hd asx_urls in  (* select just the first one *)

    (* get the ASX-file via ASX-URL *)
    (* ---------------------------- *)
    let asx_doc_opt = Network.Curly.get asx_url None in
    let xml_doc = get_some_with_exit_if_exit_none asx_doc_opt ["Could not retrieve the "; suffix; "-document via url "; asx_url; "\n"] in

    (* extract the real URLs of the streams *)
    (* ------------------------------------ *)
    let xml_as_xml = Parsers.Xmlparse.parse_string xml_doc in
    let href_list = Parsers.xml_get_href xml_as_xml in
    href_list
  end
  else
  begin
    prerr_endline "Could not extract asf-file";
    []
  end






let zdf_example   = ("http://www.zdf.de/ZDFmediathek/beitrag/video/1577656/heute-show-vom-24.02.2012?bc=sts;stt&flash=off", ".asx")
let orf_example   = ("http://tvthek.orf.at/programs/3619175-Joachim-Gauck-im-Gespraech/episodes/3619173-Joachim-Gauck-im-Gespraech", ".asx")
let arte_example  = ("http://videos.arte.tv/de/videos/raetsel_burnout-6411716.html", ".asx")
let vimeo_example = ("http://vimeo.com/3985019", ".asx")


let example_urls = [ zdf_example; orf_example ]

let () =
  List.iter ( fun example -> let mms_urls =
                                 match example with
                                   | url, suffix -> orf_zdf_mediathek_get_mmsurl url suffix
                             in
                               List.iter print_endline mms_urls
            ) example_urls

