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

(*
Module Neturl
*)



exception Could_not_find_mediafile_link of string
exception Could_not_extract_ARTE_Ajax_url
exception Could_not_get_ARTE_xml
exception ARD_Rtmp_url_extraction_error
exception ARD_mp4_url_extraction_error

exception Unknown_Base_Url


(* of data option get content if Some data was there, otherwise throw exception *)
(* ---------------------------------------------------------------------------- *)
let get_some_with_exit_if_none  value messages exc = match value with
  | None       -> List.iter prerr_endline messages; raise exc
  | Some stuff -> stuff



(* try to get web-document, otherwise print err-msg and throw exception *)
(* -------------------------------------------------------------------- *)
let get_document url message_list exc =
  let main_doc_opt = Network.Curly.get url None in
  get_some_with_exit_if_none  main_doc_opt  message_list  exc


(* mainurl is the baseurl including potential path; suburl is either basurl, or rel-url *)
(* if suburl is rel-ur, then grab baseurl from mainurl and prepend it to suburl         *)
(* ------------------------------------------------------------------------------------ *)
let prepend_baseurl_if_necessary  mainurl  suburl =
  if Parsers.url_is_rel_root suburl then Parsers.url_get_baseurl mainurl ^ suburl else suburl


(* params: one webdoc (html) and a string-matcher  *)
(* result: list of string-matching href hyperlinks *)
(* ----------------------------------------------- *)
let get_href_from_webdoc_and_match  webdoc  matcher =
  let urls = Parsers.linkextract webdoc in
  List.filter (fun url -> matcher url ) urls


exception Mainurl_error
exception Asx_error
exception Stream_error


(* Webpage -> ASX-Link -> ASX-Page -> MMS-Link *)
(* ------------------------------------------- *)
let web_asx_mms_get url =
  let suffix = ".asx" in

  (* get main-page via initial URL *)
  (* ----------------------------- *)
  let doc = get_document url ["Could not retrieve the url "; url; "\n" ] Mainurl_error in

  (* extract the ASX-file-URL *)
  (* ------------------------ *)
  let asx_matcher str = Filename.check_suffix str suffix in (* matcht hier ".asx" file-suffix, könnte aber auch pcre-match sein... *)
  let asx_urls = get_href_from_webdoc_and_match doc ( fun str -> asx_matcher str ) in

  (* REBASE: prepend the baseurl if necessary *)
  let asx_urls = List.map ( fun asxurl -> prepend_baseurl_if_necessary url asxurl ) asx_urls in


  if List.length asx_urls > 0 then
  begin
    let asx_url = List.hd asx_urls in  (* select just the first one (easiest criterium) *)

    (* get the ASX-file via ASX-URL *)
    (* ---------------------------- *)
    let xml_doc = get_document  asx_url  ["Could not retrieve the "; suffix; "-document via url "; asx_url; "\n"] Not_found in

    (* extract the real URLs of the streams *)
    (* ------------------------------------ *)
    let xml_as_xml = Parsers.Xmlparse.parse_string xml_doc in
    let href_list  = Parsers.xml_get_href xml_as_xml in
    href_list
  end
  else
  begin
    prerr_endline "Could not extract asf-file";
    []
  end




let arte_get  url =
  (* the URL-regexps, needed for handling ARTE *)
  (* ----------------------------------------- *)
  let arte_xmlurl_re    = Pcre.regexp "ajaxUrl:.?.(/../do_delegate/videos/)([^,]+),view,commentForm.html" in
  let arte_video_url_re = Pcre.regexp "<url quality=\"([^\"]+)\">([^<]+)<"                                in

  (* hier geht es los mit dem Download des Haupt-Dokumentes von der Mediathek *)
  let doc = get_document url ["Could not retrieve the url "; url; "\n" ] Mainurl_error in

  let xml_match_opt = Parsers.if_match_give_groups doc arte_xmlurl_re in
  let xml_submatch  = get_some_with_exit_if_none  xml_match_opt  [] Could_not_extract_ARTE_Ajax_url in

  let xml_url       = Printf.sprintf "http://videos.arte.tv%s%s,view,asPlayerXml.xml" xml_submatch.(1) xml_submatch.(2) in
  let video_name    = Printf.sprintf "%s.m4v" xml_submatch.(2) in

  let xml_doc = get_document xml_url [ "Could not retrieve ARTE xml-file" ] Could_not_get_ARTE_xml in

  let urls_array_opt = Parsers.if_match_give_group_of_groups xml_doc arte_video_url_re in
  let urls_array     = get_some_with_exit_if_none urls_array_opt [] Not_found in

  let res = ref [] in
  Array.iter ( fun arr -> res := arr.(1) :: arr.(2) :: !res ) urls_array;

  !res


(* für ARD
rtmpdump -y mp4:ard/mediendb/weltweit/video/2012/0228/120228_weltweit_web-m.mp4 -r rtmp://gffstream.fcod.llnwd.net/a792/e2/   -o out_l.flv

Das funktioniert auch:

rtmpdump -r rtmp://gffstream.fcod.llnwd.net/a792/e2/mp4:ard/mediendb/weltweit/video/2012/0228/120228_weltweit_web-m.mp4   -o out_l.flv
*)

let ard_mediathek_get_rtmp_mp4_url  url =
  (* hier geht es los mit dem Download des Haupt-Dokumentes von der Mediathek *)
  let doc = get_document url ["Could not retrieve the url "; url; "\n" ] Mainurl_error in

  let rtmp_urls_opt = Parsers.if_match_give_group_of_groups_2  doc (Pcre.regexp "rtmp://[^\"]+") in
  let rtmp_urls     = get_some_with_exit_if_none rtmp_urls_opt [] ARD_Rtmp_url_extraction_error in

  let mp4_urls_opt = Parsers.if_match_give_group_of_groups_2  doc (Pcre.regexp "mp4:[^\"]+") in
  let mp4_urls     = get_some_with_exit_if_none mp4_urls_opt [] ARD_mp4_url_extraction_error in

  let links = List.map2 ( fun rtmp_arr mp4_arr -> rtmp_arr.(0) ^ mp4_arr.(0)  )  rtmp_urls mp4_urls in

  links


(* WDR:
Stringmatch auf:
  rtmp://gffstream.fcod.llnwd.net/a792/e2/mediendb/markt/video/2012/0227/120227_markt_web-m.mp4&amp;overlay
*)
let wdr_mediathek_get_rtmp_mp4_url  url = ()



(* Sender-spezifische URL-Grabber *)
(* ============================== *)
let zdf_mediathek_get_mmsurl    = web_asx_mms_get
let orf_mediathek_get_mmsurl    = web_asx_mms_get
let arte_mediathek_get_rtmp_url = arte_get
let ard_mediathek_get           = ard_mediathek_get_rtmp_mp4_url




(* the URL-grabber function is selected via getting the baseurl from the url *)
(* ------------------------------------------------------------------------- *)
let select_url_grabber_via_url  url =
  let baseurl = Parsers.url_get_baseurl url in
  (* Printf.printf "URL: %s  /// Baseurl: %s\n" url baseurl; *)
  let url_grabber = match baseurl with
    | "http://videos.arte.tv"       -> arte_mediathek_get_rtmp_url
    | "http://tvthek.orf.at"        -> orf_mediathek_get_mmsurl
    | "http://www.zdf.de"           -> zdf_mediathek_get_mmsurl
    | "http://www.ardmediathek.de"  -> ard_mediathek_get
    | _                       -> raise Unknown_Base_Url
  in
    url_grabber



let zdf_example   = "http://www.zdf.de/ZDFmediathek/beitrag/video/1577656/heute-show-vom-24.02.2012?bc=sts;stt&flash=off"
let orf_example   = "http://tvthek.orf.at/programs/3619175-Joachim-Gauck-im-Gespraech/episodes/3619173-Joachim-Gauck-im-Gespraech"
let arte_example  = "http://videos.arte.tv/de/videos/raetsel_burnout-6411716.html"
let vimeo_example = "http://vimeo.com/3985019"
let ard_example   = "http://www.ardmediathek.de/ard/servlet/content/3517136?documentId=9668526"




let example_urls = [ zdf_example; orf_example ]
let example_urls_2 = [ arte_example; ard_example ]

let all_examples = List.append example_urls example_urls_2


let () =

  List.iter ( fun url ->
                             print_endline "--------------------";
                             let url_grabber = select_url_grabber_via_url url in

                             let video_urls =
                                              try url_grabber url
                                              with Mainurl_error | Asx_error | Stream_error | Not_found -> prerr_endline "dl-error occured"; []
                             in
                               List.iter print_endline video_urls
            ) example_urls_2





