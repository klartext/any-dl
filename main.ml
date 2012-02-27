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


(* of data option get content if Some data was there, otherwise throw exception *)
(* ---------------------------------------------------------------------------- *)
let get_some_with_exit_if_exit_none  value messages exc = match value with
  | None       -> List.iter prerr_endline messages; raise exc
  | Some stuff -> stuff



(* try to get web-document, otherwise print err-msg and throw exception *)
(* -------------------------------------------------------------------- *)
let get_document url message_list exc =
  let main_doc_opt = Network.Curly.get url None in
  get_some_with_exit_if_exit_none  main_doc_opt  message_list  exc


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


(* ZDF-/ORF-Mediathek-get *)
(* ---------------------- *)
let orf_zdf_mediathek_get_mmsurl url suffix =

  (* get main-apage via initial URL *)
  (* ------------------------------ *)
  let doc = get_document url ["Could not retrieve the url "; url; "\n" ] Mainurl_error in

  (* extract the ASX-file-URL *)
  (* ------------------------ *)
  let asx_matcher str = Filename.check_suffix str ".asx" in (* matcht hier ".asx" file-suffix, könnte aber auch pcre-match sein... *)
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


(*
 ARTE:

   id_re = re.compile('ajaxUrl:.?.(/../do_delegate/videos/)([^,]+),view,commentForm.html')
   xmlurl = 'http://videos.arte.tv%s%s,view,asPlayerXml.xml' % (match.group(1), match.group(2))
   name = '%s.m4v' % match.group(2)

*)




let zdf_example   = ("http://www.zdf.de/ZDFmediathek/beitrag/video/1577656/heute-show-vom-24.02.2012?bc=sts;stt&flash=off", ".asx")
let orf_example   = ("http://tvthek.orf.at/programs/3619175-Joachim-Gauck-im-Gespraech/episodes/3619173-Joachim-Gauck-im-Gespraech", ".asx")
let arte_example  = ("http://videos.arte.tv/de/videos/raetsel_burnout-6411716.html", ".asx")
let vimeo_example = ("http://vimeo.com/3985019", ".asx")


let example_urls = [ zdf_example; orf_example ]

let () =
  List.iter ( fun example -> let mms_urls =
                                 match example with
                                   | url, suffix -> try orf_zdf_mediathek_get_mmsurl url suffix
                                                    with Mainurl_error | Asx_error | Stream_error | Not_found -> prerr_endline "dl-error occured"; []
                             in
                               List.iter print_endline mms_urls
            ) example_urls

