

exception NOT_IMPLEMENTED_SO_FAR (* for planned, but not already implemented functionality *)
exception Command_Sequence_error of string (* for sequences that are not allowed *)

exception No_document_found
exception No_Match



type match_result_t = string array array
type selected_t     = string array list
type selector_t     = ( match_result_t -> string -> selected_t )


type commands_t =
  | Get       of string * string              (* url, referrer *)
  | Match     of string * string              (* contents, regexp-pattern-string *)
  | Select    of match_result_t * selector_t
  | Print
  | Print_string of string
  | Save      of string * string
  | Dummy

let command_to_string cmd = match cmd with
  | Get     _      -> "Get"
  | Match   _      -> "Match"
  | Select  _      -> "Select"
  | Print          -> "Print"
  | Print_string _ -> "Print_string"
  | Save         _ -> "Save"
  | Dummy          -> "Dummy"
  

type results_t =
  | Document of string
  | Printed
  | Dummy_result
  | Match_result of match_result_t option


(*
  let mp4_urls_opt = Parsers.if_match_give_group_of_groups_2  doc (Pcre.regexp "http://.*?mp4") in
*)



let example_commands = [ Get("http://www.first.in-berlin.de", ""); Print_string "xxxxxx"; Dummy; Print_string "sdfiuzsfd" ]
(*
let example_commands = [ Get("http://www.first.in-berlin.de", ""); Print_string "xxxxxx"; Dummy; Print_string "sdfiuzsfd" ]
let example_commands = [ Get("http://www.first.in-berlin.de", ""); Print_string "xxxxxx"; Dummy ]
let example_commands = [ Get("http://www.first.in-berlin.de", ""); Dummy ]
let example_commands = [ Get("http://www.first.in-berlin.de", ""); Print ]
*)

let print_warning str = prerr_string "WARNING: "; prerr_endline str


let evaluate_command_list cmdlst =
  let rec command commandlist = match commandlist with
    | []                          -> prerr_endline "EMPTY!"
    | cmd::[] (* evtl. guard ? *) -> prerr_endline "HALLO, match case 2 -- only commands without follower do make sense here "
                                         (* Print_string, Save, Dummy *)
    | cmd::next::tl               -> begin
                                       match cmd with
                                         | Get (url, referrer)            -> let document = Network.Curly.get url (Some referrer) in
                                                                               let doc = 
                                                                                 begin
                                                                                   match document with
                                                                                     | Some d -> d
                                                                                     | None -> raise No_document_found       
                                                                                 end
                                                                               in
                                                                                 begin
                                                                                 match next with
                                                                                   | Print -> command (Print_string doc :: tl)
                                                                                   | Save _ -> raise NOT_IMPLEMENTED_SO_FAR
                                                                                   | Dummy  -> ()
                                                                                   (*
                                                                                   | Print_string str  -> print_warning "document unused";
                                                                                                          command (Print_string str :: tl)
                                                                                   *)
                                                                                   | _ as errcmd     -> raise (Command_Sequence_error (command_to_string errcmd))
                                                                                 end


                                         | Match   (str, pattern)     -> let match_res = Parsers.if_match_give_group_of_groups str (Pcre.regexp pattern) in
                                                                         let matched =
                                                                           begin
                                                                             match match_res with
                                                                               | None   -> raise No_Match
                                                                               | Some res -> res
                                                                           end
                                                                         in
                                                                         ignore(matched);
                                                                         print_endline "Match   detected"; command (next::tl)

                                         | Select _                   -> print_endline "Select detected"; command (next::tl)
                                         | Print                      -> command (next::tl) (* already handled by former recursion step *)
                                         | Print_string str           -> print_endline str; command (next::tl)
                                         | Save   _                   -> print_endline "Save detected"; command (next::tl)
                                         | Dummy                      -> print_endline "Dummy detected"; command (next::tl)
                                     end


  in
    command cmdlst




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
exception ARD_Mp4_url_extraction_error
exception ARD_mp4_url_extraction_error
exception NDR_url_extraction_error

exception Unknown_Base_Url


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


  let all_hrefs = ref [] in
  List.iter ( fun asx_url ->
                              let asx_url = List.hd asx_urls in  (* select just the first one (easiest criterium) *)

                              (* get the ASX-file via ASX-URL *)
                              (* ---------------------------- *)
                              let xml_doc = get_document  asx_url  ["Could not retrieve the "; suffix; "-document via url "; asx_url; "\n"] Not_found in

                              (* extract the real URLs of the streams *)
                              (* ------------------------------------ *)
                              let xml_as_xml = Parsers.Xmlparse.parse_string xml_doc in
                              let href_list  = Parsers.xml_get_href xml_as_xml in
                              all_hrefs := List.append href_list !all_hrefs
            ) asx_urls;
  if List.length asx_urls = 0 then raise Not_found;
  !all_hrefs




let arte_get  url =

  (* hier geht es los mit dem Download des Haupt-Dokumentes von der Mediathek *)

  (* GET *)
  (* --- *)
  let doc = get_document url ["Could not retrieve the url "; url; "\n" ] Mainurl_error in
  (* EXTRACT *)
  (* ------- *)
  (* the URL-regexps, needed for handling ARTE *)

  let arte_xmlurl_re    = Pcre.regexp "ajaxUrl:.?(/../do_delegate/videos/)([^,]+),view,ratingForm.html" in
  (* extracting the stuff *)
  let xml_match_opt   = Parsers.if_match_give_groups  doc  arte_xmlurl_re in
  let xml_submatch    = extract_some_with_exit_if_none  xml_match_opt  ["xml_submatch"] Could_not_extract_ARTE_Ajax_url in
  flush stdout;

  let xml_url         = Printf.sprintf "http://videos.arte.tv%s%s,view,asPlayerXml.xml" xml_submatch.(1) xml_submatch.(2) in


  (* GET *)
  (* --- *)
  let xml_doc = get_document xml_url [ "Could not retrieve ARTE xml-file" ] Could_not_get_ARTE_xml in

  (* EXTRACT *)
  (* ------- *)
  let arte_video_url_re = Pcre.regexp "<url quality=\"([^\"]+)\">([^<]+)<"                                in
  let urls_array_opt = Parsers.if_match_give_group_of_groups xml_doc arte_video_url_re in
  let urls_array     = extract_some_with_exit_if_none urls_array_opt ["urls_array_opt: error"] Not_found in

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
  (* GET *)
  (* --- *)
  let doc = get_document url ["Could not retrieve the url "; url; "\n" ] Mainurl_error in

  (* EXTRACT *)
  (* ------- *)
(*
  let mp4_urls_opt = Parsers.if_match_give_group_of_groups_2  doc (Pcre.regexp "http://[^\"]+?mp4\"") in
  let mp4_urls     = extract_some_with_exit_if_none mp4_urls_opt [] ARD_Mp4_url_extraction_error in
*)

  let rtmp_urls_opt = Parsers.if_match_give_group_of_groups_2  doc (Pcre.regexp "rtmpt{0,1}://[^\"]+") in
  let rtmp_urls     = extract_some_with_exit_if_none rtmp_urls_opt [] ARD_Rtmp_url_extraction_error in
  (*
  *)

  let mp4_urls_opt = Parsers.if_match_give_group_of_groups_2  doc (Pcre.regexp "mp4:[^\"]+") in
  let mp4_urls     = extract_some_with_exit_if_none mp4_urls_opt [] ARD_mp4_url_extraction_error in
  (*
  *)

  (* zipping *)
  let links = List.map2 ( fun rtmp_arr mp4_arr -> rtmp_arr.(0) ^ "   " ^ mp4_arr.(0)  )  rtmp_urls mp4_urls in
  (*
  let links = List.map ( fun mp4_arr ->  mp4_arr.(0)  )  mp4_urls in
  *)

  links


let ard_mediathek_get_rtmp_mp4_url_version_2  url =

  (* hier geht es los mit dem Download des Haupt-Dokumentes von der Mediathek *)
  (* GET *)
  (* --- *)
  let doc = get_document url ["Could not retrieve the url "; url; "\n" ] Mainurl_error in

  (* EXTRACT *)
  (* ------- *)
  let mp4_urls_opt = Parsers.if_match_give_group_of_groups_2  doc (Pcre.regexp "http://[^\"]+?mp4\"") in
  let mp4_urls     = extract_some_with_exit_if_none mp4_urls_opt [] ARD_Mp4_url_extraction_error in

  (*
  let rtmp_urls_opt = Parsers.if_match_give_group_of_groups_2  doc (Pcre.regexp "rtmpt{0,1}://[^\"]+") in
  let rtmp_urls     = extract_some_with_exit_if_none rtmp_urls_opt [] ARD_Rtmp_url_extraction_error in
  *)

  (*
  let mp4_urls_opt = Parsers.if_match_give_group_of_groups_2  doc (Pcre.regexp "mp4:[^\"]+") in
  let mp4_urls     = extract_some_with_exit_if_none mp4_urls_opt [] ARD_mp4_url_extraction_error in
  *)

  (* zipping *)
  (*
  let links = List.map2 ( fun rtmp_arr mp4_arr -> rtmp_arr.(0) ^ "   " ^ mp4_arr.(0)  )  rtmp_urls mp4_urls in
  *)
  let links = List.map ( fun mp4_arr ->  mp4_arr.(0)  )  mp4_urls in

  links


let ard_mediathek_get_rtmp_mp4_url_version_3  url =

  (* hier geht es los mit dem Download des Haupt-Dokumentes von der Mediathek *)
  (* GET *)
  (* --- *)
  let doc = get_document url ["Could not retrieve the url "; url; "\n" ] Mainurl_error in

  (* EXTRACT *)
  (* ------- *)
  let mp4_urls_opt = Parsers.if_match_give_group_of_groups_2  doc (Pcre.regexp "(rtmpt{0,1}://[^\"])(.*?)(mp4:[^\"]+)\"") in
  let mp4_urls     = extract_some_with_exit_if_none mp4_urls_opt [] ARD_Mp4_url_extraction_error in

  (*
  let rtmp_urls_opt = Parsers.if_match_give_group_of_groups_2  doc (Pcre.regexp "rtmpt{0,1}://[^\"]+") in
  let rtmp_urls     = extract_some_with_exit_if_none rtmp_urls_opt [] ARD_Rtmp_url_extraction_error in
  *)

  (*
  let mp4_urls_opt = Parsers.if_match_give_group_of_groups_2  doc (Pcre.regexp "mp4:[^\"]+") in
  let mp4_urls     = extract_some_with_exit_if_none mp4_urls_opt [] ARD_mp4_url_extraction_error in
  *)

  (* zipping *)
  (*
  let links = List.map2 ( fun rtmp_arr mp4_arr -> rtmp_arr.(0) ^ "   " ^ mp4_arr.(0)  )  rtmp_urls mp4_urls in
  *)
  let links = List.map ( fun mp4_arr ->  mp4_arr.(0)  )  mp4_urls in

  links




(* http://www.ndr.de/fernsehen/sendungen/45_min/videos/minuten393.html

<div id="flash_player_audio_gallery_minuten393">
F&uuml;r diesen Inhalt muss JavaScript aktiviert und die aktuelle Version vom Adobe Flash Player installiert sein. Sie k&ouml;nnen den Player hier runterladen. <a href="http://get.adobe.com/de/flashplayer/" title="Adobe Flash Player runterladen">http://get.adobe.com/de/flashplayer/</a>
<div class="filename invisible" data-value="http://media.ndr.de/progressive/2011/1205/TV-20111205-2327-1201.hq.mp4"></div>
<div class="imgname invisible" data-value="/fernsehen/sendungen/45_min/hintergrund/paketsklaven133_v-contentgross.jpg"></div>
</div>
*)


let ndr_mediathek_get  url =
  (* hier geht es los mit dem Download des Haupt-Dokumentes von der Mediathek *)

  (* GET *)
  (* --- *)
  let doc = get_document url ["Could not retrieve the url "; url; "\n" ] Mainurl_error in

  (* mp4-url extrahieren *)
  let mp4_urls_opt = Parsers.if_match_give_group_of_groups_2  doc (Pcre.regexp "http://.*?mp4") in
  let mp4_urls     = extract_some_with_exit_if_none mp4_urls_opt [] NDR_url_extraction_error in

  let links = List.map ( fun mp4_arr -> mp4_arr.(0)  )  mp4_urls in

  links




(* WDR:
Stringmatch auf:
  rtmp://gffstream.fcod.llnwd.net/a792/e2/mediendb/markt/video/2012/0227/120227_markt_web-m.mp4&amp;overlay

WDR:

http://www.wdr.de/tv/quarks/sendungsbeitraege/2009/1222/003_arena_mannfrau.jsp?startMedium=122037&startPicture=/tv/fsstd-technik/codebase/img/default_startbild.jpg&dslSrc=rtmp://gffstream.fcod.llnwd.net/a792/e2/tv/quarks/091229_das_eyetracking_experiment_big.flv&overlayPic=/tv/quarks/codebase/img/overlay_video.png&offset=0&red=fsstd-tv%2Fquarks&base=/tv/quarks/codebase/video/&isdnSrc=rtmp://gffstream.fcod.llnwd.net/a792/e2/tv/quarks/091229_das_eyetracking_experiment_small.flv

Stringmatch auf:
dslSrc=rtmp://gffstream.fcod.llnwd.net/a792/e2/tv/quarks/091229_das_eyetracking_experiment_big.flv&

*)
let wdr_mediathek_get_rtmp_mp4_url  url = ()


(*
  Pro-Sieben:


  Im Gulli-Board konnte ich folgenden Trick finden

  http://www.prosieben.de/dynamic/h264/h264map/?ClipID=<...>
  <...> durch die ID des Videos ersetzen

  Für dieses Video (www.prosieben.de/tv/galileo/videos/clip/288557-asperger-informatik-1.3122799) also
  http://www.prosieben.de/dynamic/h264/h264map/?ClipID=288557

  bei Aufruf erfolgt eine Weiterleitung zu

  http://video.sevenoneintermedia.de/clips/geo_d_at_ch/mp4-840 /288000/288557-840-553674.mp4?s=1&t=20120331

  werde mich bei Gelegenheit noch vorstellen

  [Aktualisiert am: Sa, 31 März 2012 22:50]
*)

(*
 WDR Monitor:

   <a title="Video: Flashplayer ab Version 8.0 erforderlich" rel="base#/tv/monitor/codebase/video/" href="http://www.wdr.de/themen/global/flashplayer/fscreen.jhtml?dslSrc=rtmp://gffstream.fcod.llnwd.net/a792/e2/mediendb/monitor08/video/2012/0426/120426_monitor_web-l.mp4&amp"
*)

(* Sender-spezifische URL-Grabber *)
(* ============================== *)
let zdf_mediathek_get_mmsurl    = web_asx_mms_get
let orf_mediathek_get_mmsurl    = web_asx_mms_get
let arte_mediathek_get_rtmp_url = arte_get
(*
let ard_mediathek_get           = ard_mediathek_get_rtmp_mp4_url
*)
let ard_mediathek_get           = ard_mediathek_get_rtmp_mp4_url_version_3
let ndr_mediathek_get           = ndr_mediathek_get

let _3sat_mediathek_get = ()
(*
1. suche hauptseite
2. get &mode=play - Seite
3. get smil-Datei
4. ...

120308_japan_scobel.smil
*)

(*
 Beispiel: "Just Ballet"
   http://www.3sat.de/mediathek/?display=1&mode=play&obj=30650

enthält:
   (...)
   playerBottomFlashvars.mediaURL = "http://fstreaming.zdf.de/3sat/veryhigh/120427_justballet_ganzesendung1neu_musik.smil";
   (...)


*)



(* the URL-grabber function is selected via getting the baseurl from the url *)
(* ------------------------------------------------------------------------- *)
let select_url_grabber_via_url  url =
  let baseurl = Parsers.url_get_baseurl url in
  (* Printf.printf "URL: %s  /// Baseurl: %s\n" url baseurl; *)
  let url_grabber = match baseurl with
    | "http://videos.arte.tv"                -> arte_mediathek_get_rtmp_url
    | "http://tvthek.orf.at"                 -> orf_mediathek_get_mmsurl
    | "http://www.zdf.de"                    -> zdf_mediathek_get_mmsurl
    | "http://www.ardmediathek.de"           -> ard_mediathek_get
    | "http://mediathek.daserste.de"         -> ard_mediathek_get
    | "http://www.ndr.de"                    -> ndr_mediathek_get
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


let do_old_any_dl () =

  let urls_from_argv = List.tl ( Array.to_list Sys.argv ) in
  List.iter ( fun url ->
                             print_endline "# --------------------";
                             let url_grabber = select_url_grabber_via_url url in

                             let video_urls =
                                              try url_grabber url
                                              with Mainurl_error | Asx_error | Stream_error | Not_found -> prerr_endline "dl-error occured"; []
                             in
                               List.iter print_endline video_urls
            ) urls_from_argv




let _  =
  (*
    do_old_any_dl ()
  *)
  (*
  *)
  evaluate_command_list example_commands


