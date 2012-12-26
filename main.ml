

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



(*
type selected_t     = string array list
*)
(*
type selector_t     = ( match_result_t -> match_result_t ) (* function, that has a certain algorithm to select certain match_result_t *)
*)

type url_t = { url: string; referrer: string }








let print_warning str = prerr_string "WARNING: "; prerr_endline str







(*
  let mp4_urls_opt = Parsers.if_match_give_group_of_groups_2  doc (Pcre.regexp "http://.*?mp4") in
*)



(* TESTING / DEVEL purposes only *)
(*
let example_commands = []
let example_commands = [ Dummy ]
let example_commands = [ Dummy; Dummy ]
let example_commands = [ Dummy; Dummy; Dummy ]

let example_commands = [ Get_url("http://www.zdf.de/ZDFmediathek/beitrag/video/1649590/Happy-Birthday-Raumschiff-Enterprise?bc=sts;stt&flash=off", "");
                         Match "href=.(.*?asx)";
                         Print;
                         Showvar;
                         Select (fun x -> [|x.(0)|]);
                         Print;
                         Showvar;
                         Setvar( Url_list [("http://www.first.in-berlin.de", "-"); ("http://www.google.de", "")]);
                         Showvar;
                         Get_urls;
                         Dummy
                       ]

let example_commands = [ Setvar( Url ("http://www.first.in-berlin.de", "-") ); Get;
                         Match("H2>(.*?)</H2");
                         Showvar;
                         Print;
                         Select( example_selector );
                         Dummy ]
*)



(*
let  printstuff mres = Array.iter ( fun x -> Array.iter ( fun y -> Printf.printf "\"%s\" ||| " y) x; print_newline() ) mres

let select res_arr stringmatcher =
  let collect = ref [] in
  Array.iter ( fun x -> let match_flag = ref false in
                        Array.iter ( fun y -> Printf.printf "\"%s\" ||| " y; if stringmatcher y then match_flag := true ) x;
                        print_newline();
                        if !match_flag then collect := x :: !collect
                        
             ) res_arr;
*)


(*
let select_where_laban matched =
  Array.iter( fun line -> Array.iter ( fun y ->  print_endline y; y ) line; print_newline(); line  ) matched
*)

let example_selector stuff =
  Printf.printf "Array.length: %d\n" (Array.length stuff);
  Array.iter ( fun line ->
                           Printf.printf "\n\n-------------------------------\nArray.length of LINE: %d\n"  (Array.length line);
                           Array.iter print_endline line;
                           print_endline "-------------------------------"
             ) stuff;
  stuff


module Selector =
  struct
    exception Selection_Index_error

    let select_line ( matcharr : match_result_t ) lnum = Array.copy matcharr.(lnum)

    let select_first_match matcharr = Array.create 1 (Array.copy matcharr.(0))

    (* matcharr is a selction result; lineselection is a selection-index-array *)
    (* ----------------------------------------------------------------------- *)
    (* be aware: 0 means fullmatch-line, so for all real lines, add + 1        *)
    (* ----------------------------------------------------------------------- *)
    (*
    let select_lines (matcharr:match_result_t) lineselection =
      let maxline_idx = Array.length matcharr - 1 in
      let maxcol_idx = Array.length matcharr.(0) - 1 in

      let result_linenum = Array.length lineselection in

      if List.exists ( fun selidx -> selidx > maxline_idx  ) (Array.to_list lineselection) then raise Selection_Index_error;

      let res = Array.make_matrix result_linenum maxcol_idx in

      for line_idx = 0 to result_linenum - 1
      do
        for column = 0 to maxcol_idx
        do
          res.(line_idx).(column) <- matcharr.(line_idx).(column)
        done
      done;
      ()
    *)

  end










let evaluate_command_list cmdlst =
  let rec command commandlist tmpvar = match commandlist with
    | []        -> ()
    | cmd::tl   -> begin
                     match cmd with
                       | Get_url (url, referrer)  -> let document = Network.Curly.get url (Some referrer) in
                                                     begin
                                                       match document with
                                                         | Some doc -> command tl (Document doc)
                                                         | None -> raise No_document_found       
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
                                                             | Document doc -> doc
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


                       | Select selfunc             -> 
                                                       begin
                                                         match tmpvar with
                                                           | Match_result matchres -> command tl (Match_result (selfunc matchres))
                                                           | _           -> prerr_endline "Select: nothing to match"; raise No_Matchresult_available
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
                                                       print_endline "HSelect";
                                                       command tl tmpvar

                       (*   BOT READY, is Print-command so far !!! *)
                       | RowSelect   index            ->
                                                       let res = ref Empty in
                                                       print_endline "VSelect";
                                                       begin
                                                         match tmpvar with
                                                           | Match_result mres ->
                                                                                  begin
                                                                                  if index >= 0 && index < Array.length ( mres ) - 1
                                                                                  then
                                                                                    res := Result_selection ( mres.(index) )
                                                                                  else
                                                                                    begin
                                                                                      print_warning "VSelect: index clipped to 0";
                                                                                      res := Result_selection ( mres.(0) )
                                                                                    end
                                                                                  end
                                                           | _ -> print_warning "VSELECT: wrong type!!!"
                                                       end;
                                                       command tl !res

                       | Print                      ->
                                                       begin
                                                         match tmpvar with
                                                           | Document doc      -> print_endline doc
                                                           | Match_result mres -> Array.iter ( fun x -> Array.iter ( fun y -> Printf.printf "\"%s\" ||| " y) x;
                                                                                                        print_newline() ) mres
                                                           | Result_selection str_arr -> Array.iter ( fun str -> print_endline str; print_newline()) str_arr
                                                           | _ -> print_warning "Print-command found non-printable type"
                                                       end;
                                                       command tl tmpvar


                       | Print_match                -> (* prints "real" matches only (and not the fullmatch with index = 0) *)
                                                       begin
                                                         match tmpvar with
                                                           | Match_result mres -> let lines = Array.to_list mres in
                                                                      Array.iter ( fun x -> let matched_groups = List.tl (Array.to_list x) in
                                                                                            List.iter ( fun y -> Printf.printf "\"%s\" ||| " y) matched_groups;
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

                       | Showvar                    -> begin
                                                         match tmpvar with
                                                           | Document doc      -> print_endline "TMPVAR contains a document"
                                                           | Url_list url_list -> print_endline "TMPVAR contains an Url_list"
                                                           | Dummy_result      -> print_endline "TMPVAR contains Dummy_result"
                                                           | Match_result match_res -> print_endline "TMPVAR contains Match_result"
                                                           | Empty                  -> print_endline "TMPVAR contains EMPTY"
                                                       end;
                                                       command tl tmpvar
                   end


  in
    command cmdlst Empty







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

  let links = List.map ( fun mp4_arr ->  mp4_arr.(0)  )  mp4_urls in

  links



let ard_mediathek_get_rtmp_mp4_url_version_4 url =
  Printf.printf "NEU (probeweise Funktion fuer ARD) !!! url=%s\n" url;
  let commandlist = [ Get_url(url, "-");
                      Match( "(rtmpt{0,1}://[^\"]+).*?(mp4:[^\"]+)\"" );
                      (*
                      Select( Selector.select_first_match );
                      *)
                      Print_match;
                      Dummy
                    ]
  in
  evaluate_command_list commandlist;
  [""]






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



(* W D R *)
(* ===== *)

(* WDR:
Stringmatch auf:
  rtmp://gffstream.fcod.llnwd.net/a792/e2/mediendb/markt/video/2012/0227/120227_markt_web-m.mp4&amp;overlay

WDR:

http://www.wdr.de/tv/quarks/sendungsbeitraege/2009/1222/003_arena_mannfrau.jsp?startMedium=122037&startPicture=/tv/fsstd-technik/codebase/img/default_startbild.jpg&dslSrc=rtmp://gffstream.fcod.llnwd.net/a792/e2/tv/quarks/091229_das_eyetracking_experiment_big.flv&overlayPic=/tv/quarks/codebase/img/overlay_video.png&offset=0&red=fsstd-tv%2Fquarks&base=/tv/quarks/codebase/video/&isdnSrc=rtmp://gffstream.fcod.llnwd.net/a792/e2/tv/quarks/091229_das_eyetracking_experiment_small.flv

Stringmatch auf:
dslSrc=rtmp://gffstream.fcod.llnwd.net/a792/e2/tv/quarks/091229_das_eyetracking_experiment_big.flv&
*)
let wdr_mediathek_get_rtmp_mp4_url  url =
  let commandlist = [ Get_url(url, "-");
                      Match( "(rtmpt{0,1}://[^\"]+).*?(mp4:[^\"]+)\"" );
                      Select( Selector.select_first_match );
                      Print_match;
                      Dummy
                    ]
  in
  evaluate_command_list commandlist;
  [""]




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


(* Sender-spezifische URL-Grabber *)
(* ============================== *)
let zdf_mediathek_get_mmsurl    = web_asx_mms_get
let orf_mediathek_get_mmsurl    = web_asx_mms_get
let arte_mediathek_get_rtmp_url = arte_get
let ard_mediathek_get           = ard_mediathek_get_rtmp_mp4_url_version_4
let ndr_mediathek_get           = ndr_mediathek_get


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




let do_new_any_dl parserhash =
  let urls_from_argv = List.tl ( Array.to_list Sys.argv ) in
  List.iter ( fun url ->
                          let baseurl = Parsers.url_get_baseurl url in
                          try
                            let parserdef = Hashtbl.find parserhash baseurl in
                            print_endline "# --------------------";

                            evaluate_command_list ( Get_url(url, "-") :: parserdef.commands)
                               (*
                             let url_grabber = select_url_grabber_via_url url in

                             let video_urls =
                                              try url_grabber url
                                              with Mainurl_error | Asx_error | Stream_error | Not_found -> prerr_endline "dl-error occured"; []
                             in
                               List.iter print_endline video_urls
                               *)
                          with Not_found -> prerr_endline ("No parser found for " ^ url)
            ) urls_from_argv




let scriptname = "script.adl"

let read_parser_definitions filename_opt =
  print_endline "Starting experimental parser!  \n";
  Printf.printf "Scriptname: %s" scriptname;
    let tokenlist = ref [] in

    let input_channel = match filename_opt with None -> stdin | Some filename -> open_in filename in
    let lexer = Lexing.from_channel input_channel in
    begin
      try
        while true do
          prerr_string "$: "; flush_all(); (* prompt *)
          let result = Scriptparser.main Scriptlexer.read_command lexer in
          (*
          tokenlist := result :: !tokenlist
          List.rev_append result :: !tokenlist
          *)
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

    (* DEVEL/DEBBUG
    List.iter ( fun parserdef -> 
                                  Printf.printf "-------------------------\n";
                                  Printf.printf "Parsername: %s\n" parserdef.parsername;
                                  print_endline "matches on:";
                                  List.iter (fun url -> Printf.printf "\t%s\n" url) parserdef.urllist; print_newline();
                                  print_endline "Used commands\n";
                                  List.iter ( fun cmd -> Printf.printf "\t%s\n" (command_to_string cmd) ) parserdef.commands;
                                  print_newline()
              ) tokenlist;
    *)


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

    do_new_any_dl parserhash


    (* just in case I need the old stuff ;-)
    print_endline "Now OLD stuff";
    do_old_any_dl ()
    *)




(* --------------------------------------------------------------------------------------------------------------

  HOW TO DUMP STREAMS:
 ======================

rtmp / rtmpt:
  rtmpdump --resume  -r rtmp://.... -y mp4:....  -o outfile.ext

mms:
  mplayer -dumpstream mms://example.com/Globalplayers/GP_14.wmv -dumpfile ./download/test.wmv 

 ------------------------------------------------------------------------------------------------------------- *)
