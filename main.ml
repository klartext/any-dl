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
open Tools


module E = Evaluate



exception AutoTry_success               (* in auto-try mode (switch -a), if successful, this exception will be thrown *)
exception No_parser_found_for_this_url  (* *)
exception Unknown_parser  (* if a parsername is requested, which does not exist *)







(* ============================================ *)
(* read the parser-definitions from files.      *)
(* -------------------------------------------- *)
(* the filenames are given as list of filenames *)
(* ============================================ *)
let parse_parser_definitions_from_files filenames_list =

  let tokenlist = ref [] in

  (*
  Parsing.set_trace true; (* only for debugging purposes *)
  *)
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
                                                             Printf.eprintf "Parse error in file \"%s\", line %4d\n" filename !Scriptlexer.linenum;
                                                             exit 1

                                     (*
                                     | Not_found -> prerr_string "Variable not known in line ";
                                                    prerr_int !Scriptlex.linenum;prerr_newline()
                                                    (*
                                                    exit 1
                                                    *)
                                     *)

                                end;
                                close_in input_channel;
                                Scriptlexer.linenum := 1 (* reset the linenumber for the next rc-file *)
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
                  with Invalid_argument _ -> aux tl (* this happens if url is shorter than parser_url *)
  in
    aux lookup_lst



(* ============================================================ *)
(* Looks up the right parser, either via CLI-given parsername   *)
(* or via hash-lookup (url from parser-definition) for that url *)
(* ============================================================ *)
let get_parserdef url parser_urllist parser_namehash  parser_selection =
      try
        begin
          match parser_selection with
            | Some parsername ->
                                 begin
                                 try
                                   Hashtbl.find parser_namehash parsername
                                 with Not_found -> prerr_endline ("Unknown parser " ^ parsername);
                                                   raise Unknown_parser
                                 end

            | None            -> (* parsername looked up via from url *)

                                 (* comparing the url with the strings in the url-parsername-assoc-list *)
                                 (* ------------------------------------------------------------------- *)
                                 let parsername = parsername_lookup_by_url  url  parser_urllist in
                                 verbose_printf "*** selected parser: %s\n" parsername;
                                 Hashtbl.find parser_namehash  parsername
        end
      with Not_found         -> prerr_endline ("No parser found for " ^ url); raise No_parser_found_for_this_url





(* =============================================================================== *)
(* This function looks up the right parser for a given url and invokes that parser *)
(* =============================================================================== *)

let invoke_parser_on_url  url  parser_urllist  parser_namehash  parser_selection (macros_list : macrodef_t list ) =

  (* select the parserdef from command line or urls that are attached to the parserdefinitions *)
  let parserdef = get_parserdef url  parser_urllist  parser_namehash  parser_selection in

  try
    let seperator_string = Parsers.activate_controlstrings Cli.opt.Cli.sep in
    print_string seperator_string; (* print seperator before (between) the parser-calls on urls *)

    (* ---------------------------------------------------------------- *)
    (* we evaluate the parse-tree, and start with a first, implicit get *)
    (* with the url we got from the command line                        *)
    (* ---------------------------------------------------------------- *)
    ignore(
      E.evaluate_statement_list ( Command ( Setvar(Url(url, Cli.opt.Cli.initial_referrer)) ) ::
                                  Command ( Store "STARTURL" ) ::
                                  Command ( Get_url(url, Cli.opt.Cli.initial_referrer) ) ::
                                  Command ( Store("BASEDOC") ) ::
                                  parserdef.statements ) macros_list
          )

  with (* handle exceptions from the parse-tree-evaluation *)
    | E.No_Match                -> Printf.eprintf "Parser problem: Could not match to pattern!\t Parse will be exited for url %s\n" url; flush stderr
    | E.Invalid_Row_Index       -> Printf.eprintf "Error in script! Invalid_Row_Index!\t Parse exited.\n"; flush stderr
    | E.Variable_not_found name -> Printf.eprintf "Variable_not_found: \"%s\"\t This parse exited.\n" name; flush stderr
    | E.No_document_found       -> Printf.eprintf "No_document_found for URL %s\n" url; flush stderr
    | E.Tagselect_empty_list    -> Printf.eprintf "Tagselect_empty_list for URL %s\n" url; flush stderr
    | E.Parse_exit              -> Printf.eprintf "Parser exited via exitparse-command\n"; flush stderr
    | E.Csv_read_error     msg  -> Printf.eprintf "Parser exited by Csv_read_error: \"%s\" ( URL: %s )\n" msg url; flush stderr
    | Network.Pipelined.Get_error   status -> Printf.eprintf "Parser abandoned, because of Get_error ( URL: %s )\n" url ; flush stderr
    | Network.Pipelined.Get_problem status -> Printf.eprintf "Parser abandoned, because of Get_problem ( URL: %s )\n" url ; flush stderr




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
    if Cli.opt.Cli.show_commands || Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose then
      begin
        print_endline "Keywords of the parser-definition language:";
        print_endline "-------------------------------------------";
        let kwlist = Hashtbl.fold (fun key value sofar -> key :: sofar ) Scriptlexer.commands_table [] in
        List.iter ( fun kw -> Printf.printf "keyword   %s\n" kw) (List.sort compare kwlist)
      end;
      flush stdout;


    (* parse the parser-definitions *)
    (* ---------------------------- *)
    if Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose then
    begin
      print_string "rc-filename(s): ";
      List.iter ( fun str -> Printf.printf "\"%s\" " str ) Cli.opt.Cli.rc_filenames;
      print_newline()
    end;

    (* read definitions (of parsers and macros) from rc-file *)
    (* ----------------------------------------------------- *)
    let definitions_list = parse_parser_definitions_from_files Cli.opt.Cli.rc_filenames in


    let parser_list = List.fold_right ( fun def sofar -> match def with Parserdef pdef -> pdef :: sofar | _ -> sofar ) definitions_list [] in
    let parser_list = List.rev parser_list in (* as long as no other order is pushed later, let order as is read from file *)

    let (macro_list : macrodef_t list) = List.fold_right ( fun def sofar -> match def with Macrodef mdef -> mdef :: sofar | _ -> sofar ) definitions_list [] in


    (* if cli-switches ask for it, print the number of parser-defintions found *)
    (* ------------------------------------------------------------------------------------ *)
    if Cli.opt.Cli.list_parsers || Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose then
      Printf.fprintf stdout "Number of found parser definitions: %d\n" (List.length parser_list);


    (* create and initialize hashes for parser-lookup by name / url *)
    (* ------------------------------------------------------------ *)
    let parser_namehash = Hashtbl.create (List.length parser_list) in
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
                                                          if Cli.opt.Cli.list_parsers || Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose
                                                          then
                                                            Printf.fprintf stdout "Init: bound Base-URL %-30s -> parser %s\n" url parserdef.parsername

                                             ) parserdef.urllist;
                                 end
                                 else
                                 begin
                                    (* If CLI-switches ask for it, print the parser's-name, mentioning that no url is bound to it *)
                                    (* ------------------------------------------------------------------------------------------ *)
                                    if Cli.opt.Cli.list_parsers || Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose then
                                      Printf.fprintf stdout "Init: (unbound to URL)%-30s-> parser %s\n"   ""  parserdef.parsername
                                 end

              ) parser_list;

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


    (* for all distinct URLs from the command line, do the intended work :-) *)
    (* ===================================================================== *)

    (* first we kick out urls that are mentioned more than once on thecommand line, *)
    (* and bring the list into the right order.                                     *)
    (* ---------------------------------------------------------------------------- *)
    let list_of_urls = List.rev (Tools.add_item_once Cli.opt.Cli.url_list) in

    if Cli.opt.Cli.auto_try
    then
      begin
        prerr_endline "option auto-try: would need to invoke all parsers now...";

        let parsernames = Hashtbl.fold ( fun k v sofar -> k :: sofar) parser_namehash [] in
        (* for each url try the work *)
        (* ------------------------- *)
        List.iter ( fun url ->
                               (* for this url try all parsers *)
                               
                               try
                                 List.iter ( fun parsername -> prerr_endline ("========================> Parser: " ^ parsername ^ " <========================");
                                                               try
                                                                 invoke_parser_on_url  url  parser_urllist  parser_namehash  (Some parsername) macro_list;
                                                                 if Cli.opt.Cli.auto_try_stop then raise AutoTry_success
                                                               with
                                                                 | AutoTry_success -> raise AutoTry_success
                                                                 | _               -> prerr_endline "Parser failed with exception!" (* eats exception *)
                                           ) parsernames
                               with AutoTry_success -> prerr_endline "Parser succeeded." (* catch only a success; any other exceptions igonre here *)

                  ) list_of_urls
      end
    else (* non-auto (normal mode) *)
      List.iter ( fun url -> invoke_parser_on_url  url  parser_urllist  parser_namehash  Cli.opt.Cli.parser_selection macro_list ) list_of_urls


let _ =

  (* Initialization *)
  (* ============== *)
  Nettls_gnutls.init(); (* this is needed for https-support via gnutls-lib *)

  try
    main()
  with Sys_error msg -> if Pcre.pmatch ~pat:".any-dl.rc: No such file or directory" msg
                        then
                          begin
                            Printf.fprintf stderr "The config file is missing. Possible default places for it:\n";
                            Printf.fprintf stderr "$XDG_CONFIG_HOME/any-dl.rc or $HOME/.any-dl.rc or /etc/any-dl.rc\n";
                            Printf.fprintf stderr "Please provide a rc-file there or use -f optia rc-file, if you want to use a different rc-file.\n"
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

rtsp:
  cvlc  rtsp://....foobar.mp4 --sout=file/ts:foobar.mp4
 ------------------------------------------------------------------------------------------------------------- *)
