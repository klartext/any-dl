(*
  any-dl:
  -------
  Generic Media-Downloader for any kind of Online-Mediathek.

  Author / copyright: Oliver Bandel
  Copyleft:           GNU GENERAL PUBLIC LICENSE  v3 (or higher)
*)

type opt_t = {
               mutable parser_selection :  string option; (*  -p: select a parser deliberately *)
               mutable list_parsers     :  bool;          (*  -l: print out the list of parsers *)
               mutable rc_filenames     :  string list;   (*  -f: select a rc-filename different to default (add one to a list) *)
               mutable url_list         :  string list;   (* ./.: list of urls will be stored here *)
               mutable show_commands    :  bool;          (*  -c: show all commands of the parser definition language *)
               mutable verbose          :  bool;          (*  -v: print some more details (be verbose) *)
               mutable safe             :  bool;          (*  -s: safe behaviour: no download via system invoked *)
               mutable interactive      :  bool;          (*  -i: interactive features enabled (e.g. iselectmatch() *)
               mutable auto_try         :  bool;          (*  -a: auto-try: automatically try all parsers *)
               mutable auto_try_stop    :  bool;          (* -as: auto-try: automatically try all parsers; after success STOP *)
               mutable user_agent       :  string;        (*  -u: user-agent: set name of the user-agent string *)
               mutable initial_referrer :  string;        (* -ir: initial referrer-string, set before any get-action *)
               mutable ms_sleep         :  int;           (* -ms: milliseconds sleep after a get *)
               mutable sep              :  string;        (* -sep: seperator between different parser-calls *)
             }



(* the DEFAULT-settings for the program *)
(* ------------------------------------ *)
let opt =  {
             parser_selection = None;
             list_parsers     = false;
             rc_filenames     = [];
             (*
             rc_filenames     = Filename.concat (Sys.getenv "HOME") (".any-dl.rc");
             *)
             url_list         = [];
             show_commands    = false;
             verbose          = false;
             safe             = false;
             interactive      = false;
             auto_try         = false;
             auto_try_stop    = false;
             user_agent       = "any-dl";
             initial_referrer = "-";
             ms_sleep         = 0;
             sep              = "# --------------------\n"
           }



(* parse(): function to parse the command line *)
(* ------------------------------------------- *)
let parse () = 
    Arg.parse [
         ("-p",   Arg.String (fun parser_name -> opt.parser_selection <- Some parser_name ),  "    select a mandatory parser by name (give name here)" );
         ("-l",   Arg.Unit   (fun ()          -> opt.list_parsers     <- true ),              "    list parsers" );
         ("-f",   Arg.String (fun rcfilename  -> opt.rc_filenames  <- rcfilename :: opt.rc_filenames ),        "    rc-file-name" );
         ("-c",   Arg.Unit   (fun ()          -> opt.show_commands    <- true       ),        "    show commands of parserdef-language" );
         ("-v",   Arg.Unit   (fun ()          -> opt.verbose          <- true       ),        "    verbose     " );
         ("-s",   Arg.Unit   (fun ()          -> opt.safe             <- true       ),        "    safe: no download via system invoked" );
         ("-i",   Arg.Unit   (fun ()          -> opt.interactive      <- true       ),        "    interactive: interactive features enabled" );
         ("-a",   Arg.Unit   (fun ()          -> opt.auto_try         <- true       ),        "    auto-try: try all parsers" );
         ("-as",  Arg.Unit   (fun ()          -> opt.auto_try         <- true;
                                                 opt.auto_try_stop    <- true; prerr_endline "!!!"       ),        "   auto-try-stop: try all parsers; stop after first success" );
         ("-u",   Arg.String (fun useragent   -> opt.user_agent       <- useragent ),         "    set the user-agent-string manually" );
         ("-ir",  Arg.String (fun init_ref    -> opt.initial_referrer <- init_ref ),          "    set the initial referrer from '-' to custom value " );
         ("-ms",  Arg.Int    (fun sleep_ms    -> opt.ms_sleep <- sleep_ms ),                  "    set a sleep-time in a (bulk-) get-command in milli-seconds" );
         ("-sep", Arg.String (fun sep         -> opt.sep <- sep ),                            "    set seperator-string, which is printed between parser-calls" );
       (* => DEFAULT (hardcoded) !!!
       *)
              ]
         ( fun str -> opt.url_list <- str :: opt.url_list  )
         "Use \"any-dl\" following options:"


