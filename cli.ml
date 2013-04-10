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
               mutable rc_filename      :  string;        (*  -f: select a rc-filename different to default *)
               mutable url_list         :  string list;   (* ./.: list of urls will be stored here *)
               mutable show_commands    :  bool;          (*  -c: show all commands of the parser definition language *)
               mutable verbose          :  bool;          (*  -v: print some more details (be verbose) *)
               mutable safe             :  bool;          (*  -s: safe behaviour: no download via system invoked *)
               mutable interactive      :  bool;          (*  -i: interactive features enabled (e.g. iselectmatch() *)
               mutable auto_try         :  bool;          (*  -a: auto-try: automatically try all parsers *)
               (*
               mutable user_agent       :  string;        (*  -u: user-agent: set name of the user-agent string *)
               *)
             }



(* the DEFAULT-settings for the program *)
(* ------------------------------------ *)
let opt =  {
             parser_selection = None;
             list_parsers     = false;
             rc_filename      = Filename.concat (Sys.getenv "HOME") (".any-dl.rc");
             url_list         = [];
             show_commands    = false;
             verbose          = false;
             safe             = false;
             interactive      = false;
             auto_try         = false
             (*
             user_agent       = "any-dl"
             *)
           }



(* parse(): function to parse the command line *)
(* ------------------------------------------- *)
let parse () = 
    Arg.parse [
         ("-p",   Arg.String (fun parser_name -> opt.parser_selection <- Some parser_name ),  "    select a mandatory parser by name (give name here)" );
         ("-l",   Arg.Unit   (fun ()          -> opt.list_parsers     <- true ),              "    list parsers" );
         ("-f",   Arg.String (fun rcfilename  -> opt.rc_filename      <- rcfilename ),        "    rc-file-name" );
         ("-c",   Arg.Unit   (fun ()          -> opt.show_commands    <- true       ),        "    show commands of parserdef-language" );
         ("-v",   Arg.Unit   (fun ()          -> opt.verbose          <- true       ),        "    verbose     " );
         ("-s",   Arg.Unit   (fun ()          -> opt.safe             <- true       ),        "    safe: no download via system invoked" );
         ("-i",   Arg.Unit   (fun ()          -> opt.interactive      <- true       ),        "    interactive: interactive features enabled" );
         ("-a",   Arg.Unit   (fun ()          -> opt.auto_try         <- true       ),        "    auto-try: try all parsers" );
         (*
         ("-u",   Arg.String (fun useragent -> opt.user_agent <- Some useragent ),  "    set the user-agent-string manually" );
         *)
       (* => DEFAULT (hardcoded) !!!
       *)
              ]
         ( fun str -> opt.url_list <- str :: opt.url_list  )
         "Use \"any-dl\" following options:"


