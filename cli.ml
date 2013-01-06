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
               mutable verbose          :  bool;          (*  -v: print some more details (be verbose) *)
             }



(* the DEFAULT-settings for the program *)
(* ------------------------------------ *)
let opt =  {
             parser_selection = None;
             list_parsers     = false;
             rc_filename      = Filename.concat (Sys.getenv "HOME") (".any-dl.rc");
             url_list         = [];
             verbose          = false
           }



(* parse(): function to parse the command line *)
(* ------------------------------------------- *)
let parse () = 
    Arg.parse [
         ("-p",   Arg.String (fun parser_name -> opt.parser_selection <- Some parser_name ),  "    select a mandatory parser by name (give name here)" );
         ("-l",   Arg.Unit   (fun ()          -> opt.list_parsers     <- true ),              "    list parsers" );
         ("-f",   Arg.String (fun rcfilename  -> opt.rc_filename      <- rcfilename ),        "    rc-file-name" );
         ("-v",   Arg.Unit   (fun ()          -> opt.verbose          <- true       ),        "    verbose     " );
       (* => DEFAULT (hardcoded) !!!
       *)
              ]
         ( fun str -> opt.url_list <- str :: opt.url_list  )
         "Use \"any-dl\" following options:"


