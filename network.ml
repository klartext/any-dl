(*
  any-dl:
  -------
  Generic Media-Downloader for any kind of Online-Mediathek.

  Author / copyright: Oliver Bandel
  Copyleft:           GNU GENERAL PUBLIC LICENSE  v3 (or higher)
*)


type verbosity = Silent | Verbose | Very_verbose
let verbosity = ref Silent

let networking_verbosity = function
    | `Silent -> verbosity := Silent
    | `Verbose -> verbosity := Verbose
    | `Very_verbose -> verbosity := Very_verbose

(* user agent *)
let user_agent_ref = ref "any-dl"
let set_useragent agent_string = user_agent_ref := agent_string



module Simple =
  struct
    open Nethttp_client.Convenience

    let get  url = http_get url
    let head url = (http_head_message url) # response_header # fields
  end





module Cookies =
  struct
    open Nethttp

    (* ======================================== *)
    (* print the contents of a cookie to stdout *)
    (* ======================================== *)
    let print_cookie  cookie =
      let cookie_as_string = Tools.cookie_to_string cookie in
      print_endline cookie_as_string;
      flush_all()

      (* old format, as printed in curl-version *)
      (* -------------------------------------- *)
      (*
          =====> COOOKIES:
          --> winware.org FALSE   /de/    FALSE   1422655609      test    1
          <===== COOKIES
      *)


      (* ================================================== *)
      (* if very_verbose cli-flag is set, then print        *)
      (* the cookies, sorrounded by some eye catcher-text.  *)
      (* ================================================== *)
      let if_veryverbose_print_cookies cookies =
        if !verbosity = Very_verbose
        then
          begin
            print_endline "=*=*=> COOOKIES:";
              List.iter print_cookie cookies;
            print_endline "<=*=*= COOKIES";
            print_endline "<-------------------------------";
            flush_all()
          end


      (* If the server does not send DOMAIN- and PATH-fields, fill them from request-url *)
      (* =============================================================================== *)
      let fill_empty_cookiefields  url cookie =
        let open Neturl         in
        let open Parsers.Rebase in

          let syn    = common_syntax_of_url url in
          let neturl = parse_url ~accept_8bits:true ~enable_fragment:true  ~base_syntax:syn  url in

          (* a missing DOMAIN-field will be created from the request-url *)
          (* ----------------------------------------------------------- *)
          let dom = match cookie.cookie_domain  with
            | None     -> url_host neturl  (* host-part from url *)
            | Some dom -> dom              (* original from what the server sent *)
          in

          (* a missing PATH-field will be created from the request-url *)
          (* --------------------------------------------------------- *)
          let path_opt =
            match cookie.cookie_path    with
              | Some path -> Some path
              | None      ->  let pl = url_path neturl in
                              if List.length pl >= 2 then Some ( "/" ^ (List.nth pl 1) ^ "/" )
                              else None
          in
            { cookie with cookie_domain = Some dom; cookie_path = path_opt } (* updated record *)


      (* ======================================================== *)
      (* convert the cookie-forms function is missing in ocamlnet *)
      (* ======================================================== *)
      let cookie_to_cookie_ct  nscookie =
        let lst = ref [] in
        lst := (nscookie.cookie_name, nscookie.cookie_value) :: !lst;

        ignore( match nscookie.cookie_expires with None -> [] | Some flt  -> (("expires", string_of_float flt ) :: !lst) );
        ignore( match nscookie.cookie_domain  with None -> [] | Some dom  -> (("domain", dom ) :: !lst) );
        ignore( match nscookie.cookie_path    with None -> [] | Some path -> (("path", path ) :: !lst) );
        ignore( ("secure", string_of_bool nscookie.cookie_secure) :: !lst );
        !lst

  end





module Pipelined =
  struct
    open Cookies

    exception Get_error   of Nethttp_client.status (* error: can#t be solved   *)
    exception Get_problem of Nethttp_client.status (* problem: might be solved *)
    exception Download_error   of Nethttp_client.status (* error: can#t be solved   *)
    exception Download_problem of Nethttp_client.status (* problem: might be solved *)


      (* ========================================================================================================== *)
      (* this function judges/checks the status of a get-call and prints messages / raises exceptions, if necessary *)
      (* ========================================================================================================== *)
      let judge_getcall_status command_name status =
        match status with
           | `Client_error           -> prerr_endline "Get_error: Client_error";
                                        raise (Get_error `Client_error)

           | `Http_protocol_error  exc -> prerr_string "Get_error: Http_protocol_error";
                                          prerr_endline (Printexc.to_string exc);
                                          raise (Get_error (`Http_protocol_error exc))

           | `Redirection            -> if !verbosity = Verbose || !verbosity = Very_verbose then ( print_string command_name; print_endline "-Redirection" )

           | `Server_error           -> prerr_endline "Get_error: Server_error";
                                        raise (Get_error `Server_error)

           | `Successful             -> if !verbosity = Verbose || !verbosity  = Very_verbose then ( print_string command_name; print_endline "-Successful" )

           | `Unserved               -> prerr_endline "Get_problem: Unserved";
                                        raise (Get_problem `Unserved)


      (* ============================================================================= *)
      (* set the response-body-storage to write to a file, if Some <filename> is given *)
      (* ============================================================================= *)
      let set_response_body_storage_of_call  call  opt_filename =
          match opt_filename with
            | None               -> call # set_response_body_storage `Memory
            | Some dest_filename -> call # set_response_body_storage (`File ( fun f -> dest_filename ))


      (* =================================== *)
      (* set the USER-AGENT string of a call *)
      (* =================================== *)
      let set_useragent_of_call  call  useragent_string =
        Nethttp.Header.set_user_agent (call # request_header `Base) useragent_string


      (* ================================ *)
      (* set the REFERER string of a call *)
      (* ================================ *)
      let set_referrer_of_call call opt_referer =
          match opt_referer with
            | None     -> ()
            | Some ref -> Nethttp.Header.set_referer (call # request_header `Base) ref


      (* ================================ *)
      let set_cookies_of_call  call opt_cookies =
          match opt_cookies with
            | None      ->  ()
            | Some cook ->  let cookies_ct = List.map cookie_to_cookie_ct cook in
                            List.iter ( fun cookie -> Nethttp.Header.set_cookie (call # request_header `Base) cookie ) cookies_ct

      (* ==================================================================================== *)
      (* This function allows GET as well as POST,                                            *)
      (* and the data can be loaded to MEM as well as saved do file (DOWNLOAD)                *)
      (* ------------------------------------------------------------------------------------ *)
      (* To select GET,  opt_postdata is set to None.                                         *)
      (* To select POST, opt_postdata is set to Some (post_data).                             *)
      (*                                                                                      *)
      (* To load the data to memory, set opt_outfilename to None.                             *)
      (* To save the data to a file, set opt_outfilename to Some (filename).                  *)
      (* ==================================================================================== *)
      let get_or_post_to_mem_or_file_raw url ?user_agent:(user_agent = !user_agent_ref) (referer: string option) opt_postdata cookies opt_outfilename =

        let cmd_string      = match opt_postdata, opt_outfilename with
          | None,   None   -> "GET  (MEM)"
          | None,   Some _ -> "GET  (DOWNLOAD)"
          | Some _, None   -> "POST (MEM)"
          | Some _, Some _ -> "POST (DOWNLOAD)"
        in

        if !verbosity = Verbose || !verbosity  = Very_verbose then
        begin
          print_endline "------------------------------->";
          Printf.printf "%s URL: %s\n" cmd_string url;
          flush_all()
        end;
        let url = Parsers.Rebase.remove_fragment_from_url url in


        (* the Pipeline to run the call *)
        (* ---------------------------- *)
        let pipeline = new Nethttp_client.pipeline in


        (* the call-object - it can either be an object for GET or for POST *)
        (* ---------------------------------------------------------------- *)
        let call_obj = match opt_postdata with
          | None           -> new Nethttp_client.get  url            (* Referrer? Cookies? Will be set below. *)
          | Some post_data -> new Nethttp_client.post url post_data  (* Referrer? Cookies? Will be set below. *)
        in


        set_response_body_storage_of_call call_obj opt_outfilename; (* If there is Some outfilename (for download), then set it as set_response_body_storage *)

        set_useragent_of_call call_obj user_agent;      (* set the USER-AGENT string *)

        set_referrer_of_call call_obj referer;                      (* set the REFERER string *)

        set_cookies_of_call call_obj cookies;                       (* set the Cookies *)


        (* Get the data from webserver now *)
        (* =============================== *)
        pipeline # add call_obj;  (* add the get-call to the pipeline *)
        pipeline # run();         (* process the pipeline (retrieve data) *)


        (* verbosity-message with status-infos for call-object *)
        (* --------------------------------------------------- *)
        if !verbosity = Verbose || !verbosity  = Very_verbose then
        begin
          Printf.printf "Status-Code    %s:  %d\n" cmd_string (call_obj # response_status_code);
          Printf.printf "Status-Message %s:  %s\n" cmd_string (call_obj # response_status_text);
          flush_all()
        end;

        (* check status (throws exceptions if not `Success) *)
        (* ------------------------------------------------ *)
        judge_getcall_status cmd_string ( call_obj # status );


        let cookies = Nethttp.Header.get_set_cookie  (call_obj # response_header) in

        if_veryverbose_print_cookies cookies;

        match opt_outfilename with
          | None   -> ( Some (call_obj # response_body # value) , Some cookies )
          | Some _ -> (None, Some cookies)

      let get_or_post_to_mem_or_file url (referer: string option) opt_postdata cookies opt_outfilename =
        try
          get_or_post_to_mem_or_file_raw url (referer: string option) opt_postdata cookies opt_outfilename
        with Failure msg -> Printf.eprintf "Error caught (get_or_post_to_mem_or_file_raw) (url:%s) (message: %s)\n" url msg; (None, None)


      (* ============================================================================ *)
      (* get: get a document from a webserver with http-get, into memory              *)
      (* ============================================================================ *)
      let get url (referer: string option) cookies =
        match ( get_or_post_to_mem_or_file url referer None cookies None ) with
          | Some body, Some cookies -> Some ( body, cookies )
          | _, _                    -> None


      (* ============================================================================ *)
      (* download: get a document from a webserver with http-get,                     *)
      (* save it directly to disk.                                                    *)
      (* ============================================================================ *)
      let get_download url (referer: string option) cookies dest_filename =
        match ( get_or_post_to_mem_or_file url referer None cookies (Some dest_filename) ) with
          | _, cookies_opt -> cookies_opt


      (* ============================================================================ *)
      (* post: get a document from a webserver with http-post, into memory            *)
      (* ============================================================================ *)
      let post url (referer: string option) post_data cookies =
        match ( get_or_post_to_mem_or_file url referer (Some post_data) cookies None ) with
          | Some body, Some cookies -> Some ( body, cookies )
          | _, _                    -> None


      (* ============================================================================ *)
      (* download: get a document from a webserver with http-post,                    *)
      (* save it directly to disk.                                                    *)
      (* ============================================================================ *)
      let post_download url (referer: string option) post_data cookies dest_filename =
        match ( get_or_post_to_mem_or_file url referer (Some post_data) cookies (Some dest_filename) ) with
          | _, cookies_opt -> cookies_opt



  end


