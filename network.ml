(*
  any-dl:
  -------
  Generic Media-Downloader for any kind of Online-Mediathek.

  Author / copyright: Oliver Bandel
  Copyleft:           GNU GENERAL PUBLIC LICENSE  v3 (or higher)
*)



module Simple =
  struct
    open Nethttp_client.Convenience

    let get  url = http_get url
    let head url = (http_head_message url) # response_header # fields
  end



(* type http_options = { ....

      number_of_parallel_connections : int;   (*  The client keeps up to this
      number of parallel connections to a single content server or proxy. Default: 2
      You may increase this value if you are mainly connected with an HTTP/1.0 proxy. *)

      }

====================== OPTIONS ======================
Options for the whole pipeline. It is recommended to change options the following way:

    let opts = pipeline # get_options in
    let new_opts = { opts with <field> = <value>; ... } in
    pipeline # set_options new_opts
 
============================================


Printf.printf "Status-Message GET:  %s\n" get_call#response_status_text;
  
  (* GET-call *)
  (* ======== *)
  (*
  let body = get_call # response_body # value  in
  let response_header = get_call # response_header # fields in
  *)


*)




module Pipelined =
  struct
    open Nethttp

    exception Get_error   of Nethttp_client.status (* error: can#t be solved   *)
    exception Get_problem of Nethttp_client.status (* problem: might be solved *)
    exception Download_error   of Nethttp_client.status (* error: can#t be solved   *)
    exception Download_problem of Nethttp_client.status (* problem: might be solved *)

    (* ======================================== *)
    (* print the contents of a cookie to stdout *)
    (* ======================================== *)
    let print_cookie  cookie =
      Printf.printf "    cookie-name:    %s\n" cookie.cookie_name;
      Printf.printf "    cookie-value:   %s\n" cookie.cookie_value;
      (match cookie.cookie_expires with None -> () | Some ex   -> Printf.printf "    cookie-expires: %f\n" ex);
      (match cookie.cookie_domain  with None -> () | Some dom  -> Printf.printf "    cookie-domain:  %s\n" dom);
      (match cookie.cookie_path    with None -> () | Some path -> Printf.printf "    cookie-path:    %s\n" path);
      Printf.printf "    cookie-secure:  %s\n" (if cookie.cookie_secure then "TRUE" else "FALSE");
      print_endline "  ------";
      ()

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
        if Cli.opt.Cli.very_verbose
        then
          begin
            print_endline "=*=*=> COOOKIES:";
              List.iter print_cookie cookies;
            print_endline "<=*=*= COOKIES";
            print_endline "<-------------------------------"
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


      (* ========================================================================================================== *)
      (* this function judges/checks the status of a get-call and prints messages / raises exceptions, if necessary *)
      (* ========================================================================================================== *)
      let judge_getcall_status status =
        match status with
           | `Client_error           -> prerr_endline "Client_error";
                                        raise (Get_error `Client_error)

           | `Http_protocol_error  _ -> prerr_endline "Http_protocol_error";
                                        raise (Get_error `Client_error)

           | `Redirection            -> prerr_endline "Redirection";
                                        raise (Get_problem `Client_error)

           | `Server_error           -> prerr_endline "Server_error";
                                        raise (Get_error `Client_error)

           | `Successful             -> if Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose then print_endline "GET-Successful"

           | `Unserved               -> prerr_endline "Unserved";
                                        raise (Get_problem `Client_error)


      (* ==================================================================================== *)
      (* This function allows "raw get" (no error-wrappers, as were necessary with curl-libs) *)
      (* as well as "download" (get with directly writing the data to disk                    *)
      (* ------------------------------------------------------------------------------------ *)
      (* the real get_raw- and download-functions can call this function and pick out the     *)
      (* data that is expected.                                                               *)
      (* ==================================================================================== *)
      let get_raw_or_download url (referer: string option) cookies opt_outfilename =

        let verbose_message = match opt_outfilename with None -> "get_raw: GET URL" | Some _ -> "get_raw: GET (DOWNLOAD) URL" in

        if Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose then
        begin
          print_endline "------------------------------->";
          Printf.printf "%s: %s\n" verbose_message url;
        end;

        let pipeline = new Nethttp_client.pipeline in

        let get_call  = new Nethttp_client.get url in (* Referrer? Cookies? *)

        (* If there is Some outfilename (for download), then set it as set_response_body_storage *)
        (* ------------------------------------------------------------------------------------- *)
        begin
          match opt_outfilename with
            | None               -> get_call # set_response_body_storage `Memory
            | Some dest_filename -> get_call # set_response_body_storage (`File ( fun f -> dest_filename ))
        end;

        (* set the USER-AGENT string *)
        (* ------------------------- *)
        Nethttp.Header.set_user_agent (get_call # request_header `Base) Cli.opt.Cli.user_agent;

        (* set the REFERER string *)
        (* ---------------------- *)
        begin
          match referer with None -> () | Some ref -> Nethttp.Header.set_referer (get_call # request_header `Base) ref
        end;

        (* set the Cookies *)
        (* --------------- *)
        begin
          match cookies with
            | None      ->  ()
            | Some cook ->  let c = List.map cookie_to_cookie_ct cook in
                            List.iter ( fun xx -> Nethttp.Header.set_cookie (get_call # request_header `Base) xx) c
        end;


        (* Get the data from webserver now *)
        (* =============================== *)
        pipeline # add get_call;  (* add the get-call to the pipeline *)
        pipeline # run();         (* process the pipeline (retrieve data) *)

        (* check status *)
        (* ------------ *)
        judge_getcall_status ( get_call # status );

        if Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose then
        begin
          Printf.printf "Status-Code    GET:  %d\n" get_call # response_status_code;
          Printf.printf "Status-Message GET:  %s\n" get_call # response_status_text
        end;

        let cookies = Nethttp.Header.get_set_cookie  (get_call # response_header) in

        if_veryverbose_print_cookies cookies;

        ( Some (get_call # response_body # value) , Some cookies )



      (* ============================================================================ *)
      (* get_raw: get a document from a webserver, into memory                        *)
      (* ---------------------------------------------------------------------------- *)
      (* The "raw" means: purely the http-stuff, without wrapper for error reecovery. *)
      (* OCamlNet does auto-retry by default, and the results are nice. ocaml-curl    *)
      (* yielded in many connection-errors and error-handling-wrappers were needed    *)
      (* (unraw). Possibly changing name of this function makes sense later.          *)
      (* ============================================================================ *)
      let get_raw url (referer: string option) cookies =
        match ( get_raw_or_download url referer cookies None ) with
          | Some body, Some cookies -> Some ( body, cookies )
          | _, _                    -> None



      (* ============================================================================ *)
      (* download: get a document from a webserver, save it directly to disk.         *)
      (* ============================================================================ *)
      let download url (referer: string option) cookies dest_filename =
        match ( get_raw_or_download url referer cookies (Some dest_filename) ) with
          | _, cookies_opt -> cookies_opt



  end


