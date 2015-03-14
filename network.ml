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

    let get url = http_get url
  end



(* type http_options = { ....

      number_of_parallel_connections : int;   (*  The client keeps up to this
      number of parallel connections to a single content server or proxy. Default: 2
      You may increase this value if you are mainly connected with an HTTP/1.0 proxy. *)

      }

*)




module Pipelined =
  struct
    open Nethttp
    (*
    open Nethttp_client
    *)


    exception Get_error   of Nethttp_client.status (* error: can#t be solved   *)
    exception Get_problem of Nethttp_client.status (* problem: might be solved *)
    exception Download_error   of Nethttp_client.status (* error: can#t be solved   *)
    exception Download_problem of Nethttp_client.status (* problem: might be solved *)

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



      (*if the server does not send DOMAIN- and PATH-fields, fill them from request-url *)
      (* ------------------------------------------------------------------------------ *)
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


      (* =============== *)
      let cookie_to_cookie_ct  nscookie =
        let lst = ref [] in
        lst := (nscookie.cookie_name, nscookie.cookie_value) :: !lst;

        ignore( match nscookie.cookie_expires with None -> [] | Some flt  -> (("expires", string_of_float flt ) :: !lst) );
        ignore( match nscookie.cookie_domain  with None -> [] | Some dom  -> (("domain", dom ) :: !lst) );
        ignore( match nscookie.cookie_path    with None -> [] | Some path -> (("path", path ) :: !lst) );
        ignore( ("secure", string_of_bool nscookie.cookie_secure) :: !lst );
        !lst



      (* ==================================================== *)
      let get_raw url (referer: string option) cookies =

        if Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose then
        begin
          print_endline "------------------------------->";
          Printf.printf "get_raw: GET URL: %s\n" url;
        end;

        let pipeline = new Nethttp_client.pipeline in

        let get_call  = new Nethttp_client.get url in (* Referrer? Cookies? *)

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


(*
 Curl-Lib:  conn#set_sslverifypeer false; (* Zertifikate-Prüfung auschalten! *)
*)

      
        (* Get the data from webserver now *)
        (* =============================== *)
        pipeline # add get_call;  (* add the get-call to the pipeline *)
        pipeline # run();         (* process the pipeline (retrieve data) *)

        (* check status *)
        (* ------------ *)
        begin
          match get_call # status with
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
        end;

        if Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose then
        begin
          Printf.printf "Status-Code    GET:  %d\n" get_call # response_status_code;
          Printf.printf "Status-Message GET:  %s\n" get_call # response_status_text
        end;

        let cookies = Nethttp.Header.get_set_cookie  (get_call # response_header) in

        if Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose
        then
          begin
            print_endline "=*=*=> COOOKIES:";
              List.iter print_cookie cookies;
            print_endline "<=*=*= COOKIES";
            print_endline "<-------------------------------"
          end;

        Some ( get_call # response_body # value, cookies )



      (* ==================================================== *)
      let download url (referer: string option) cookies dest_filename =
        let pipeline = new Nethttp_client.pipeline in

        let get_call  = new Nethttp_client.get url in (* Referrer? Cookies? *)

        get_call # set_response_body_storage (`File ( fun f -> dest_filename )); (*!*)

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


(*
 Curl-Lib:  conn#set_sslverifypeer false; (* Zertifikate-Prüfung auschalten! *)
*)

      
        (* Get the data from webserver now *)
        (* =============================== *)
        pipeline # add get_call;  (* add the get-call to the pipeline *)
        pipeline # run();         (* process the pipeline (retrieve data) *)

        (* check status *)
        (* ------------ *)
        begin
          match get_call # status with
             | `Client_error           -> prerr_endline "Client_error";
                                          raise (Download_error `Client_error)

             | `Http_protocol_error  _ -> prerr_endline "Http_protocol_error";
                                          raise (Download_error `Client_error)

             | `Redirection            -> prerr_endline "Redirection";
                                          raise (Download_problem `Client_error)

             | `Server_error           -> prerr_endline "Server_error";
                                          raise (Download_error `Client_error)

             | `Successful             -> if Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose then prerr_endline "GET-Successful"

             | `Unserved               -> prerr_endline "Unserved";
                                          raise (Download_problem `Client_error)
        end;

        if Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose then
        begin
          Printf.eprintf "Status-Code    GET (DOWNLOAD):  %d\n" get_call # response_status_code;
          Printf.eprintf "Status-Message GET (DOWNLOAD):  %s\n" get_call # response_status_text
        end;

        let cookies = Nethttp.Header.get_set_cookie  (get_call # response_header) in

        if Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose
        then
          begin
            print_endline "------------------------------------------";
            print_endline "=*=*=> COOOKIES:";
              List.iter print_cookie cookies;
            print_endline "<=*=*= COOKIES";
          end;

        Some ( cookies )




(*
            (* Ergebnis-Auswertung *)
            (* ------------------- *)
            let http_code = conn#get_responsecode in

            (* if http-code is less than 400, give back result; else print a msg and give back None *)
            (* ------------------------------------------------------------------------------------ *)
            if http_code < 400
            then
              (* no http-error *)
              let cookies = conn#get_cookielist in
              begin
                if Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose
                then
                  begin
                    print_endline "=====> COOOKIES:";
                    List.iter (fun s -> print_string "--> "; print_endline s) cookies;
                    print_endline "<===== COOKIES"
                  end;
                  conn#cleanup; (* CLEAN UP CONNECTION *)

                  (* give back result *)
                  (* ---------------- *)
                  let result = Buffer.contents buffer in Some (result, cookies)
              end

            (* http-error-case *)
            else
              begin
                if Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose then Printf.eprintf "http-returncode: %d (URL: %s)\n" http_code url;
                None
              end
*)

(*
    let get url referer cookies =
      verbose_printf "GET %s\n" url; flush stdout;

      let trial_numbers = 3 in
      let rec get_aux num =
        if num <= trial_numbers
        then
          try get_raw url referer cookies
          with
            | Curl.CurlException(curl_code, err_int, "CURLE_COULDNT_CONNECT") as exc -> prerr_curlerror exc;
                                                                                        prerr_endline "...will, try it again"; get_aux (num+1)

            | Curl.CurlException(curl_code, err_int, "CURLE_OPERATION_TIMEOUTED") as exc -> prerr_curlerror exc;
                                                                                         prerr_endline "...will try it again"; get_aux (num+1)

            | Curl.CurlException(curl_code, err_int, "CURLE_COULDNT_RESOLVE_HOST") as exc -> prerr_curlerror exc; None

            | Curl.CurlException(curl_code, err_int, "CURLE_GOT_NOTHING") as exc -> prerr_curlerror exc;
                                                                                    prerr_endline "I now will wait 90 s and then try it again";
                                                                                    Unix.sleep 90; get_aux (num+1)

            | Curl.CurlException( curl_code, err_int, message ) as exc  -> prerr_curlerror exc; None

        else (prerr_endline ("The following document could not be retrieved:" ^ url); None)
      in
        get_aux 1


  SIGNATUR:
  sig 
    val get : string -> string option -> string option -> (string * string list) option 
   end 

*)



(*



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


  end






