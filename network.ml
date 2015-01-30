(*
  any-dl:
  -------
  Generic Media-Downloader for any kind of Online-Mediathek.

  Author / copyright: Oliver Bandel
  Copyleft:           GNU GENERAL PUBLIC LICENSE  v3 (or higher)
*)

open Nethtml
open Tools


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
    open Nethttp_client

      let print_cookie  cookie =
        Printf.printf "cookie-name: %s\n" cookie.cookie_name;
        Printf.printf "cookie-value: %s\n" cookie.cookie_value;
        (match cookie.cookie_expires with None -> () | Some ex   -> Printf.printf "cookie-expires: %f\n" ex);
        (match cookie.cookie_domain  with None -> () | Some dom  -> Printf.printf "cookie-domain: %s\n" dom);
        (match cookie.cookie_path    with None -> () | Some path -> Printf.printf "cookie-path: %s\n" path);
        Printf.printf "cookie-secure: %s\n" (if cookie.cookie_secure then "true" else "false");
        print_endline "     ------";
        ()


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

        (match nscookie.cookie_expires with None -> [] | Some flt  -> (("expires", string_of_float flt ) :: !lst));
        (match nscookie.cookie_domain  with None -> [] | Some dom  -> (("domain", dom ) :: !lst));
        (match nscookie.cookie_path    with None -> [] | Some path -> (("path", path ) :: !lst));
        ("secure", string_of_bool nscookie.cookie_secure) :: !lst;
        !lst



      (* ==================================================== *)
      let get_raw url (referer: string option) cookies =
        let pipeline = new pipeline in

        let get_call  = new get url in (* Referrer? Cookies? *)

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
             | `Client_error           -> print_endline "Client_error"
             | `Http_protocol_error  _ -> print_endline "Http_protocol_error"
             | `Redirection            -> print_endline "Redirection"
             | `Server_error           -> print_endline "Server_error"
             | `Successful             -> print_endline "GET-Successful"
             | `Unserved               -> print_endline "Unserved"
        end;

        Printf.printf "Status-Code    GET:  %d\n" get_call # response_status_code;
        Printf.printf "Status-Message GET:  %s\n" get_call # response_status_text;

        let cookies = Nethttp.Header.get_set_cookie  (get_call # response_header) in

        if Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose
        then
          begin
            print_endline "------------------------------------------";
            print_endline "=*=*=> COOOKIES:";
              List.iter print_cookie cookies;
            print_endline "<=*=*= COOKIES";
          end;

        Some ( get_call # response_body # value, cookies )



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




module CurlHelp =
  struct
  open Curl


  let string_of_curlcode curlcode =
    match  curlcode with
      | CURLE_OK                   -> "CURLE_OK"
      | CURLE_UNSUPPORTED_PROTOCOL -> "CURLE_UNSUPPORTED_PROTOCOL"
      | CURLE_FAILED_INIT          -> "CURLE_FAILED_INIT"
      | CURLE_URL_MALFORMAT        -> "CURLE_URL_MALFORMAT"
      | CURLE_URL_MALFORMAT_USER   -> "CURLE_URL_MALFORMAT_USER"
      | CURLE_COULDNT_RESOLVE_PROXY -> "CURLE_COULDNT_RESOLVE_PROXY"
      | CURLE_COULDNT_RESOLVE_HOST -> "CURLE_COULDNT_RESOLVE_HOST"
      | CURLE_COULDNT_CONNECT      -> "CURLE_COULDNT_CONNECT"
      | CURLE_FTP_WEIRD_SERVER_REPLY -> "CURLE_FTP_WEIRD_SERVER_REPLY"
      | CURLE_FTP_ACCESS_DENIED    -> "CURLE_FTP_ACCESS_DENIED"
      | CURLE_FTP_USER_PASSWORD_INCORRECT -> "CURLE_FTP_USER_PASSWORD_INCORRECT"
      | CURLE_FTP_WEIRD_PASS_REPLY -> "CURLE_FTP_WEIRD_PASS_REPLY"
      | CURLE_FTP_WEIRD_USER_REPLY -> "CURLE_FTP_WEIRD_USER_REPLY"
      | CURLE_FTP_WEIRD_PASV_REPLY -> "CURLE_FTP_WEIRD_PASV_REPLY"
      | CURLE_FTP_WEIRD_227_FORMAT -> "CURLE_FTP_WEIRD_227_FORMAT"
      | CURLE_FTP_CANT_GET_HOST    -> "CURLE_FTP_CANT_GET_HOST"
      | CURLE_FTP_CANT_RECONNECT   -> "CURLE_FTP_CANT_RECONNECT"
      | CURLE_FTP_COULDNT_SET_BINARY -> "CURLE_FTP_COULDNT_SET_BINARY"
      | CURLE_PARTIAL_FILE         -> "CURLE_PARTIAL_FILE"
      | CURLE_FTP_COULDNT_RETR_FILE -> "CURLE_FTP_COULDNT_RETR_FILE"
      | CURLE_FTP_WRITE_ERROR      -> "CURLE_FTP_WRITE_ERROR"
      | CURLE_FTP_QUOTE_ERROR      -> "CURLE_FTP_QUOTE_ERROR"
      | CURLE_HTTP_NOT_FOUND       -> "CURLE_HTTP_NOT_FOUND"
      | CURLE_WRITE_ERROR          -> "CURLE_WRITE_ERROR"
      | CURLE_MALFORMAT_USER       -> "CURLE_MALFORMAT_USER"
      | CURLE_FTP_COULDNT_STOR_FILE -> "CURLE_FTP_COULDNT_STOR_FILE"
      | CURLE_READ_ERROR           -> "CURLE_READ_ERROR"
      | CURLE_OUT_OF_MEMORY        -> "CURLE_OUT_OF_MEMORY"
      | CURLE_OPERATION_TIMEOUTED  -> "CURLE_OPERATION_TIMEOUTED"
      | CURLE_FTP_COULDNT_SET_ASCII -> "CURLE_FTP_COULDNT_SET_ASCII"
      | CURLE_FTP_PORT_FAILED      -> "CURLE_FTP_PORT_FAILED"
      | CURLE_FTP_COULDNT_USE_REST -> "CURLE_FTP_COULDNT_USE_REST"
      | CURLE_FTP_COULDNT_GET_SIZE -> "CURLE_FTP_COULDNT_GET_SIZE"
      | CURLE_HTTP_RANGE_ERROR     -> "CURLE_HTTP_RANGE_ERROR"
      | CURLE_HTTP_POST_ERROR      -> "CURLE_HTTP_POST_ERROR"
      | CURLE_SSL_CONNECT_ERROR    -> "CURLE_SSL_CONNECT_ERROR"
      | CURLE_FTP_BAD_DOWNLOAD_RESUME -> "CURLE_FTP_BAD_DOWNLOAD_RESUME"
      | CURLE_FILE_COULDNT_READ_FILE -> "CURLE_FILE_COULDNT_READ_FILE"
      | CURLE_LDAP_CANNOT_BIND     -> "CURLE_LDAP_CANNOT_BIND"
      | CURLE_LDAP_SEARCH_FAILED   -> "CURLE_LDAP_SEARCH_FAILED"
      | CURLE_LIBRARY_NOT_FOUND    -> "CURLE_LIBRARY_NOT_FOUND"
      | CURLE_FUNCTION_NOT_FOUND   -> "CURLE_FUNCTION_NOT_FOUND"
      | CURLE_ABORTED_BY_CALLBACK  -> "CURLE_ABORTED_BY_CALLBACK"
      | CURLE_BAD_FUNCTION_ARGUMENT -> "CURLE_BAD_FUNCTION_ARGUMENT"
      | CURLE_BAD_CALLING_ORDER    -> "CURLE_BAD_CALLING_ORDER"
      | CURLE_HTTP_PORT_FAILED     -> "CURLE_HTTP_PORT_FAILED"
      | CURLE_BAD_PASSWORD_ENTERED -> "CURLE_BAD_PASSWORD_ENTERED"
      | CURLE_TOO_MANY_REDIRECTS   -> "CURLE_TOO_MANY_REDIRECTS"
      | CURLE_UNKNOWN_TELNET_OPTION -> "CURLE_UNKNOWN_TELNET_OPTION"
      | CURLE_TELNET_OPTION_SYNTAX -> "CURLE_TELNET_OPTION_SYNTAX"
      | CURLE_OBSOLETE             -> "CURLE_OBSOLETE"
      | CURLE_SSL_PEER_CERTIFICATE -> "CURLE_SSL_PEER_CERTIFICATE"
      | CURLE_GOT_NOTHING          -> "CURLE_GOT_NOTHING"
      | CURLE_SSL_ENGINE_NOTFOUND  -> "CURLE_SSL_ENGINE_NOTFOUND"
      | CURLE_SSL_ENGINE_SETFAILED -> "CURLE_SSL_ENGINE_SETFAILED"
      | CURLE_SEND_ERROR           -> "CURLE_SEND_ERROR"
      | CURLE_RECV_ERROR           -> "CURLE_RECV_ERROR"
      | CURLE_SHARE_IN_USE         -> "CURLE_SHARE_IN_USE"
      | CURLE_SSL_CERTPROBLEM      -> "CURLE_SSL_CERTPROBLEM"
      | CURLE_SSL_CIPHER           -> "CURLE_SSL_CIPHER"
      | CURLE_SSL_CACERT           -> "CURLE_SSL_CACERT"
      | CURLE_BAD_CONTENT_ENCODING -> "CURLE_BAD_CONTENT_ENCODING"
      | CURLE_LDAP_INVALID_URL     -> "CURLE_LDAP_INVALID_URL"
      | CURLE_FILESIZE_EXCEEDED    -> "CURLE_FILESIZE_EXCEEDED"
      | CURLE_FTP_SSL_FAILED       -> "CURLE_FTP_SSL_FAILED"
      | CURLE_USE_SSL_FAILED       -> "CURLE_USE_SSL_FAILED"
      | CURLE_SEND_FAIL_REWIND     -> "CURLE_SEND_FAIL_REWIND"
      | CURLE_SSL_ENGINE_INITFAILED -> "CURLE_SSL_ENGINE_INITFAILED"
      | CURLE_LOGIN_DENIED         -> "CURLE_LOGIN_DENIED"
      | CURLE_TFTP_NOTFOUND        -> "CURLE_TFTP_NOTFOUND"
      | CURLE_TFTP_PERM            -> "CURLE_TFTP_PERM"
      | CURLE_REMOTE_DISK_FULL     -> "CURLE_REMOTE_DISK_FULL"
      | CURLE_TFTP_ILLEGAL         -> "CURLE_TFTP_ILLEGAL"
      | CURLE_TFTP_UNKNOWNID       -> "CURLE_TFTP_UNKNOWNID"
      | CURLE_REMOTE_FILE_EXISTS   -> "CURLE_REMOTE_FILE_EXISTS"
      | CURLE_TFTP_NOSUCHUSER      -> "CURLE_TFTP_NOSUCHUSER"
      | CURLE_CONV_FAILED          -> "CURLE_CONV_FAILED"
      | CURLE_CONV_REQD            -> "CURLE_CONV_REQD"
      | CURLE_SSL_CACERT_BADFILE   -> "CURLE_SSL_CACERT_BADFILE"
      | CURLE_REMOTE_FILE_NOT_FOUND -> "CURLE_REMOTE_FILE_NOT_FOUND"
      | CURLE_SSH                  -> "CURLE_SSH"
      | CURLE_SSL_SHUTDOWN_FAILED  -> "CURLE_SSL_SHUTDOWN_FAILED"
      | CURLE_AGAIN                -> "CURLE_AGAIN"

  let prerr_curlerror curl_exc =
    match curl_exc with
      | Curl.CurlException(curl_code, err_int, message) -> Printf.eprintf "%s, %d, %s\n" (string_of_curlcode curl_code) err_int message
      | exc                                             -> Printf.eprintf "exception found in prerr_curlerror: %s" (Printexc.to_string exc)
  end


(* ------------------------ *)
(* networking with CURL-lib *)
(* ------------------------ *)
module Curly =
  (
  struct

    open Curl
    open CurlHelp

    let new_curl_connection () =
        let buffer = Buffer.create 4096 in
        let connection = new Curl.handle in
        let write_function str = Buffer.add_string buffer str; String.length str in
        connection#set_writefunction write_function;

        let user_agent = Cli.opt.Cli.user_agent in
        connection#set_useragent user_agent;
        (*
        connection#set_timeout Config.timeout;
        *)
        connection#set_followlocation true;
        connection#set_connecttimeout 10;
        connection#set_timeout 600;
        connection#set_cookiefile "cookiefile.txt";
        connection#set_forbidreuse false;
        connection#set_encoding CURL_ENCODING_DEFLATE;
        (*
        connection#set_connecttimeout Config.connect_timeout;
        *)

      (*  if very_verbose is true, then Curl-option set_verbose is activated. *)
      (*  Curl is very verbose, when this option is set.                      *)
      (* -------------------------------------------------------------------- *)
      if Cli.opt.Cli.very_verbose then connection#set_verbose true; (* curl itself is very_verbose here, driven by cli *)

      (connection, buffer)



    let get_raw url referer cookies =

            let conn, buffer = new_curl_connection() in
            conn#set_url url;
            begin
              match referer with None -> () | Some ref -> conn#set_referer ref
            end;
            begin
              match cookies with None -> () | Some cookies -> conn#set_cookielist cookies
            end;
            conn#set_sslverifypeer false; (* Zertifikate-Prüfung auschalten! *)
            conn#perform;                 (* loslegen: retrieve data *)

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

  end
(*
*)
   :
  sig
    val get : string -> string option -> string option -> (string * string list) option
  end
  )


