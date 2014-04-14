(*
  any-dl:
  -------
  Generic Media-Downloader for any kind of Online-Mediathek.

  Author / copyright: Oliver Bandel
  Copyleft:           GNU GENERAL PUBLIC LICENSE  v3 (or higher)
*)

open Nethtml

(* ------------------------------------------------------------------------ *)
(* Sortiere String-Liste mit Reihenfolge von a nach z; case insensitive *)
let sort stringlist = List.sort ( fun a b -> let al = String.lowercase a and bl = String.lowercase b
                                   in if al < bl then (-1) else if al = bl then 0 else 1)  stringlist

(* ------------------------------------------------------------------------ *)


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

  let prerr_curlerror curl_exc =
    match curl_exc with
      Curl.CurlException(curl_code, err_int, message) -> Printf.printf "%s, %d, %s\n" (string_of_curlcode curl_code) err_int message
  end


(* ------------------------ *)
(* networking with CURL-lib *)
(* ------------------------ *)
module Curly =
  (
  struct

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
    (*
        connection#set_connecttimeout Config.connect_timeout;
    *)
    (*  connection#set_verbose true;*)
      if Cli.opt.Cli.verbose then connection#set_verbose true; (* curl itself is verbose here, driven by cli *)
        (connection, buffer)


    let get_raw url referer cookies =
            (*
            if Cli.opt.Cli.verbose then Printf.printf "URL: %s\t Referrer: %s\n" url referer;
            *)

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
                if Cli.opt.Cli.verbose
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
                if Cli.opt.Cli.verbose then Printf.eprintf "http-returncode: %d (URL: %s)\n" http_code url;
                None
              end


    let get url referer cookies =
      Printf.printf "GET %s\n" url; flush stdout;
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


