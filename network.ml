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



(* ------------------------ *)
(* networking with CURL-lib *)
(* ------------------------ *)
module Curly =
  (
  struct
    let new_curl_connection () =
        let buffer = Buffer.create 4096 in
        let connection = new Curl.handle in
        let write_function str = Buffer.add_string buffer str; String.length str in
        connection#set_writefunction write_function;
    (*
        connection#set_useragent Config.user_agent;
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
              ( Printf.eprintf "http-returncode: %d (URL: %s)\n" http_code url; None)


    let get url referer cookies =
      let trial_numbers = 3 in
      let rec get_aux num =
        if num <= trial_numbers
        then
          try get_raw url referer cookies
          with
            | Curl.CurlException(curl_code, err_int, "CURLE_COULDNT_CONNECT") -> prerr_endline "Could not connect, try again"; get_aux (num+1)
            | Curl.CurlException(curl_code, err_int, "CURLE_OPERATION_TIMEOUTED") -> prerr_endline "Timed_out, try again"; get_aux (num+1)
            | Curl.CurlException(curl_code, err_int, "CURLE_COULDNT_RESOLVE_HOST") -> prerr_endline "could not resolve host"; None
            | Curl.CurlException(curl_code, err_int, "CURLE_GOT_NOTHING") -> prerr_endline "CURLE_GOT_NOTHING: now will wait 90 s and try again"; Unix.sleep 90; get_aux (num+1)
            | Curl.CurlException( curl_code, err_int, message )  -> prerr_endline message; None
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


