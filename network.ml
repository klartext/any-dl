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


(*
let urlmatcher url url_regexp =
  if (Pcre.pmatch ~rex:url_regexp url)
  then
    begin
      let substr_obj = Pcre.exec ~rex:url_regexp url in
        Pcre.get_substring substr_obj 0
    end
  else
    begin
      prerr_endline ("url-scheme for baseurl could not be matched (" ^ url ^ ")");
      url
    end

*)


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
        (connection, buffer)

    let get_raw url referer =
            let conn, buffer = new_curl_connection() in
            conn#set_url url;
            begin
              match referer with None -> () | Some ref -> conn#set_referer ref
            end;
            conn#set_sslverifypeer false; (* Zertifikate-Prüfung auschalten! *)
            conn#perform;                 (* loslegen: retrieve data *)

            (* Ergebnis-Auswertung *)
            (* ------------------- *)
            let http_code = conn#get_responsecode in
            (*
            List.iter (fun s -> print_string "=========="; print_endline s) (conn#get_cookielist);
            *)
            conn#cleanup; (* CLEAN UP *)

            if http_code < 400
            then
              let result = Buffer.contents buffer in Some result
            else ( Printf.eprintf "http-returncode: %d (URL: %s)\n" http_code url; None)

    let get url referer =
      let trial_numbers = 3 in
      let rec get_aux num =
        if num <= trial_numbers
        then
          try get_raw url referer
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
    val get : string -> string option -> string option
  end
  )


