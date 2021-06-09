val networking_verbosity : [< `Silent | `Verbose | `Very_verbose ] -> unit
val set_useragent : string -> unit
module Simple :
  sig
    val get : string -> string
    val head : string -> (string * string) list
  end
module Cookies :
  sig
    val print_cookie : Nethttp.netscape_cookie -> unit
    val if_veryverbose_print_cookies : Nethttp.netscape_cookie list -> unit
    val fill_empty_cookiefields :
      string -> Nethttp.netscape_cookie -> Nethttp.netscape_cookie
    val cookie_to_cookie_ct :
      Nethttp.netscape_cookie -> (string * string) list
  end
module Pipelined :
  sig
    exception Get_error of Nethttp_client.status
    exception Get_problem of Nethttp_client.status
    exception Download_error of Nethttp_client.status
    exception Download_problem of Nethttp_client.status
    val judge_getcall_status :
      string ->
      [< `Client_error
       | `Http_protocol_error of exn
       | `Redirection
       | `Server_error
       | `Successful
       | `Unserved ] ->
      unit
    val set_response_body_storage_of_call :
      < set_response_body_storage : [> `File of 'a -> 'b | `Memory ] -> 'c;
        .. > ->
      'b option -> 'c
    val set_useragent_of_call :
      < request_header : [> `Base ] -> #Nethttp.http_header; .. > ->
      string -> unit
    val set_referrer_of_call :
      < request_header : [> `Base ] -> #Nethttp.http_header; .. > ->
      string option -> unit
    val set_cookies_of_call :
      < request_header : [> `Base ] -> #Nethttp.http_header; .. > ->
      Nethttp.netscape_cookie list option -> unit
    val get_or_post_to_mem_or_file_raw :
      string ->
      ?user_agent:string ->
      string option ->
      (string * string) list option ->
      Nethttp.netscape_cookie list option ->
      string option -> string option * Nethttp.netscape_cookie list option
    val get_or_post_to_mem_or_file :
      string ->
      string option ->
      (string * string) list option ->
      Nethttp.netscape_cookie list option ->
      string option -> string option * Nethttp.netscape_cookie list option
    val get :
      string ->
      string option ->
      Nethttp.netscape_cookie list option ->
      (string * Nethttp.netscape_cookie list) option
    val get_download :
      string ->
      string option ->
      Nethttp.netscape_cookie list option ->
      string -> Nethttp.netscape_cookie list option
    val post :
      string ->
      string option ->
      (string * string) list ->
      Nethttp.netscape_cookie list option ->
      (string * Nethttp.netscape_cookie list) option
    val post_download :
      string ->
      string option ->
      (string * string) list ->
      Nethttp.netscape_cookie list option ->
      string -> Nethttp.netscape_cookie list option
  end
