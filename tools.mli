exception Invalid_Index
val apply_on_pair : ('a -> 'b) -> 'a * 'a -> 'b * 'b
val apply_on_pair_and_combine_with :
  ('a -> bool) -> 'a * 'a -> (bool -> bool -> bool) -> bool
val verbose_fprintf :
  ?optflag:bool ->
  out_channel -> ('a, out_channel, unit, unit, unit, unit) format6 -> 'a
val very_verbose_fprintf :
  ?optflag:bool ->
  out_channel -> ('a, out_channel, unit, unit, unit, unit) format6 -> 'a
val verbose_printf :
  ?optflag:bool -> ('a, out_channel, unit, unit, unit, unit) format6 -> 'a
val very_verbose_printf :
  ?optflag:bool -> ('a, out_channel, unit, unit, unit, unit) format6 -> 'a
val print_warning : string -> unit
val item_selection : 'a array -> int list -> 'a array
val filesize : string -> int
val save_string_to_file : string -> string -> unit
val read_file : string -> string
val sort : string list -> string list
val array_drop : 'a array -> int -> 'a array
val pairlist_to_list : ('a * 'a) list -> 'a list
val select_decoding_scheme : string -> [> `Enc_iso88591 | `Enc_utf8 ]
val html_decode : ?inenc:Netconversion.encoding -> string -> string
val lines_of_string : string -> string list
val test_pattern_match_on_string : string -> string -> bool
val add_item_once : 'a list -> 'a list
val transpose : string array array -> string array array
val paste : ?sep:string -> string list -> string
val wrap_string : string -> string -> string list -> string list
module Array2 :
  sig
    external length : 'a array -> int = "%array_length"
    external get : 'a array -> int -> 'a = "%array_safe_get"
    external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
    external make : int -> 'a -> 'a array = "caml_make_vect"
    external create : int -> 'a -> 'a array = "caml_make_vect"
    val init : int -> (int -> 'a) -> 'a array
    val make_matrix : int -> int -> 'a -> 'a array array
    val create_matrix : int -> int -> 'a -> 'a array array
    val append : 'a array -> 'a array -> 'a array
    val concat : 'a array list -> 'a array
    val sub : 'a array -> int -> int -> 'a array
    val copy : 'a array -> 'a array
    val fill : 'a array -> int -> int -> 'a -> unit
    val blit : 'a array -> int -> 'a array -> int -> int -> unit
    val to_list : 'a array -> 'a list
    val of_list : 'a list -> 'a array
    val iter : ('a -> unit) -> 'a array -> unit
    val map : ('a -> 'b) -> 'a array -> 'b array
    val iteri : (int -> 'a -> unit) -> 'a array -> unit
    val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
    val fold_right : ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a
    external make_float : int -> float array = "caml_make_float_vect"
    val sort : ('a -> 'a -> int) -> 'a array -> unit
    val stable_sort : ('a -> 'a -> int) -> 'a array -> unit
    val fast_sort : ('a -> 'a -> int) -> 'a array -> unit
    external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
    external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
    val filter : ('a -> bool) -> 'a array -> 'a array
    val exists : ('a -> bool) -> 'a array -> bool
    val filter_row_by_colmatch :
      ('a -> bool) -> 'a array array -> 'a array array
    val num_rows : 'a array -> int
    val max_row_idx : 'a array -> int
    val num_cols_of_row : int -> 'a array array -> int
    val max_col_idx_of_row : int -> 'a array array -> int
    val remove_empty_arrays_from_matrix :
      ?message:bool -> ?msgtxt:string -> 'a array array -> 'a array array
  end
module Sleep :
  sig val sleep_float : float -> unit val sleep_ms : int -> unit end
val print_pcre_error : Pcre.error -> unit
