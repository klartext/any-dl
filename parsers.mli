exception Not_found_Element
val activate_controlstrings : string -> string
val if_match_give_group_of_groups :
  string -> regexp_str:string -> string array array option
val first_nonblank_position : string -> int * int * int
module Rebase :
  sig
    val guess_scheme : string -> string
    val common_syntax_of_url : string -> Neturl.url_syntax
    val remove_scheme_from_url : string -> string
    val remove_fragment_from_url : string -> string
    val rebase_url : ?verbose:bool -> string -> string -> string option
    val try_rebase : ?verbose:bool -> ?errmsg:bool -> string -> string -> string
  end
val suffixes : string list
val url_to_filename : string -> string
val url_to_filename_with_suffix_check : string -> string
val print_times : ?scale:int -> char -> int -> unit
module Xmlparse :
  sig
    module A :
      sig
        type tree = XmlElement of Xmlm.tag * tree list | PCData of string
        val input_tree : Xmlm.input -> Xmlm.dtd * tree
        val create_xmltree_from_string : string -> Xmlm.dtd * tree
      end
    val print_element : string * (string * string) -> unit
    val traverse_print : A.tree -> unit
    val parse_string : string -> Xmlm.dtd * A.tree
    val get_href_from_xml : A.tree -> string list
  end
module Htmlparse :
  sig
    val string_to_nethtml : string -> Nethtml.document list
    val strip_html_data : Nethtml.document list -> Nethtml.document list
    val dump_html : Nethtml.document list -> unit
    val dump_html_data : Nethtml.document list -> unit
    val show_tags : Nethtml.document list -> unit
    val show_tags_full_path : Nethtml.document list -> unit
    val show_tag_hierarchy : Nethtml.document list -> unit
    val collect_subtags :
      (string * string) list -> string option -> string list
    val debug : bool
    val parse_html :
      ?pickdata:bool ->
      ?tagmatch:string ->
      ?subtag:string option ->
      ?matcher:(string -> bool) -> Nethtml.document list -> string list
    val element_or_data_in_doclist : Nethtml.document list -> unit
    val arg_pair_does_match : 'a -> 'b -> ('a * 'b) list -> bool
    val arg_key_does_match : string -> (string * string) list -> bool
    val arg_val_does_match : string -> (string * string) list -> bool
    val extract_arg_keys_from_doc : Nethtml.document -> string list
    val extract_arg_keys_from_topdocs_of_doclist :
      Nethtml.document list -> string array list
    val extract_arg_values_from_doc : Nethtml.document -> string list
    val extract_arg_values_from_topdocs_of_doclist :
      Nethtml.document list -> string array list
    val extract_arg_pairs_from_doc :
      Nethtml.document -> (string * string) list
    val extract_arg_pairs_from_topdocs_of_doclist :
      Nethtml.document list -> string array list
    val extract_tagname_from_doc : Nethtml.document -> string
    val extract_tagname_from_topdocs_of_doclist :
      Nethtml.document list -> string list
    val find_elements_by_tag_name :
      string -> Nethtml.document list -> Nethtml.document list
    val find_any_elements : Nethtml.document list -> Nethtml.document list
    type matcher_t =
        string ->
        string -> string -> string -> (string * string) list -> bool
    val find_elements_by :
      ?tagval:string ->
      ?argkey:string ->
      ?argval:string ->
      matcher_t -> Nethtml.document list -> Nethtml.document list
    val matcher_tag_argkey : matcher_t
    val matcher_tag_argval : matcher_t
    val matcher_tag_argpair : matcher_t
    val matcher_argkey : matcher_t
    val matcher_argval : matcher_t
    val matcher_argpair : matcher_t
    val find_elements_by_tag_argpair :
      string ->
      string -> string -> Nethtml.document list -> Nethtml.document list
    val find_elements_by_tag_argkey :
      string -> string -> Nethtml.document list -> Nethtml.document list
    val find_elements_by_tag_argval :
      string -> string -> Nethtml.document list -> Nethtml.document list
    val find_elements_by_argpair :
      string -> string -> Nethtml.document list -> Nethtml.document list
    val find_elements_by_argkey :
      string -> Nethtml.document list -> Nethtml.document list
    val find_elements_by_argval :
      string -> Nethtml.document list -> Nethtml.document list
    val collect_data : ?sep:char -> Nethtml.document list -> string
    val collect_data_per_doc : Nethtml.document list -> string list
    val find_elements_by_class_name :
      string -> Nethtml.document list -> Nethtml.document list
    val find_elements_by_id :
      string -> Nethtml.document list -> Nethtml.document list
    val find_elements_by_name :
      string -> Nethtml.document list -> Nethtml.document list
  end
val conv_to_doclist : string -> Nethtml.document list
val print_doclist_to_outchannel : Nethtml.document list -> unit
val convert_doclist_to_htmlstring : Nethtml.document list -> string
val dump_html_from_string : string -> unit
val dump_html_data_from_string : string -> unit
val show_tags_from_string : string -> unit
val show_tags_fullpath_from_string : string -> unit
val show_tag_hierarchy_from_string : string -> unit
val linkextract_str : string -> string list
val imageextract_str : string -> string list
val titleextract_str : string -> string list
val tagextract_str : string -> string -> string list
val find_elements_by_tag_name_str : string -> string -> Nethtml.document list
val find_elements_by_class_name_str :
  string -> string -> Nethtml.document list
val find_elements_by_id_str : string -> string -> Nethtml.document list
val find_elements_by_name_str : string -> string -> Nethtml.document list
val find_elements_by_argpair_str :
  string -> string -> string -> Nethtml.document list
val xml_get_href_from_string : string -> string list
val xml_get_href : Xmlparse.A.tree -> string list
val dump_elements_as_seperate_doclists : Nethtml.document list -> unit
val table_unparse : Nethtml.document list -> string array array
