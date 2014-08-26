exception Invalid_Index                 (* indexing a col/row that does not exist *)



(* CLI-VERBOSE-dependent print functions ! *)
(* --------------------------------------- *)
let verbose_fprintf ?(optflag=false) channel formatstr =
  let frmt = format_of_string formatstr in
  if Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose || optflag
  then Printf.fprintf channel frmt
  else Printf.ifprintf channel frmt

let very_verbose_fprintf ?(optflag=false) channel formatstr =
  let frmt = format_of_string formatstr in
  if Cli.opt.Cli.very_verbose || optflag
  then Printf.fprintf channel frmt
  else Printf.ifprintf channel frmt


let verbose_printf       ?(optflag=false) formatstr = verbose_fprintf      ~optflag:optflag stdout formatstr
let very_verbose_printf  ?(optflag=false) formatstr = very_verbose_fprintf ~optflag:optflag stdout formatstr


(* -v means: very_verbose is wanted output, not an error... why then stderr? -> unneeded?
let very_verbose_eprintf formatstr = very_verbose_fprintf stderr formatstr
*)



(* save string to file *)
(* ------------------- *)
let save_string_to_file str filename =
  let oc = open_out filename in
  output_string oc str;
  close_out oc



(* ------------------------------------------------------------------------ *)
(* Sortiere String-Liste mit Reihenfolge von a nach z; case insensitive *)
let sort stringlist = List.sort ( fun a b -> let al = String.lowercase a and bl = String.lowercase b
                                   in if al < bl then (-1) else if al = bl then 0 else 1)  stringlist
(* ------------------------------------------------------------------------ *)


(* =================================================== *)
(* from an array drop the element with the given index *)
(* =================================================== *)
let array_drop arr dropidx =
  let len = Array.length arr             in

  (* Argument checking *)
  (* ----------------- *)
  if dropidx < 0 || dropidx > len - 1 then raise Invalid_Index;


  let res = Array.make (len - 1) arr.(0) in

  let srcidx    = ref 0 in
  let targetidx = ref 0 in

  (* --------------------------------------------------------------------------------- *)
  (* copy any element from src to target, that has different index than the drop-index *)
  (* --------------------------------------------------------------------------------- *)
  while !srcidx < len
  do
    if !srcidx != dropidx
    then
      begin
        res.(!targetidx) <- arr.(!srcidx); (* copy data *)
        incr srcidx;
        incr targetidx
      end
    else
      begin
        incr srcidx;
      end
  done;
  res (* the resulting array *)



(* ======================================================== *)
(* converts a list of pairs into a list, by just prepending *)
(* the items of the pairs into the resullting list          *)
(* ======================================================== *)
let pairlist_to_list  inputlist =
  let rec aux res li = match li with
    | (k,v)::tl -> aux (v::k::res) tl
    | []        -> List.rev res
in
  aux [] inputlist




(* ======================================================== *)
(* Decode HTML-stuff (ampersand-foobar)                     *)
(* -------------------------------------------------------- *)
(* utf8 is hard encoded, as long as no encoding detection   *)
(* is implemented and in use.                               *)
(* ======================================================== *)
let html_decode ?(inenc=`Enc_utf8) str =
  try
    Netencoding.Html.decode ~in_enc:inenc ~out_enc:`Enc_utf8 () str
  with  Netconversion.Malformed_code -> str



(* ======================================= *)
let lines_of_string  str = Pcre.split ~pat:"\n" str
