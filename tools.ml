(* CLI-VERBOSE-dependent print functions ! *)
(* --------------------------------------- *)
let verbose_fprintf channel formatstr =
  let frmt = format_of_string formatstr in
  if Cli.opt.Cli.verbose || Cli.opt.Cli.very_verbose then Printf.fprintf channel frmt else Printf.ifprintf channel frmt

let very_verbose_fprintf channel formatstr =
  let frmt = format_of_string formatstr in
  if Cli.opt.Cli.very_verbose then Printf.fprintf channel frmt else Printf.ifprintf channel frmt

let verbose_printf       formatstr = verbose_fprintf stdout formatstr
let very_verbose_printf  formatstr = very_verbose_fprintf stdout formatstr
(* -v means: very_verbose is wanted output, not an error... why then stderr? -> unneeded?
let very_verbose_eprintf formatstr = very_verbose_fprintf stderr formatstr
*)

