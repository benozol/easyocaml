
(** This module provides some EasyOCaml specific miscellaneous functions:
  *  - registering error printer: used in driver/ and toplevel/
  *)

open EzyUtils
open EzyUtils.Infix

type error_printer = 
  [ `Text | `Plugin of string | EzyStructuredErrorReport.output_type ]

let recognize_error_printer str : error_printer =
  match String.lowercase str with
    | "text" -> `Text
    | "xml" -> `Xml
    | "sexp" -> `Sexp
    | plugin ->
        if String.check_suffix plugin ".cmo" 
        then `Plugin plugin
        else Misc.fatal_error ("Not a valid error printer: " ^ plugin)

let register_error_printer : string -> unit =
  recognize_error_printer >> begin function
    | `Text -> ()
    | `Plugin plugin ->
        begin try (
          Dynlink.init () ;
          Dynlink.loadfile plugin ;
        ) with Dynlink.Error err as exn ->
         print_endline (Dynlink.error_message err) ;
         raise exn ;
        end
    | #EzyStructuredErrorReport.output_type as output_type ->
        EzyStructuredErrorReport.register output_type
  end
