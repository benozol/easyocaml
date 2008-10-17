
(** This module provides some EasyOCaml specific miscellaneous functions:
  *  - registering error printer: used in driver/ and toplevel/
  *)

open EzyUtils
open EzyUtils.Infix

let logger = new Logger.logger "StructuredErrorReport"

type error_printer = 
  [ `Text | `Plugin of string | EzyStructuredErrorReport.output_type ]

let recognize_error_printer str : error_printer =
  match String.lowercase str with
    | "text" -> `Text
    | "xml" -> `Xml
    | "sexp" -> `Sexp
    | _ ->
        if String.check_suffix str ".cmo" 
        then `Plugin str
        else Misc.fatal_error ("Not a valid error printer: " ^ str ^ " (can only be `xml', `sexp', or a plugin)")

let register_error_printer : string -> unit =
  fun str ->
    match recognize_error_printer str with
      | `Text -> 
          logger#info "Default error report"
      | #EzyStructuredErrorReport.output_type as output_type ->
          logger#info "Setting structured error reporter" ;
          EzyStructuredErrorReport.register output_type
      | `Plugin plugin ->
          begin try 
            logger#info "Loading error reporting plugin %s" plugin ;
            Dynlink.init () ;
            Dynlink.loadfile plugin ;
          with Dynlink.Error err ->
           Misc.fatal_error (Dynlink.error_message err);
          end

