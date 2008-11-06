
(** This module provides some EasyOCaml specific miscellaneous functions:
  *  - registering error printer: used in driver/ and toplevel/
  *)

open EzyUtils
open EzyUtils.Infix

let logger = new Logger.logger "StructuredErrorReport"

type error_printer = 
  [ `Text | `Plugin of string ]

let recognize_error_printer : string -> error_printer =
  fun str ->
    if String.lowercase str = "text" then
      `Text
    else
      if String.check_suffix str ".cmo" 
      then `Plugin str
      else Misc.fatal_error ("Not a valid error printer: " ^ str ^ " (must be an OCaml object file, calling EzyErrors.register")

let register_error_printer : string -> unit =
  fun str ->
    match recognize_error_printer str with
      | `Text -> 
          logger#info "Default error report"
(*
      | #EzyStructuredErrorReport.output_type as output_type ->
          logger#info "Setting structured error reporter" ;
          EzyStructuredErrorReport.register output_type
 *)
      | `Plugin plugin ->
          begin try 
            logger#info "Loading error reporting plugin %s" plugin ;
            Dynlink.init () ;
            Dynlink.loadfile plugin ;
          with Dynlink.Error err ->
           Misc.fatal_error (Dynlink.error_message err);
          end

