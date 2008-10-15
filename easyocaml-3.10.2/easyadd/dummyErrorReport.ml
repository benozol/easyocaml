open EzyUtils
open EzyTypingCoreTypes
open EzyOcamlmodules
open EzyErrors

let logger = new Logger.logger "dummy err report"

module ErrorReporter = struct

  let name = "Dummy error reporter"

  let print_errors ?program ast ppf errors =
    Format.pp_print_string ppf "Lots of errors"

  let print_heavies ?program ast ppf heavies =
    Format.pp_print_string ppf "Lots of heavy errors"

  let print_fatal ?program loc ppf fatal =
    Format.pp_print_string ppf "A fatal error"
end

let _ =
  let module M = EzyErrors.Register (ErrorReporter) in () ;
  EzyErrors.check := begin fun ppf ->
    let oc = open_out "dummy.out" in
    output_string oc "DummyErrorReport.check\n" ;
    close_out oc ;
    Format.fprintf ppf "DummyErrorReport.check@.";
  end
