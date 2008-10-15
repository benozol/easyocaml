open EzyFeatures
open EzyUtils

;;let module M = EzyLangLevel in () ;;

let logger = new Logger.logger "ezySetup"

type t = { features : program_feats option;       (** allowed features; if None then no language restrictions are in place *)
           modules : (string * bool) list option; (** accessible modules; if the bool flag is true, the module is opened automatically;
                                                      if None then the module restrictions are in placed *)
           inc_dirs : string list;                (** list of directories that should be added to the load path *)
           obj_files : string list;               (** list of object files *)
         }

let default = { features = None; inc_dirs = []; modules = None; obj_files = [] }
   
let do_setup () =
  if !Clflags.easytyping then
    match !Clflags.easylevel with
      | None -> { default with features = Some (all_program_features true) }
      | Some name -> 
          let init_lang = EzyLang.load name in
          let lang = List.fold_left EzyLang.extend_with_teachpack init_lang !Clflags.easyteachpacks in
            { features = Some lang.EzyLang.features; 
              inc_dirs = lang.EzyLang.inc_dirs; 
              modules = Some lang.EzyLang.modules; 
              obj_files = lang.EzyLang.obj_files }
  else
    if !Clflags.easyerrorprinter <> None ||
       !Clflags.easy_dot_type_graph <> false ||
       !Clflags.easylevel <> None ||
       !Clflags.easyteachpacks <> []
    then EzyErrors.raise_fatal (EzyErrors.Other_fatal "Cannot use easyocaml features unless -easy flag given.")
    else default

let patch_clflags s =
  (* objfiles, include_dirs, and preload_objects are in reverse order *)
  let add r l = r := !r @ (List.rev l) in
  add Clflags.objfiles s.obj_files;
  add Clflags.include_dirs s.inc_dirs

let setup_val = ref None

let setup () = 
  match !setup_val with
    | Some x -> x
    | None ->
        let s = do_setup () in
          logger#debug "easyocaml setup: modules = %a; inc_dirs = %a; obj_files = %a" 
            (format_option (format_list (format_pair Format.pp_print_string Format.pp_print_bool) ", ")) s.modules
            (format_list Format.pp_print_string " ") s.inc_dirs
            (format_list Format.pp_print_string " ") s.obj_files;
          patch_clflags s;
          setup_val := Some s;
          s
