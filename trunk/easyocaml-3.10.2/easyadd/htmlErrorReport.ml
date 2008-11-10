open EzyUtils
open EzyUtils.Infix
open EzyOcamlmodules
open EzyTypingCoreTypes
open EzyErrors

let logger = new EzyUtils.Logger.logger "htmlErrorReport"

let template () = format_of_string "
<span id='reset' onclick='javascript:reset()' class='othererror'>reset</span>
<p class='errorlistparagraph'>
  <ul id='errorlist'></ul>
</p>
<p>
  <div class='code'><div class='currenterror'><span name='codeitem' id='null'>
%a
  </div>
</p>
<script type='text/javascript'>
  init([%a]);
</script>
"

let escape_quote char str =
  (* let quote_re = Str.regexp_string char in Str.global_replace quote_re (char ^ String.make 1 char) *)
  let len = String.length str in
  let buf = Buffer.create len in
  let rec aux offset =
    if offset = len
    then Buffer.contents buf
    else
      try
        let next = String.index_from str offset char in
        if next <> offset then
          Buffer.add_string buf (String.sub str offset (next - offset));
        Buffer.add_string buf ("\\" ^ String.make 1 char);
        aux (succ next)
      with Not_found ->
        Buffer.add_string buf (String.sub str offset (String.length str - offset));
        Buffer.contents buf in
  aux 0

let type_to_str ty =
  let pre = format_str "%a" Ty.print ty in
  "'" ^ escape_quote '\'' pre ^ "'"

let loc_string loc = 
  format_str "'%a'" Location.print loc

let for_type_error =
  let force_source = function ExtLocation.Source loc -> loc | _ -> invalid_arg "force_source" in
  let loc_string_for_ty ?(default="null") ty =
    Option.value ~default
      (Option.map ~f:loc_string
         (Option.for_exn (force_source << Ty.get_label) ty)) in
  function
  | ConstructorClash (tx, ty) ->
      let args = [
        type_to_str tx ;
        type_to_str ty ;
        loc_string_for_ty tx ;
        loc_string_for_ty ty ;
      ] in
      "ConstructorClash", args
  | ArityClash (tx, ty, arx, ary) ->
      let args = [
        string_of_int arx ;
        string_of_int ary ;
        loc_string_for_ty tx ;
        loc_string_for_ty ty ;
      ] in
      "ArityClash", args
  | CircularType (tx, ty) ->
      let args = [
        type_to_str tx ;
        type_to_str ty ;
        loc_string_for_ty tx ;
        loc_string_for_ty ty ;
      ] in
      "CircularType", args

let print_error ppf (loc, err) =
  match err with
    | Type_error (terr, err_locs) ->
        let class_name, args = for_type_error terr in
        let err_locs' =
          let filter_to_loc = function
            | ExtLocation.Source loc -> Some (loc_string loc)
            | _ -> None in
          ExtLocationSet.elements err_locs |>
          List.filter_map ~f:filter_to_loc in
        Format.fprintf ppf "new %s(%a, [%a])" class_name
          (format_list Format.pp_print_string ", ") args
          (format_list Format.pp_print_string ", ") err_locs'
    | _ ->
        Format.fprintf ppf "new LocalError('%a', %s)"
          EzyErrors.print_error_desc err
          (loc_string loc)

let print_heavy ppf : (Location.t * heavy_error) -> unit = function
  | _, Error_as_heavy (loc, error) ->
      print_error ppf (loc, error)
  | loc, heavy ->
      Format.fprintf ppf "new LocalError('%a', %s)"
        EzyErrors.print_heavy_error_desc heavy
        (loc_string loc)

let print_fatal ppf (loc, fatal) =
  Format.fprintf ppf "new LocalError('%a', %s)"
    EzyErrors.print_fatal_error_desc fatal
    (loc_string loc)
  

exception Found of Location.t
let name_string = "'codeitem'"
let print_program locs ppf code =

  let find_open_locs i =
    let fitting = LocationSet.filter (fun loc -> loc.Location.loc_start.Lexing.pos_cnum = i) locs in
    let compare loc1 loc2 =
      loc2.Location.loc_end.Lexing.pos_cnum - loc1.Location.loc_end.Lexing.pos_cnum in
    List.sort compare (LocationSet.elements fitting) in

  let count_closing_locs i =
    let fitting = LocationSet.filter (fun loc -> loc.Location.loc_end.Lexing.pos_cnum = i) locs in
    LocationSet.cardinal fitting in

  let print_open_tag ppf loc =
    Format.fprintf ppf "<span name=%s id=%s>" name_string (loc_string loc) in

  for i = 0 to String.length code - 1 do
    for i = 1 to count_closing_locs i do
      Format.pp_print_string ppf "</span>" 
    done ;
    List.iter (print_open_tag ppf) (find_open_locs i) ;
    Format.pp_print_string ppf begin
      match code.[i] with
        | ' ' -> "&nbsp;"
        | '&' -> "&amp;"
        | '>' -> "&gt;"
        | '<' -> "&lt;"
        | '\n' -> "<br />"
        | c -> String.make 1 c
    end ;
  done

let name = "Html error reporting"

let print_program_aux ppf (ast, program) =
  print_program (EzyAst.CollectLocs.structure ast) ppf (Lazy.force program)

let safe_print p ppf x =
(*   let str = print_to_to_string p x in *)
  p ppf x

let print_errors' ~program ast ppf errors =
  Format.fprintf ppf (template ())
    print_program_aux (ast, program)
    (format_list print_error ", ") (ErrorSet.elements errors)

let print_heavies' ~program ast ppf heavies =
  Format.fprintf ppf (template ())
    print_program_aux (ast, program)
    (format_list print_heavy ", ") (HeavyErrorSet.elements heavies)

let print_fatal' ~program loc ppf fatal =
  Format.fprintf ppf (template ())
    (print_program (LocationSet.singleton loc)) (Lazy.force program)
    (safe_print print_fatal) (loc, fatal)

let () = EzyErrors.register name print_errors' print_heavies' print_fatal'
