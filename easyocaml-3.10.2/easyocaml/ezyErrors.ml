open EzyOcamlmodules
open EzyTypingCoreTypes
open EzyUtils.Infix
open EzyUtils
open EzyAst

let logger = new Logger.logger "ezyErrors"

type type_error =
  | ConstructorClash of Ty.t * Ty.t
  | ArityClash of Ty.t * Ty.t * int * int
  | CircularType of Ty.t * Ty.t
type error =
  | Type_error of type_error * ExtLocationSet.t
  | Unbound_variable of Longident.t
  | Mutable_content of Longident.t
  | Missing_fields of Longident.t * string list (* [Missing_fields (lid, strs)] the fields [strs] are missing for the record of type [lid] *)
  | Unknown_field of Longident.t
  | No_subtype of Ty.t * Parsetree.core_type
type import_error =
  | Not_supported_expression of Parsetree.expression_desc
  | Not_supported_constant
  | Not_supported_core_type
  | Not_supported_structure_item of Parsetree.structure_item_desc
  | Not_supported_pattern of Parsetree.pattern_desc
  | Not_supported_type_declaration of Parsetree.type_declaration
type record_heavy =
  | Alien_fields of Path.t * (Longident.t * Path.t) list
    (** Fields from different records are intermixed *)
  | Unknown_fields of Longident.t list 
    (** Fields of a record construction are unknown *)
  | Fields_bound_several_times of Longident.t list
  | No_fields
    (** No fields are given in a record construction. Syntactically possible? *)
type heavy_error =
  | Several_bindings of string list
  | Different_bindings of string list * string list
  | Invalid_record of record_heavy list
  | Unknown_variant of Longident.t
  | Type_variables_not_generalized of string * Ty.t
  | Type_names_not_unique of string list
  | Invalid_variant_construction of Longident.t * int * int
  | Invalid_type_constructor of Longident.t * int * int (* name of the ctor + expected argument count + applied argument count *)
  | Unbound_type_constructor of Longident.t
  | Error_as_heavy of (Location.t * error)
type fatal = 
  | Syntax_error of exn option
  | Import_error of import_error
  | Module_not_found of Longident.t
  | Other_fatal of string

(* Errors {{{ *)

(** Errors which permit further processing. *)
module Error = struct
  type t = Location.t * error
  type printable = t
  let print ppf _ = failwith "no printer for Error.t"
  let compare =
    lexical2 Pervasives.compare Pervasives.compare
end
module ErrorSet = Set.Make(Error)

(** Errors which stop any further generation, unification 
    or any other processes 
*)
module HeavyError = struct
  type t = Location.t * heavy_error
  type printable = t
  let print ppf _ = failwith "no printer for Error.t"
  let compare x y =
    lexical2 Pervasives.compare Pervasives.compare x y
  let from_error : Error.t -> t = fun (loc, err) ->
    Location.none, Error_as_heavy (loc, err)
end

module HeavyErrorSet = struct
  include Set.Make(HeavyError)
  let add_errors errs heavies =
    let aux err sofar = add (HeavyError.from_error err) sofar in
    ErrorSet.fold aux errs heavies
end

(* }}} *)

open Format

type lang = [ `En | `Fr | `De ]

let lang : lang =
  let default = `En in
  let lang_specs = ["en", `En; "fr", `Fr; "de", `De] in
  let rec decode lang_code = function
    | (prefix, lang) :: rem ->
        if lang_code = prefix || String.check_prefix lang_code (prefix ^ "_")
        then lang
        else decode lang_code rem
    | [] -> raise Not_found in
  let rec find = function
    | [] ->  default
    | var :: rem ->
        begin try
          decode (Sys.getenv var) lang_specs
        with Not_found ->
          find rem
        end in
  find ["LANGUAGE"; "LANG"]

let _ = logger#info "Selected language: %s" (match lang with `En -> "en" | `Fr  -> "fr" | `De -> "de")

let rough_ty_print =
  match lang with
    | `En -> begin fun ppf -> function
        | Ty.Var _  as ty ->
            Ty.print ppf ty
        | Ty.Tuple (label, _) ->
            Format.fprintf ppf "@[tuple@]"
        | Ty.Arrow (label, _, _) ->
            Format.fprintf ppf "@[function@]"
        | Ty.Constr (label, path, args) ->
            Format.fprintf ppf "@[%s@]" (Path.name path)
      end 
    | `Fr -> not_implemented "EzyErrors.rough_ty_print (french)" 
    | `De -> begin fun ppf -> function
        | Ty.Var _ as ty ->
            Ty.print ppf ty
        | Ty.Tuple (label, _) ->
            Format.fprintf ppf "@[Tupel@]" 
        | Ty.Arrow (label, _, _) ->
            Format.fprintf ppf "@[Funktion@]" 
        | Ty.Constr (label, path, args) ->
            Format.fprintf ppf "@[%s@]" (Path.name path)
      end

let slice ppf =
  Format.pp_print_string ppf
    (match lang with
       | `En | `De -> "Slice"
       | `Fr -> not_implemented "EzyErrors.ezyErrors.slice (french)")

let at =
  match lang with
    | `En -> (fun ppf -> Format.pp_print_string ppf "at")
    | `De -> (fun ppf -> Format.pp_print_string ppf "an der Stelle")
    | `Fr -> not_implemented "EzyErrors.at (french)"


let print_tuple = 
  match lang with
    | `En -> begin fun ppf -> function
        | 1 -> Format.pp_print_string ppf "single value"
        | 2 -> Format.pp_print_string ppf "pair"
        | 3 -> Format.pp_print_string ppf "triple"
        | n -> Format.fprintf ppf "%d-tuple" n
      end
    | `Fr -> not_implemented "EzyErrors.print_tuple (french)"
    | `De -> begin fun ppf -> function
        | 1 -> Format.pp_print_string ppf "einzelner Wert"
        | 2 -> Format.pp_print_string ppf "Paar"
        | 3 -> Format.pp_print_string ppf "Triple"
        | 4 -> Format.pp_print_string ppf "Quartupel"
        | n -> Format.fprintf ppf "%d-Tupel" n
      end

let similar_types = function
  | Ty.Tuple (_, args1), Ty.Tuple (_, args2) ->
      List.length args1 = List.length args2
  | Ty.Constr (_, p1, _), Ty.Constr (_, p2, _) ->
      Path.same p1 p2
  | Ty.Arrow _, Ty.Arrow _ ->
      true
  | _ -> false
          
let print_type_error_desc =
  match lang with
    | `En -> begin fun ppf -> function
        | ConstructorClash (ty1, ty2) (* when similar_types (ty1, ty2) ->
            Format.fprintf ppf "Type constructor clash between %a and %a"
              Ty.print ty1 Ty.print ty2
        | ConstructorClash (ty1, ty2) *) ->
            Format.fprintf ppf "Type constructor clash between %a and %a"
              rough_ty_print ty1 rough_ty_print ty2
        | ArityClash (ty1, ty2, ar1, ar2) ->
            Format.fprintf ppf "Arity clash, %a vs. %a"
              print_tuple ar1 print_tuple ar2
        | CircularType (ty1, ty2) ->
            Format.fprintf ppf "Circular type, %a vs. %a"
              Ty.print ty1 Ty.print ty2 
      end
    | `Fr -> not_implemented "EzyErrors.print_type_error_desc (french)"
    | `De -> begin fun ppf -> function
        | ConstructorClash (ty1, ty2) when similar_types (ty1, ty2) ->
            Format.fprintf ppf "Kollision zwischen Werten der Typen %a und %a"
              Ty.print ty1 Ty.print ty2
        | ConstructorClash (ty1, ty2) ->
            Format.fprintf ppf "Kollision zwischen Werten der Typen %a und %a"
              rough_ty_print ty1 rough_ty_print ty2
        | ArityClash (ty1, ty2, ar1, ar2) ->
            Format.fprintf ppf "Ein %a anstatt eines %a"
              print_tuple ar1 print_tuple ar2
        | CircularType (ty1, ty2) ->
            Format.fprintf ppf "Zirkulaerer typ: %a vs. %a"
              Ty.print ty1 Ty.print ty2 
      end

let print_error_desc =
  match lang with
    | `En -> begin fun ppf -> function
        | Type_error (err, locs) ->
            Format.fprintf ppf "Type error"
        | Missing_fields (tyctor, lis) ->
            Format.fprintf ppf "The fields %a for record type %a are not defined"
              (format_list Format.pp_print_string ", ") lis Longident.print tyctor
        | Unknown_field lid ->
            Format.fprintf ppf "There is no record with a field named %a"
              Longident.print lid          
        | Unbound_variable lid ->
            Format.fprintf ppf "The variable %a is unbound"
              Longident.print lid
        | Mutable_content lid ->
            Format.fprintf ppf "The record field %a is not mutable"
              Longident.print lid
        | No_subtype (tx, ty) ->
            Format.fprintf ppf "This expression of type %a cannot be constrained to type %a"
              Ty.print tx EzyAst.print_core_type ty
      end
    | `De -> begin fun ppf -> function
        | Type_error (err, locs) ->
            Format.fprintf ppf "Typfehler"
        | Missing_fields (tyctor, lis) ->
            Format.fprintf ppf "Die Feld(er) %a fehlen fuer die Konstruktion eines Records vom Typ %a"
              (format_list Format.pp_print_string ", ") lis Longident.print tyctor
        | Unknown_field lid ->
            Format.fprintf ppf "Feldname %a ist unbekannt"
              Longident.print lid          
        | Unbound_variable lid ->
            Format.fprintf ppf "Die Variable %a ist unbekannt"
              Longident.print lid
        | Mutable_content lid ->
            Format.fprintf ppf "Das Record Feld %a ist nicht aenderbar (mutable)"
              Longident.print lid
        | No_subtype (tx, ty) ->
            Format.fprintf ppf "Ungueltige Typannotation fuer einen Wert vom Typ %a"
              Ty.print tx
      end
    | `Fr -> not_implemented "EzyErrors.print_error (french)"

let print_error ?program ast ppf (loc, error) =
   match error with
    | Type_error (err, locs) ->
        let sliced_ast = EzyErrorReportUtils.create_slices ast locs in
        Format.fprintf ppf "@[<3> * %a: %a@\n%t:@ %a@]"
          print_error_desc error
          print_type_error_desc err
          slice
          (fun ppf -> List.iter (EzyAst.print_structure_item () ppf)) sliced_ast
    | _ ->
        Format.fprintf ppf "@[<3> * %a@ (%t %a)@]"
          print_error_desc error at Location.print loc

let print_record_heavy =
  let pp = format_list Longident.print ", " in
  match lang with
    | `En | `De -> begin fun ppf -> function
        | Alien_fields (p1, lis) ->
            let aux ppf (lid, p2) =
              Format.fprintf ppf "field %a belongs to %s" Longident.print lid (Path.name p2) in
            Format.fprintf ppf "@[<3> * The field does not belong to type %s: %a"
              (Path.name p1) (format_list aux ", ") lis
        | Unknown_fields lis ->
            Format.fprintf ppf "@[<3> * The field(s) %a are unknown"
              pp lis
        | Fields_bound_several_times fs ->
            Format.fprintf ppf "@[<3> * The field(s) %a are bound several times"
              pp fs
        | No_fields ->
            Format.fprintf ppf "@[<3> * Not a single fields of this record is defined"
      end
    | `Fr -> not_implemented "EzyErrors.print_record_heavy (french)"

let print_heavy_error_desc =
  match lang with
    | `En | `De -> begin fun ppf -> function
        | Several_bindings names ->
            Format.fprintf ppf "The variables %a are bound several times"
              (format_list Format.pp_print_string ", ") names
        | Different_bindings ([], only_right) ->
            Format.fprintf ppf "The variables %a are only bound on the right hand side of the or pattern"
              (format_list Format.pp_print_string ", ") only_right
        | Different_bindings (only_left, []) ->
            Format.fprintf ppf "The variables %a are only bound on the left hand side of the or pattern"
              (format_list Format.pp_print_string ", ") only_left
        | Different_bindings (only_left, only_right) ->
            Format.fprintf ppf "The variables %a are only bound on the left hand side of the or pattern and %a are only bound on the right hand side"
              (format_list Format.pp_print_string ", ") only_left
              (format_list Format.pp_print_string ", ") only_right
        | Invalid_variant_construction (lid, expected, real) ->
            Format.fprintf ppf "The constructor %a expexts %d argument(s)@ but is here applied to %d argument(s)"
              Longident.print lid expected real
        | Invalid_record errs ->
            Format.fprintf ppf "Errors in record definition:@ @[%a@]"
              (format_list print_record_heavy "@ ") errs
        | Unknown_variant lid ->
            Format.fprintf ppf "Unbound constructor %a" Longident.print lid
        | Type_variables_not_generalized (nm, _) ->
            Format.fprintf ppf "Variable %s is monomorph and contains type variables (thus cannot be generalized)" nm
        | Type_names_not_unique nms ->
            Format.fprintf ppf "The type names %a are not unique in this module" (format_list Format.pp_print_string ", ") nms
        | Invalid_type_constructor (lid, n, m) ->
            Format.fprintf ppf "The type constructor %a expects %d argument(s), but is here applied to %d argument(s)"
              Longident.print lid n m
        | Unbound_type_constructor lid ->
            Format.fprintf ppf "Unbound type constructor %a" Longident.print lid
        | Error_as_heavy err ->
            failwith "print_heavy_error_desc"
      end
    | `Fr -> not_implemented "EzyErrors.print_heavy (french)"

let print_heavy ?program ast ppf (loc, heavy) =
  match heavy with
    | Error_as_heavy error ->
        print_error ?program ast ppf error
    | _ ->
      Format.fprintf ppf "@[<3> * %a@ (%t %a)@]"
        print_heavy_error_desc heavy at Location.print loc

let print_errors_default ?program ast ppf errors =
  Format.fprintf ppf "@[%a@]"
    (format_list (print_error ?program ast) "@\n")
    (ErrorSet.elements errors)

let print_heavies_default ?program ast ppf heavies =
  Format.fprintf ppf "@[%a@]"
    (format_list (print_heavy ?program ast) "@\n")
    (HeavyErrorSet.elements heavies)

let print_fatal_error_desc =
  match lang with
    | `En | `Fr | `De -> begin fun ppf -> function
        | Import_error err ->
            Format.fprintf ppf "This feature is not supported by EasyOcaml (in this language level)" 
        | Module_not_found lid ->
            Format.fprintf ppf "Module %a not found." Longident.print lid
        | Syntax_error None ->
            Format.fprintf ppf "Syntax error"
        | Syntax_error (Some (Stream.Error err)) ->
            Format.fprintf ppf "Parse error: %s" err
        | Syntax_error (Some exn) ->
            Format.fprintf ppf "Parse error: %a" Camlp4.ErrorHandler.print exn
        | Other_fatal msg ->
            Format.pp_print_string ppf msg
        end

let long_print_loc ppf loc =
  let print_details ppf =
    let col pos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
    if loc.Location.loc_start.Lexing.pos_lnum = loc.Location.loc_end.Lexing.pos_lnum then
      (match lang with
         | `En | `Fr -> Format.fprintf ppf "line %d characters %d-%d"
         | `De -> Format.fprintf ppf "Zeile %d Zeichen %d-%d"
      ) loc.Location.loc_start.Lexing.pos_lnum
        (col loc.Location.loc_start)
        (col loc.Location.loc_end)
  else
      (match lang with
         | `En | `Fr -> Format.fprintf ppf "line %d characters %d - line %d characters %d"
         | `De -> Format.fprintf ppf "Zeile %d Zeichen %d - Zeile %d Zeichen %d"
      ) loc.Location.loc_start.Lexing.pos_lnum
        (col loc.Location.loc_start)
        loc.Location.loc_end.Lexing.pos_lnum
        (col loc.Location.loc_end) in
  if loc.Location.loc_start.Lexing.pos_fname = "" then
    print_details ppf 
  else
    (match lang with
       | `En | `Fr -> Format.fprintf ppf "file: %s: %t"
       | `De -> Format.fprintf ppf "Datei: %s: %t"
    ) loc.Location.loc_start.Lexing.pos_fname print_details 

let print_fatal_default ?program loc ppf fatal =
  Format.fprintf ppf "@[<3> * " ;
  if loc <> Location.none then
    (match lang with
       | `En | `Fr -> Format.fprintf ppf "@[At %a@]@ "
       | `De -> Format.fprintf ppf "@[An %a@]@ "
    ) long_print_loc loc ;
  Format.fprintf ppf "%a@]" print_fatal_error_desc fatal


let print_errors_ref = ref print_errors_default
let print_heavies_ref = ref print_heavies_default
let print_fatal_ref = ref print_fatal_default

let check = ref (fun ppf -> Format.fprintf ppf "EzyErrors.check@.")

let print_errors () = !print_errors_ref
let print_heavies () = !print_heavies_ref
let print_fatal () = !print_fatal_ref


module type ERROR_REPORTER = sig
  val name : string
  val print_errors : ?program:(string lazy_t) -> EzyAst.imported_structure -> formatter -> ErrorSet.t -> unit
  val print_heavies : ?program:(string lazy_t) -> EzyAst.imported_structure -> formatter -> HeavyErrorSet.t -> unit
  val print_fatal : ?program:(string lazy_t) -> Location.t -> formatter -> fatal -> unit
end

module Register (ErrorReporter : ERROR_REPORTER) = struct
  logger#info "Loading error reporting plugin %s" ErrorReporter.name ;
  print_errors_ref := ErrorReporter.print_errors ;
  print_heavies_ref := ErrorReporter.print_heavies ;
  print_fatal_ref := ErrorReporter.print_fatal ;
end
              
type some_errors =
  | Errors of ErrorSet.t
  | Heavies of HeavyErrorSet.t

type annotated_errors = {
  errors: some_errors ;
  ast: EzyAst.imported_structure ;
  program: string lazy_t option ;
}

exception AnnotatedError of annotated_errors
exception Fatal of Location.t option * string lazy_t option * fatal

let raise_fatal ?loc ?program s =
  raise (Fatal (loc, program, s))
