
(** This modules gives access to easyocaml's errors and their reporting.
  * 
  * In this module, there are three kinds of errors declared:
  *   [Fatal errors] occurr only singular, because they prohibit any further
  *       processing. 
  *   [Heavy errors] can be generated while EzyGenerate.for_(expr|strit) runs. They
  *       are reported between constraint generation and unification, if any.
  *   [(Common) errors] are generated while constraint generation, unification and
  *       error enumeration. They are reported after error enumeration.
  *  
  * All errors have optional access to the program code (as an [string lazy_t option])
  * and heavy and common errors are annotated with the program code as well as the
  * imported AST (as EzyAst.structure).
  *
  * One can define a new error reporter by writing a module which calls
  * EzyErrors.set_printer with appropriate error reporting functions. This is than
  * dynamically linked into the easyocaml compiler/toploop with the -easyerrorprinter
  * flag. The appropriate function is called on fatal, heavy and common errors.
  *)

type lang = [ `En | `Fr | `De ]
val lang : lang

type type_error =
  | ConstructorClash of EzyTypingCoreTypes.Ty.t * EzyTypingCoreTypes.Ty.t
  | ArityClash of EzyTypingCoreTypes.Ty.t * EzyTypingCoreTypes.Ty.t * 
      int * int
  | CircularType of EzyTypingCoreTypes.Ty.t * EzyTypingCoreTypes.Ty.t


(** Normal errors are collected while constraint generation and constraint unification.
  *)
type error =
  | Type_error of type_error * EzyOcamlmodules.ExtLocationSet.t
    (** A type error. The location of the error in the error set is none. *)
  | Unbound_variable of EzyOcamlmodules.Longident.t
    (** Usage of an unbound variable. *)
  | Mutable_content of EzyOcamlmodules.Longident.t
    (** For [x.f <- y] where f is not mutable. *)
  | Missing_fields of EzyOcamlmodules.Longident.t * string list
    (** Missing field in record definition. *)
  | Unknown_field of Longident.t
    (** Unknown field in record definition or reading/writing an unknown field. *)
  | No_subtype of EzyTypingCoreTypes.Ty.t * Parsetree.core_type
    (** Invalid type annotation (e.g. [(e : t)] where [t] is not a subtype of the type of expression [e]) *)


(** Information abount invalid record definitions. *)
type record_heavy =
  | Alien_fields of EzyOcamlmodules.Path.t *
      (EzyOcamlmodules.Longident.t * EzyOcamlmodules.Path.t) list
    (** [Alien_fields (p, fs)]: In the record construction of record type [p], the fields
      * in the association list [fs] are from record types given by the values of the assoc list.
      * The actual type of a record construction is given by the type of the _first_ field. *)
  | Unknown_fields of EzyOcamlmodules.Longident.t list
    (** [Unknown_fields fs]: The fields [fs] are unkown in a record construction. *)
  | Fields_bound_several_times of EzyOcamlmodules.Longident.t list
    (** [Fields_bound_several_times fs]: The fields [fs] are bound several times in a record construction. *)
  | No_fields
    (** An empty record definition (syntactically impossible). *)


(** Heavy errors are collected while constraint generation and yield to the termination
  * of compilation before unification. Normal errors are reported along.
  *)
type heavy_error =
  | Several_bindings of string list
    (** [Several_bindings vs]: [vs] is a list of variables which occur several times in one pattern *)
  | Different_bindings of string list * string list
    (** [Different_bindings (vs1, vs2)]: In an or pattern (p1 | p2), variables [vs1] only occur in [p1],
      * and [vs2] occur only in [p2].  *)
  | Invalid_record of record_heavy list
    (** An invalid record definition with a not empty list of [record_heavy] errors. *)
  | Unknown_variant of EzyOcamlmodules.Longident.t
    (** Occurence of an unknown variant constructor. *)
  | Type_variables_not_generalized of string * EzyTypingCoreTypes.Ty.t
    (** [Type_variables_not_generalized [v, ty]: The top level variable [v] has inferred type [ty]
      * which contains a free monomorphic type variable. *)
  | Type_names_not_unique of string list
    (** [Type_names_not_unique ts]: types with names [ts] are defined several times. *)
  | Invalid_variant_construction of EzyOcamlmodules.Longident.t * int * int
    (** [Invalid_variant_construction (lid, n, m)]: Construction of a variant with name [lid], but with
      * [m] arguments instead of the expected [n] arguments. *)
  | Invalid_type_constructor of EzyOcamlmodules.Longident.t * int * int
    (** [Invalid_type_constructor (lid, n, m)]: In a type description, type constructor [lid] expects [n]
      * parameters but is used with [m] parameters. *)
  | Unbound_type_constructor of EzyOcamlmodules.Longident.t
    (** Usage of an unbound type constructor. *)
  | Error_as_heavy of (EzyOcamlmodules.Location.t * error)
    (** The set of all occuring normal errors. The location of the heavy error in the heavy error set is none. *)


type import_error =
  | Not_supported_expression of Parsetree.expression_desc
  | Not_supported_constant
  | Not_supported_core_type
  | Not_supported_structure_item of Parsetree.structure_item_desc
  | Not_supported_pattern of Parsetree.pattern_desc
  | Not_supported_type_declaration of Parsetree.type_declaration

(** Fatal errors terminate compilation directly and are reported single.
  *)
type fatal =
  | Parse_error of EzyCamlgrammar.ParseError.t
    (** Syntax error, sometimes including the parsers original exception. *)
  | Import_error of import_error
    (** [Import_error (b, ie)]: Actually valid OCaml code, but either not allowed by
      * language features [b = false] or not allowed in EasyOCaml at all [b = true]. *)
  | Module_not_found of EzyOcamlmodules.Longident.t
    (** Usage of an unknown module. *)
  | Other_fatal of string
    (** Different errors which should not occur in a correctly configured system. *)

val raise_fatal : ?loc:EzyOcamlmodules.Location.t -> ?program:string lazy_t -> fatal -> 'a

(** Some i18n'ed error printing code as a simple default. *)
val print_type_error_desc : Format.formatter -> type_error -> unit
val print_error_desc : Format.formatter -> error -> unit
val print_heavy_error_desc : Format.formatter -> heavy_error -> unit
val print_fatal_error_desc : Format.formatter -> fatal -> unit

open EzyUtils

module Error : PrintableOrderedType with type t = EzyOcamlmodules.Location.t * error
module ErrorSet : Set.S with type elt = Error.t
module HeavyError : sig
  include PrintableOrderedType with type t = EzyOcamlmodules.Location.t * heavy_error
  val from_error : Error.t -> t
end
module HeavyErrorSet : sig
  include Set.S with type elt = HeavyError.t
  val add_errors : ErrorSet.t -> t -> t
end


(** To register arbitrary error reporting code, one should implement a module with signature [ERROR_REPORTER] and
  * pass it to [Register].
  * This may happen in some module which is given to the compiler with flat -easyerrorprinter *)
module type ERROR_REPORTER = sig
  open Format
  val name : string
  val print_errors : ?program:(string lazy_t) -> EzyAst.imported_structure -> formatter -> ErrorSet.t -> unit
  val print_heavies : ?program:(string lazy_t) -> EzyAst.imported_structure -> formatter -> HeavyErrorSet.t -> unit
  val print_fatal : ?program:(string lazy_t) -> Location.t -> formatter -> fatal -> unit
end
module Register (Reporter: ERROR_REPORTER) : sig end

(**/**)

val print_errors :
  unit -> ?program:string lazy_t -> EzyAst.imported_structure -> Format.formatter -> ErrorSet.t -> unit
val print_heavies :
  unit -> ?program:string lazy_t -> EzyAst.imported_structure -> Format.formatter -> HeavyErrorSet.t -> unit
val print_fatal :
  unit -> ?program:string lazy_t -> EzyOcamlmodules.Location.t -> Format.formatter -> fatal -> unit

type some_errors = Errors of ErrorSet.t | Heavies of HeavyErrorSet.t
type annotated_errors = {
  errors : some_errors;
  ast : EzyAst.imported_structure;
  program : string lazy_t option;
}
exception AnnotatedError of annotated_errors
exception Fatal of EzyOcamlmodules.Location.t option * string lazy_t option * fatal

val check : (Format.formatter -> unit) ref