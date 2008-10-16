
(** This modules provides the interfaces for EasyOCaml's parser. One can
    register new parsers, because the actual Camlp4 parser must be dynamically
    linked into the compiler.
  
    Each parser must have the functions [file] and [phrase] as well as a short
    description [id] and one can register it by applying it to the functor
    [Register].
  *)

type file_parser = EzyFeatures.program_feats -> string -> string -> Parsetree.structure
type phrase_parser = EzyFeatures.program_feats -> Lexing.lexbuf -> Parsetree.toplevel_phrase

val file : unit -> file_parser
val phrase : unit -> phrase_parser

module type PARSER = sig
  val id: string
  val file: file_parser
  val phrase: phrase_parser
end
module Register (Parser: PARSER) : sig end
