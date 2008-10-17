open EzyUtils

let log = new Logger.logger "ezyParser"


type file_parser = EzyFeatures.program_feats -> string -> string -> Parsetree.structure
type phrase_parser = EzyFeatures.program_feats -> Lexing.lexbuf -> Parsetree.toplevel_phrase


let file_ref : file_parser ref =
  ref begin fun _ inputfile _ ->
    let ic = open_in_bin inputfile in
    seek_in ic 0;
    Location.input_name := inputfile;
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf inputfile;
    Parse.implementation lexbuf
  end

let phrase_ref : phrase_parser ref =
  ref (const Parse.toplevel_phrase)


let file () = !file_ref

let phrase () = !phrase_ref


module type PARSER = sig
  val id: string
  val file: file_parser
  val phrase: phrase_parser
end

module Register (Parser: PARSER) = struct
  let _ =
    log#info "Registering parser %s" Parser.id ;
    file_ref := Parser.file ;
    phrase_ref := Parser.phrase
end

let () = let module M = Register (EzyCamlgrammar.M) in ()
