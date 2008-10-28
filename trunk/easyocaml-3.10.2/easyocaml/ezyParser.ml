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

let register (id, file_parser, phrase_parser) =
  log#info "Registering parser %s" id ;
  file_ref := file_parser ;
  phrase_ref := phrase_parser
