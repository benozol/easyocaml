open EzyUtils;

open Camlp4_import.Parsetree;
open Lexing;
open Camlp4;

(*pp camlp4oof *)
value logger = new EzyUtils.Logger.logger "ezy_gram";

module Id = struct
  value name = "EzyCamlgrammar";
  value version = "0.1";
end;

module Restrict (Spec: sig value spec : EzyFeatures.program_feats; end) (Syntax: Sig.Camlp4Syntax) : sig end = struct

  open EzyFeatures;
  open Camlp4.Sig;
  open Syntax;
  open Spec;

  do {
    logger#info "Restricting by EzyCamlgrammar";

    let (pat_spec, fun_spec, let_spec, letrec_spec, expr_spec, type_spec, str_spec, spec) = maximum spec;

    (* DELETE_RULE Gram patt: `ANTIQUOT (("" | "pat" | "anti"), _) END;
    DELETE_RULE Gram patt: `ANTIQUOT (("" | "pat" | "anti"), _) END;
    DELETE_RULE Gram patt: `ANTIQUOT ("tup", _) END;
    DELETE_RULE Gram patt: `ANTIQUOT ("`bool", _) END;
    DELETE_RULE Gram patt: `QUOTATION _ END; *)
    DELETE_RULE Gram patt: "`"; a_ident END;
    DELETE_RULE Gram patt: "#"; type_longident END;

    DELETE_RULE Gram patt: SELF; "as"; a_LIDENT END;
    DELETE_RULE Gram patt: SELF; "|"; SELF END;
    DELETE_RULE Gram patt: SELF; ","; LIST1 NEXT SEP "," END;
    DELETE_RULE Gram patt: SELF; "::"; SELF END;
    (* No access, because this is defined in Camlp4OCamlParser
    DELETE_RULE Gram patt: patt_constr; SELF END;
    DELETE_RULE Gram patt: patt_constr END; *)
    DELETE_RULE Gram patt: "-"; a_INT END;
    DELETE_RULE Gram patt: "-"; a_INT32 END;
    DELETE_RULE Gram patt: "-"; a_INT64 END;
    DELETE_RULE Gram patt: "-"; a_NATIVEINT END;
    DELETE_RULE Gram patt: "-"; a_FLOAT END;
    DELETE_RULE Gram patt: "false" END;
    DELETE_RULE Gram patt: "true" END;
    DELETE_RULE Gram patt: "["; "]" END;
    DELETE_RULE Gram patt: "["; sem_patt_for_list; "::"; SELF; "]" END;
    DELETE_RULE Gram patt: "["; sem_patt_for_list; "]" END;
    DELETE_RULE Gram patt: "[|"; "|]" END;
    DELETE_RULE Gram patt: "[|"; sem_patt; "|]" END;
    DELETE_RULE Gram patt: "{"; label_patt; "}" END;
    DELETE_RULE Gram patt: "("; ")" END;
    DELETE_RULE Gram patt: "("; SELF; ":"; ctyp; ")" END;
    DELETE_RULE Gram patt: "("; SELF; ")" END;
    DELETE_RULE Gram patt: "_" END;
    DELETE_RULE Gram patt: ident END;
    DELETE_RULE Gram patt: a_INT END;
    DELETE_RULE Gram patt: a_INT32 END;
    DELETE_RULE Gram patt: a_INT64 END;
    DELETE_RULE Gram patt: a_NATIVEINT END;
    DELETE_RULE Gram patt: a_FLOAT END;
    DELETE_RULE Gram patt: a_STRING END;
    DELETE_RULE Gram patt: a_CHAR; ".."; a_CHAR END;
    DELETE_RULE Gram patt: a_CHAR END;

    DELETE_RULE Gram expr: "let"; "module"; a_UIDENT; module_binding0; "in"; expr LEVEL ";" END;
    DELETE_RULE Gram expr: "object"; opt_class_self_patt; class_structure; "end" END;
    DELETE_RULE Gram expr: "lazy"; SELF END;
    DELETE_RULE Gram expr: `LABEL _; SELF END;
    DELETE_RULE Gram expr: `OPTLABEL _; SELF END;
    DELETE_RULE Gram expr: "new"; class_longident END;
    (* DELETE_RULE Gram expr: `QUOTATION _ END;
    DELETE_RULE Gram expr: `ANTIQUOT (("exp" | "" | "anti"), _) END;
    DELETE_RULE Gram expr: `ANTIQUOT ("`bool", _) END;
    DELETE_RULE Gram expr: `ANTIQUOT ("tup", _) END;
    DELETE_RULE Gram expr: `ANTIQUOT ("seq", _) END; *)
    DELETE_RULE Gram expr: "`"; a_ident END;
    DELETE_RULE Gram expr: "{<"; ">}" END;
    DELETE_RULE Gram expr: "{<"; field_expr; ">}" END;
    DELETE_RULE Gram expr: "("; SELF; ":"; ctyp; ":>"; ctyp; ")" END;
    DELETE_RULE Gram expr: "("; SELF; ":>"; ctyp; ")" END;

    DELETE_RULE Gram expr: SELF; ";" END;
    DELETE_RULE Gram expr: "let"; opt_rec; binding; "in"; expr LEVEL ";" END;
    DELETE_RULE Gram expr: "function"; match_case END;
    DELETE_RULE Gram expr: "if"; SELF; "then"; expr LEVEL "top"; "else"; expr LEVEL "top" END;
    DELETE_RULE Gram expr: "if"; SELF; "then"; expr LEVEL "top" END;
    DELETE_RULE Gram expr: "fun"; fun_def END;
    DELETE_RULE Gram expr: "match"; sequence; "with"; match_case END;
    DELETE_RULE Gram expr: "try"; sequence; "with"; match_case END;
    DELETE_RULE Gram expr: "for"; a_LIDENT; "="; sequence; direction_flag; sequence; "do"; do_sequence END;
    DELETE_RULE Gram expr: "while"; sequence; "do"; do_sequence END;
    DELETE_RULE Gram expr: SELF; ","; LIST1 NEXT SEP "," END;
    DELETE_RULE Gram expr: SELF; ":="; expr LEVEL "top" END;
    DELETE_RULE Gram expr: SELF; "<-"; expr LEVEL "top" END;
    (* No access, because this is defined in Camlp4OCamlRevisedParser ... 
    DELETE_RULE Gram expr: SELF; infixop6; SELF END;
    DELETE_RULE Gram expr: SELF; infixop5; SELF END;
    DELETE_RULE Gram expr: SELF; infix operator (level 0) (comparison operators, and some others); END;
    DELETE_RULE Gram expr: SELF; infix operator (level 1) (start with '^', '@'); SELF END;
    DELETE_RULE Gram expr: SELF; infix operator (level 2) (start with '+', '-'); SELF END;
    DELETE_RULE Gram expr: SELF; infix operator (level 3) (start with '*', '/', '%'); SELF END;
    DELETE_RULE Gram expr: SELF; infix operator (level 4) (start with "**") (right assoc); SELF END; *)
    (* DELETE_RULE Gram expr: SELF END; *)
    DELETE_RULE Gram expr: SELF; "::"; SELF END;
    DELETE_RULE Gram expr: SELF; "land"; SELF END;
    DELETE_RULE Gram expr: SELF; "lor"; SELF END;
    DELETE_RULE Gram expr: SELF; "lxor"; SELF END;
    DELETE_RULE Gram expr: SELF; "mod"; SELF END;
    DELETE_RULE Gram expr: SELF; "asr"; SELF END;
    DELETE_RULE Gram expr: SELF; "lsl"; SELF END;
    DELETE_RULE Gram expr: SELF; "lsr"; SELF END;
    DELETE_RULE Gram expr: "-"; SELF END;
    DELETE_RULE Gram expr: "-."; SELF END;
    DELETE_RULE Gram expr: SELF; SELF END;
    DELETE_RULE Gram expr: "assert"; SELF END;
    DELETE_RULE Gram expr: "~"; a_LIDENT END;
    DELETE_RULE Gram expr: "?"; a_LIDENT END;
    DELETE_RULE Gram expr: SELF; "."; "("; SELF; ")" END;
    DELETE_RULE Gram expr: SELF; "."; "["; SELF; "]" END;
    DELETE_RULE Gram expr: SELF; "."; "{"; comma_expr; "}" END;
    DELETE_RULE Gram expr: SELF; "."; SELF END;
    DELETE_RULE Gram expr: SELF; "#"; label END;
    DELETE_RULE Gram expr: "!"; SELF END;
    (* DELETE_RULE Gram expr: prefix operator (start with '!', '?', '~'); SELF END; *)
    DELETE_RULE Gram expr: "false" END;
    DELETE_RULE Gram expr: "true" END;
    (* defined in Oamlp4OCamlParser:
    DELETE_RULE Gram expr: "{"; test_label_eq; label_expr; "}" END; *)
    DELETE_RULE Gram expr: "{"; expr LEVEL "."; "with"; label_expr; "}" END;
    DELETE_RULE Gram expr: "["; "]" END;
    DELETE_RULE Gram expr: "["; sem_expr_for_list; "]" END;
    DELETE_RULE Gram expr: "[|"; "|]" END;
    DELETE_RULE Gram expr: "[|"; sem_expr; "|]" END;
    DELETE_RULE Gram expr: "("; ")" END;
    DELETE_RULE Gram expr: "("; SELF; ";"; sequence; ")" END;
    DELETE_RULE Gram expr: "("; SELF; ":"; ctyp; ")" END;
    DELETE_RULE Gram expr: "("; SELF; ")" END;
    DELETE_RULE Gram expr: "begin"; "end" END;
    DELETE_RULE Gram expr: "begin"; sequence; "end" END;
    DELETE_RULE Gram expr: a_INT END;
    DELETE_RULE Gram expr: a_INT32 END;
    DELETE_RULE Gram expr: a_INT64 END;
    DELETE_RULE Gram expr: a_NATIVEINT END;
    DELETE_RULE Gram expr: a_FLOAT END;
    DELETE_RULE Gram expr: a_STRING END;
    DELETE_RULE Gram expr: a_CHAR END;
    DELETE_RULE Gram expr: val_longident END;

    (* str_item *)
    DELETE_RULE Gram str_item: "let"; "module"; a_UIDENT; module_binding0; "in"; expr END;
    (* Camlp4OCamlRevisedParser:
    DELETE_RULE Gram str_item: "external"; a_LIDENT; ":"; ctyp; "="; string_list END; *)
    DELETE_RULE Gram str_item: "include"; module_expr END;
    DELETE_RULE Gram str_item: "module"; "rec"; module_binding END;
    DELETE_RULE Gram str_item: "module"; "type"; a_UIDENT; "="; module_type END;
    DELETE_RULE Gram str_item: "module"; a_UIDENT; module_binding0 END;
    DELETE_RULE Gram str_item: "class"; "type"; class_type_declaration END;
    DELETE_RULE Gram str_item: "class"; class_declaration END;
    (* DELETE_RULE Gram str_item: `ANTIQUOT (("" | "stri" | "anti" | "list"), _) END;
    DELETE_RULE Gram str_item: `QUOTATION _ END; *)

    DELETE_RULE Gram str_item: "exception"; constructor_declaration; "="; type_longident END;
    DELETE_RULE Gram str_item: "let"; opt_rec; binding; "in"; expr END;
    DELETE_RULE Gram str_item: "let"; opt_rec; binding END;
    DELETE_RULE Gram str_item: "exception"; constructor_declaration END;
    DELETE_RULE Gram str_item: "open"; module_longident END;
    DELETE_RULE Gram str_item: "type"; type_declaration END;
    DELETE_RULE Gram str_item: expr END;
  };
end;

   
(** The following function has nearly the same functionaly like Camlp4.Top: It sets parsing functions in Toplevel
    (here passed by ref because of module dependency reasons) to wrapped parsing functions of Syntax.Gram *)
module RegisterTop (Syntax: Sig.Camlp4Syntax) = struct
  open Camlp4.Sig;
  open Syntax;
  module Ast2pt = Camlp4.Struct.Camlp4Ast2OCamlAst.Make Ast;
  module Lexer = Camlp4.Struct.Lexer.Make Token;
  external not_filtered : 'a -> Gram.not_filtered 'a = "%identity";
  value register print_parse_error toploop_print_location toploop_parse_toplevel_phrase toploop_parse_use_file toploop_print_warning topdirs_dir_load topdirs_dir_directory =
    (* There is a few Obj.magic due to the fact that we no longer have compiler
       files like Parsetree, Location, Longident but Camlp4_import that wrap them to
       avoid name clashing. *)
    let module Toploop : sig
      value print_location :
        Format.formatter -> Camlp4_import.Location.t -> unit;
      value print_warning :
        Camlp4_import.Location.t -> Format.formatter -> Camlp4_import.Warnings.t -> unit;
      value parse_toplevel_phrase :
        ref (Lexing.lexbuf -> Camlp4_import.Parsetree.toplevel_phrase);
      value parse_use_file :
        ref (Lexing.lexbuf -> list Camlp4_import.Parsetree.toplevel_phrase);
    end = struct
      value print_location fmt loc =
        toploop_print_location fmt (Obj.magic loc);
      value parse_toplevel_phrase =
        Obj.magic toploop_parse_toplevel_phrase;
      value parse_use_file =
        Obj.magic toploop_parse_use_file;
      value print_warning loc fmt w =
        toploop_print_warning (Obj.magic loc) fmt (Obj.magic w);
    end in

    let wrap parse_fun =
      let token_stream_ref = ref None in
      fun lb ->
        let () = logger#trace "Wrapped parsing function called" in
        let token_stream =
          match token_stream_ref.val with
          [ None ->
            let not_filtered_token_stream = Lexer.from_lexbuf lb in
            let token_stream = Gram.filter (not_filtered not_filtered_token_stream) in
            do { token_stream_ref.val := Some token_stream; token_stream }
          | Some token_stream -> token_stream ]
        in try
          match token_stream with parser
          [ [: `(EOI, _) :] -> raise End_of_file
          | [: :] -> parse_fun token_stream ]
        with
        [ End_of_file | Sys.Break | (Loc.Exc_located _ (End_of_file | Sys.Break))
            as x -> let () = logger#debug "wrap recognized an error" in raise x
        | Gram.ParseError.E (loc, err) ->
            do {
              Stream.junk token_stream;
              logger#debug "catched! located parse error";
              print_parse_error Format.std_formatter loc err ;
              raise Exit
            }
        | x ->
            let () = Stream.junk token_stream in
            let x =
              match x with
              [ Loc.Exc_located loc x -> do { 
                Toploop.print_location Format.err_formatter
                  (Loc.to_ocaml_location loc);
                x }
              | x -> x ]
            in
            do {
              Format.eprintf "@[<0>%a@]@." Camlp4.ErrorHandler.print x;
              raise Exit
            } ] in

    let toplevel_phrase token_stream =
      match Gram.parse_tokens_after_filter Syntax.top_phrase token_stream with
      [ Some phr -> Ast2pt.phrase phr
      | None -> raise End_of_file ] in

    let use_file token_stream =
      let (pl0, eoi) =
        loop () where rec loop () =
          let (pl, stopped_at_directive) =
            Gram.parse_tokens_after_filter Syntax.use_file token_stream
          in
          if stopped_at_directive <> None then
            match pl with
            [ [ <:str_item< #load $str:s$ >> ] ->
                do { topdirs_dir_load Format.std_formatter s; loop () }
            | [ <:str_item< #directory $str:s$ >> ] ->
                do { topdirs_dir_directory s; loop () }
            | _ -> (pl, False) ]
          else (pl, True)
      in
      let pl =
        if eoi then []
        else
          loop () where rec loop () =
            let (pl, stopped_at_directive) =
              Gram.parse_tokens_after_filter Syntax.use_file token_stream
            in
            if stopped_at_directive <> None then pl @ loop () else pl
      in List.map Ast2pt.phrase (pl0 @ pl) in

    do {
      Toploop.parse_toplevel_phrase.val := wrap toplevel_phrase;
      Toploop.parse_use_file.val := wrap use_file;
      current_warning.val :=
        fun loc txt ->
          Toploop.print_warning (Loc.to_ocaml_location loc) Format.err_formatter
            (Camlp4_import.Warnings.Camlp4 txt);
      (* Cannot Camlp4.ErrorHandler.Register ParseError here, because control should
       * go back to Toploop with an Exit exception (and that would be handled by the
       * ErrorHandler ...) *)
    };
end;

module MakeOCaml (Unit: sig end) =
  Camlp4OCamlParser.Make
    (Camlp4OCamlRevisedParser.Make
       (Camlp4.OCamlInitSyntax.Make
          (PreCast.Ast)(PreCast.Gram)(PreCast.Quotation)));

module ECaml = MakeOCaml (struct end);
module ParseError = ECaml.Gram.ParseError; 

value import_loc : ECaml.Loc.t -> Location.t =
  fun loc -> Obj.magic (ECaml.Loc.to_ocaml_location loc);

value restrict_and_register_top spec
  print_parse_error toploop_print_location toploop_parse_toplevel_phrase
  toploop_parse_use_file toploop_print_warning topdirs_dir_load topdirs_dir_directory
=
  let module M = Restrict (struct value spec = spec; end) ECaml in
  let module RegTop = RegisterTop ECaml in
  RegTop.register print_parse_error toploop_print_location toploop_parse_toplevel_phrase toploop_parse_use_file toploop_print_warning topdirs_dir_load topdirs_dir_directory;


(* Actually, I want to throw [EzyErrors.Fatal] for parsing errors. This [E] is to break circular
 * dependencies between EzyCamlgrammar and EzyErrors. *)
exception E of EzyOcamlmodules.Location.t and lazy_t string and ParseError.error;

value restrict_and_parse_implem : EzyFeatures.program_feats -> string -> string -> Parsetree.structure =
  fun spec ->
    let module Syntax = ECaml in
    let module M = Restrict (struct value spec = spec; end) Syntax in
    (* let module Syntax = Camlp4.PreCast.Syntax in *)
    (* let module AstConversion = Camlp4.Struct.Camlp4Ast2OCamlAst.Make (Syntax.Ast) in *)
    let module AstConversion = MyCamlp4Ast2OCamlAst.Make Syntax.Ast in 
    let module Printer = Camlp4.Printers.OCamlr.Make Syntax in
    fun inputfile ast_impl_magic_number ->
      let program = lazy (
        let ic = open_in inputfile in
        between input_all ic (fun _ -> close_in ic)
      ) in
      let loc = Syntax.Loc.mk inputfile in
      let ic = open_in inputfile in
      try Misc.try_finally
        (fun () -> 
           do {
             let (ast, rest) = Syntax.Gram.parse Syntax.implem loc (Stream.of_channel ic);
             logger#info "Parsed tree: %a" (fun ppf -> List.iter (Printer.print_implem ~input_file:inputfile)) ast;
             (List.flatten (List.map AstConversion.str_item ast));
           })
        (fun () -> close_in ic)
      with
        [ ParseError.E (loc, err) ->
            raise (E (import_loc loc, program, err)) ];
