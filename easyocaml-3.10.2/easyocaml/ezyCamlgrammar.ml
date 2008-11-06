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

    (* TODO [Gram.Entry.clear] all unnecessary entries from Camlp4/Sig.ml:1198 *)

    (* prune top_phrase *)
    (* DELETE_RULE Gram top_phrase: "#"; a_LIDENT; opt_expr; ";;" END ; *)

    (* prune implem *)
    DELETE_RULE Gram implem: "#"; a_LIDENT; opt_expr; semi END ; 
    if not str_spec.s_semisemi_optional then
      DELETE_RULE Gram semi: END 
    else ();
    (* FIXME DELETE_RULE Gram fun_def_cont: "when"; expr; "->"; expr END ; *)
    DELETE_RULE Gram opt_when_expr: "when"; expr END ;

    (* prune str_item *)
    (*  FIXME DELETE_RULE Gram str_item: "let"; "module"; a_UIDENT; module_binding0; "in"; expr END ; *)
    if Option.is_none expr_spec.e_let_in && Option.is_none expr_spec.e_let_rec_in then
      DELETE_RULE Gram str_item: "let"; opt_rec; binding; "in"; expr END 
    else ();
    if Option.is_none str_spec.s_let && Option.is_none str_spec.s_let_rec then
      DELETE_RULE Gram str_item: "let"; opt_rec; binding END
    else ();
    DELETE_RULE Gram str_item: "exception"; constructor_declaration; "="; type_longident END ; 
    (* FIXME DELETE_RULE Gram str_item: "external"; a_LIDENT; ":"; ctyp; "="; OCamlRevised.string_list END ; *)
    DELETE_RULE Gram str_item: "include"; module_expr END ;
    DELETE_RULE Gram str_item: "module"; "rec"; module_binding END ;
    DELETE_RULE Gram str_item: "module"; "type"; a_UIDENT; "="; module_type END ;
    DELETE_RULE Gram str_item: "module"; a_UIDENT; module_binding0 END ;
    if Option.is_none type_spec then
      DELETE_RULE Gram str_item: "type"; type_declaration END
    else ();
    DELETE_RULE Gram str_item: "class"; "type"; class_type_declaration END ;
    DELETE_RULE Gram str_item: "class"; class_declaration END ;
    (*   DELETE_RULE Gram str_item: `ANTIQUOT (("" | "stri" | "anti" | "list"), _) END ;  *)
    (*   DELETE_RULE Gram str_item: `QUOTATION _ END ;  *)
    if not str_spec.s_eval_expr then
      DELETE_RULE Gram str_item: expr END
    else () ;

    (* prune expr *)
    (* FIXME DELETE_RULE Gram expr: "parser"; OPT parser_ipatt; parser_case_list END ; *)
    (* FIXME DELETE_RULE Gram expr: "parser"; OPT parser_ipatt; parser_case_list END ; *)
    if not expr_spec.e_sequence then do {
      DELETE_RULE Gram expr: SELF; ";"; SELF END ;
      DELETE_RULE Gram expr: SELF; ";" END ;
      DELETE_RULE Gram expr: "begin"; sequence; "end" END ;
      DELETE_RULE Gram expr: "("; ")" END;
      DELETE_RULE Gram expr: "begin"; "end" END;
      DELETE_RULE Gram expr: "("; SELF; ";"; sequence; ")" END
    } else () ;
    (* FIXME DELETE_RULE Gram expr: "let"; "module"; a_UIDENT; module_binding0; "in"; expr LEVEL ";" END ; *)
    if not expr_spec.e_if_then then
      DELETE_RULE Gram expr: "if"; SELF; "then"; expr LEVEL "top" END
    else ();
    if not expr_spec.e_if_then_else then
      DELETE_RULE Gram expr: "if"; SELF; "then"; expr LEVEL "top"; "else"; expr LEVEL "top" END
    else ();
    (* FIXME  DELETE_RULE Gram expr: "match"; sequence; "with"; "parser"; OPT parser_ipatt; parser_case_list END ; *)
    (* FIXME  DELETE_RULE Gram expr: "match"; sequence; "with"; "parser"; OPT parser_ipatt; parser_case_list END ; *)
    if Option.is_none expr_spec.e_match then
      DELETE_RULE Gram expr: "match"; sequence; "with"; match_case END
    else ();
    if Option.is_none expr_spec.e_try then
      DELETE_RULE Gram expr: "try"; sequence; "with"; match_case END
    else ();
    if not expr_spec.e_for then
      DELETE_RULE Gram expr: "for"; a_LIDENT; "="; sequence; direction_flag; sequence; "do"; do_sequence END
    else ();
    if not expr_spec.e_while then
      DELETE_RULE Gram expr: "while"; sequence; "do"; do_sequence END
    else ();
    DELETE_RULE Gram expr: "object"; opt_class_self_patt; class_structure; "end" END ;
    if not expr_spec.e_reference_update then
      DELETE_RULE Gram expr: SELF; ":="; expr LEVEL "top" END
    else ();
    if not expr_spec.e_record_field_update then 
      DELETE_RULE Gram expr: SELF; "<-"; expr LEVEL "top" END
    else ();
    if not expr_spec.e_assert then 
      DELETE_RULE Gram expr: "assert"; SELF END
    else () ;
    DELETE_RULE Gram expr: "~"; a_LIDENT END ;
    (*   DELETE_RULE Gram expr: `LABEL _; SELF END ;  *)
    (*   DELETE_RULE Gram expr: `OPTLABEL _; SELF END ;  *)
    DELETE_RULE Gram expr: "?"; a_LIDENT END ;
    if not expr_spec.e_array_update then
      DELETE_RULE Gram expr: SELF; "."; "("; SELF; ")" END
    else ();
    DELETE_RULE Gram expr: SELF; "."; "{"; comma_expr; "}" END ;
    if not expr_spec.e_string_access then
      DELETE_RULE Gram expr: SELF; "."; "["; SELF; "]" END
    else ();
    if not expr_spec.e_record_field_access then
      DELETE_RULE Gram expr: SELF; "."; SELF END
    else ();
    DELETE_RULE Gram expr: SELF; "#"; label END ;
    (* FIXME
    if not gramspec.e_record_construction then 
      DELETE_RULE Gram expr: "{"; OCaml.test_label_eq; label_expr; "}" END ; 
    *)
    if not expr_spec.e_record_functional_update then
      DELETE_RULE Gram expr: "{"; expr LEVEL "."; "with"; label_expr; "}" END
    else ();
    DELETE_RULE Gram expr: "new"; class_longident END ;
    DELETE_RULE Gram expr: "`"; a_ident END ;
    if not expr_spec.e_array then do {
      DELETE_RULE Gram expr: "[|"; "|]" END ;
      DELETE_RULE Gram expr: "[|"; sem_expr; "|]" END
    } else ();
    DELETE_RULE Gram expr: "{<"; ">}" END ;
    DELETE_RULE Gram expr: "{<"; field_expr; ">}" END ;
    DELETE_RULE Gram expr: "("; SELF; ":"; ctyp; ":>"; ctyp; ")" END ;
    DELETE_RULE Gram expr: "("; SELF; ":>"; ctyp; ")" END ;
    (*
    DELETE_RULE Gram expr: stream_begin; stream_end END ;
    DELETE_RULE Gram expr: stream_begin; stream_expr_comp_list; stream_end END ;
    DELETE_RULE Gram expr: stream_begin; stream_end END ;
    DELETE_RULE Gram expr: stream_begin; stream_expr_comp_list; stream_end END ;
    *)

    (* prune types *)
    if match type_spec with [ Some { t_polymorphic = True } -> False | _ -> True ] then do {
      DELETE_RULE Gram type_ident_and_parameters: "("; LIST1 type_parameter SEP ","; ")"; a_LIDENT END ;
      DELETE_RULE Gram type_ident_and_parameters: type_parameter; a_LIDENT END
    } else ();
    (* FIXME (better: fix camlp4) DELETE_RULE Gram type_ident_and_parameters: a_LIDENT; LIST0 type_parameter END ; *)
    DELETE_RULE Gram type_declaration: type_ident_and_parameters; opt_eq_ctyp; LIST0 constrain END ;
    DELETE_RULE Gram opt_eq_ctyp: END ;
    DELETE_RULE Gram type_parameter: "+"; "'"; a_ident END ;
    DELETE_RULE Gram type_parameter: "-"; "'"; a_ident END ;
    (* FIXME DELETE_RULE Gram ctyp: SELF; "as"; "'"; a_ident END ; *)
    (* FIXME DELETE_RULE Gram ctyp: "?"; a_LIDENT; ":"; ctyp LEVEL "star"; "->"; SELF END ; *)
    (* FIXME DELETE_RULE Gram ctyp: lident_colon; ctyp LEVEL "star"; "->"; SELF END ; *)
    (* FIXME DELETE_RULE Gram ctyp: a_OPTLABEL; ctyp LEVEL "star"; "->"; SELF END ; *)
    (* DELETE_RULE Gram ctyp: SELF; "("; SELF; ")" END ; *)
    (* DELETE_RULE Gram ctyp: `ANTIQUOT (("" | "typ" | "anti"), _) END ; *)
    (* DELETE_RULE Gram ctyp: `ANTIQUOT ("tup", _) END ; *)
    (* DELETE_RULE Gram ctyp: `ANTIQUOT ("id", _) END ; *)
    (* DELETE_RULE Gram ctyp: `QUOTATION _ END ; *)
    DELETE_RULE Gram ctyp: "#"; class_longident END ;
    DELETE_RULE Gram ctyp: "<"; opt_meth_list; opt_dot_dot; ">" END ;
    DELETE_RULE Gram ctyp: "["; ">"; "]" END ;
    (* FIXME DELETE_RULE Gram ctyp: "["; ">"; OPT "|"; row_field; "]" END ; *)
    (* FIXME DELETE_RULE Gram ctyp: "["; OPT "|"; row_field; "]" END ; *)
    (* FIXME DELETE_RULE Gram ctyp: "[<"; OPT "|"; row_field; "]" END ; *)
    (* FIXME DELETE_RULE Gram ctyp: "[<"; OPT "|"; row_field; ">"; name_tags; "]" END ; *)
    (* DELETE_RULE Gram ctyp:  END ; *)
    (* FIXME DELETE_RULE Gram type_kind: "private"; SELF END ; *)
    if match type_spec with [ Some { t_record = True } -> False | _ -> True ] then
      DELETE_RULE Gram type_kind: "{"; label_declaration; "}" END
    else ();
    (* FIXME DELETE_RULE Gram type_kind: ctyp; "="; "private"; SELF END ;*)
    (* FIXME DELETE_RULE Gram type_kind: ctyp; "="; "{"; label_declaration; "}" END ; *)
    (* FIXME DELETE_RULE Gram type_kind: ctyp; "="; OPT "|"; constructor_declarations END ; *)
    (* DELETE_RULE Gram ctyp:  END ; *)


    if match (let_spec, letrec_spec) with [ (Some {l_and = True}, Some {lr_and = True}) -> False | _ -> True ] then 
      DELETE_RULE Gram binding: SELF; "and"; SELF END
    else ();
    (* DELETE_RULE Gram binding: `ANTIQUOT (("binding" | "list"), _) END ; *)
    (* DELETE_RULE Gram binding: `ANTIQUOT (("" | "anti"), _); "="; expr END ; *)
    (* DELETE_RULE Gram binding: `ANTIQUOT (("" | "anti"), _) END ; *)
    (* DELETE_RULE Gram binding: let_binding END ; *)
    (* DELETE_RULE Gram fun_binding: ":>"; ctyp; "="; expr END ;  *)

    if match pat_spec with [ Some {p_alias = True} -> False | _ -> True ] then
      DELETE_RULE Gram patt: SELF; "as"; a_LIDENT END
    else ();
    if match pat_spec with [ Some {p_or = True} -> False | _ -> True ] then
      DELETE_RULE Gram patt: SELF; "|"; SELF END
    else ();
    if match pat_spec with [ Some {p_tuple = True} -> False | _ -> True ] then
      DELETE_RULE Gram patt: SELF; ","; LIST1 NEXT SEP "," END
    else ();
    if match pat_spec with [ Some {p_constructor = True} -> False | _ -> True ] then do {
      (* DELETE_RULE Gram patt: patt_constr; SELF END ; *)
      (* DELETE_RULE Gram patt: patt_constr END ; *)
      DELETE_RULE Gram patt: "("; ")" END ;
    } else ();
    if match pat_spec with [ Some {p_list = True} -> False | _ -> True ] then do {
      DELETE_RULE Gram patt: SELF; "::"; SELF END ;
      DELETE_RULE Gram patt: "["; "]" END ;
      DELETE_RULE Gram patt: "["; sem_patt_for_list; "::"; SELF; "]" END ;
      DELETE_RULE Gram patt: "["; sem_patt_for_list; "]" END ;
    } else ();
    if match pat_spec with [ Some {p_array = True} -> False | _ -> True ] then do {
      DELETE_RULE Gram patt: "[|"; "|]" END ;
      DELETE_RULE Gram patt: "[|"; sem_patt; "|]" END ;
    } else ();
    if match pat_spec with [ Some {p_record = True} -> False | _ -> True ] then
      DELETE_RULE Gram patt: "{"; label_patt; "}" END
    else ();
    (*
    DELETE_RULE Gram patt: `ANTIQUOT (("" | "pat" | "anti"), _) END ;
    DELETE_RULE Gram patt: `ANTIQUOT (("" | "pat" | "anti"), _) END ;
    DELETE_RULE Gram patt: `ANTIQUOT ("tup", _) END ;
    DELETE_RULE Gram patt: `ANTIQUOT ("`bool", _) END ;
    DELETE_RULE Gram patt: `QUOTATION _ END ;
    *)
    if match pat_spec with [ Some {p_constant = True} -> False | _ -> True ] then do {
      DELETE_RULE Gram patt: "-"; a_INT END ;
      DELETE_RULE Gram patt: "false" END ;
      DELETE_RULE Gram patt: "true" END ;
      DELETE_RULE Gram patt: a_FLOAT END ;
      DELETE_RULE Gram patt: a_STRING END ;
      DELETE_RULE Gram patt: a_CHAR END ;
    } else ();
    if match pat_spec with [ Some {p_type_annotation = True} -> False | _ -> True ] then
      DELETE_RULE Gram patt: "("; SELF; ":"; ctyp; ")" END
    else ();
    if match pat_spec with [ Some {p_wildcard = True} -> False | _ -> True ] then
      DELETE_RULE Gram patt: "_" END
    else ();
    (*
    DELETE_RULE Gram patt: "`"; a_ident END ;
    DELETE_RULE Gram patt: "#"; type_longident END ;
    *)
    if match pat_spec with [ Some {p_var = True} -> False | _ -> True ] then
      DELETE_RULE Gram patt: ident END
    else ();
    (*
    DELETE_RULE Gram patt: a_INT32 END ;
    DELETE_RULE Gram patt: a_INT64 END ;
    DELETE_RULE Gram patt: a_NATIVEINT END ;
    DELETE_RULE Gram patt: a_CHAR; ".."; a_CHAR END ;
   *)
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
      ErrorHandler.register
        (fun ppf ->
           fun [ Stream.Error code -> failwith "pure Stream.Error"
               | Loc.Exc_located (loc, Stream.Error code) -> failwith "located Stream.Error"
               | Loc.Exc_located (_, Gram.ParseError.E _)
               | Gram.ParseError.E _ -> failwith "UUUUH!"
               | x -> let () = logger#debug "the error handler could not recognize the error" in raise x ]);
  (*     Register.iter_and_take_callbacks (fun (name, f) -> do { logger#debug "register applies %s" name; f () }); *)
      (*
      let module Error : Sig.Error = struct
        type t = string;
        exception E = Stream.Error;
        value print ppf code =
          let err = ParseError.decode code
          print_parse_error ppf loc err;
        value to_string = 
          print_to_to_string print;
      end in
      let module M = ErrorHandler.Register Error in ()
     *)
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
