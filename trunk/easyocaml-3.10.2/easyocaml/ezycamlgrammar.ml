open EzyUtils
(*pp camlp4oof *)


let logger = new EzyUtils.Logger.logger "ezy_gram"

module Id = struct
  let name = "EasyCamlGrammar"
  let version = "0.1"
end

open Camlp4 
open PreCast

module OCamlRevised = Camlp4OCamlRevisedParser.Make
                 (Camlp4.OCamlInitSyntax.Make
                    (Ast)(Gram)(Quotation))
module OCaml = Camlp4OCamlParser.Make (OCamlRevised)

module Make (Spec : sig val spec: EzyFeatures.program_feats end) (Syntax : Camlp4.Sig.Camlp4Syntax) = struct
  open Syntax

  let _ = logger#info "This is the Ezycamlsyntax" ;;
  let pat_spec, fun_spec, let_spec, letrec_spec, expr_spec, type_spec, str_spec, spec = EzyFeatures.maximum Spec.spec ;;

  open EzyFeatures

  (* TODO [Gram.Entry.clear] all unnecessary entries from Camlp4/Sig.ml:1198 *)

  (* prune top_phrase *)

  (* DELETE_RULE Gram top_phrase: "#"; a_LIDENT; opt_expr; ";;" END ; *)

  (* prune implem *)

  DELETE_RULE Gram implem: "#"; a_LIDENT; opt_expr; semi END ; 

  if not str_spec.s_semisemi_optional then
    DELETE_RULE Gram semi: END ; 

  (* FIXME DELETE_RULE Gram fun_def_cont: "when"; expr; "->"; expr END ; *)
  DELETE_RULE Gram opt_when_expr: "when"; expr END ;

  (* prune str_item *)

  DELETE_RULE Gram str_item: "let"; "module"; a_UIDENT; module_binding0; "in"; expr END ;
  if Option.is_none expr_spec.e_let_in && Option.is_none expr_spec.e_let_rec_in then
    DELETE_RULE Gram str_item: "let"; opt_rec; binding; "in"; expr END ;
  if Option.is_none str_spec.s_let && Option.is_none str_spec.s_let_rec then
    DELETE_RULE Gram str_item: "let"; opt_rec; binding END ;
  DELETE_RULE Gram str_item: "exception"; constructor_declaration; "="; type_longident END ; 
  (* FIXME DELETE_RULE Gram str_item: "external"; a_LIDENT; ":"; ctyp; "="; OCamlRevised.string_list END ; *)
  DELETE_RULE Gram str_item: "include"; module_expr END ;
  DELETE_RULE Gram str_item: "module"; "rec"; module_binding END ;
  DELETE_RULE Gram str_item: "module"; "type"; a_UIDENT; "="; module_type END ;
  DELETE_RULE Gram str_item: "module"; a_UIDENT; module_binding0 END ;
  if Option.is_none type_spec then
    DELETE_RULE Gram str_item: "type"; type_declaration END ;
  DELETE_RULE Gram str_item: "class"; "type"; class_type_declaration END ;
  DELETE_RULE Gram str_item: "class"; class_declaration END ;
  DELETE_RULE Gram str_item: `ANTIQUOT (("" | "stri" | "anti" | "list"), _) END ; 
  DELETE_RULE Gram str_item: `QUOTATION _ END ; 
  if not str_spec.s_eval_expr then
    DELETE_RULE Gram str_item: expr END ;

  (* prune expr *)

  (* FIXME DELETE_RULE Gram expr: "parser"; OPT parser_ipatt; parser_case_list END ; *)
  (* FIXME DELETE_RULE Gram expr: "parser"; OPT parser_ipatt; parser_case_list END ; *)
  if not expr_spec.e_sequence then (
    DELETE_RULE Gram expr: SELF; ";"; SELF END ;
    DELETE_RULE Gram expr: SELF; ";" END ;
    DELETE_RULE Gram expr: "begin"; sequence; "end" END ;
    DELETE_RULE Gram expr: "("; ")" END;
    DELETE_RULE Gram expr: "begin"; "end" END;
    DELETE_RULE Gram expr: "("; SELF; ";"; sequence; ")" END
  ) ;
  DELETE_RULE Gram expr: "let"; "module"; a_UIDENT; module_binding0; "in"; expr LEVEL ";" END ;
  if not expr_spec.e_if_then then
    DELETE_RULE Gram expr: "if"; SELF; "then"; expr LEVEL "top" END ;
  if not expr_spec.e_if_then_else then
    DELETE_RULE Gram expr: "if"; SELF; "then"; expr LEVEL "top"; "else"; expr LEVEL "top" END ;
  (* FIXME  DELETE_RULE Gram expr: "match"; sequence; "with"; "parser"; OPT parser_ipatt; parser_case_list END ; *)
  (* FIXME  DELETE_RULE Gram expr: "match"; sequence; "with"; "parser"; OPT parser_ipatt; parser_case_list END ; *)
  if Option.is_none expr_spec.e_match then
    DELETE_RULE Gram expr: "match"; sequence; "with"; match_case END ;
  if Option.is_none expr_spec.e_try then
    DELETE_RULE Gram expr: "try"; sequence; "with"; match_case END ;
  if not expr_spec.e_for then
    DELETE_RULE Gram expr: "for"; a_LIDENT; "="; sequence; direction_flag; sequence; "do"; do_sequence END ;
  if not expr_spec.e_while then
    DELETE_RULE Gram expr: "while"; sequence; "do"; do_sequence END ;
  DELETE_RULE Gram expr: "object"; opt_class_self_patt; class_structure; "end" END ;
  if not expr_spec.e_reference_update then
    DELETE_RULE Gram expr: SELF; ":="; expr LEVEL "top" END ;
  if not expr_spec.e_record_field_update then 
    DELETE_RULE Gram expr: SELF; "<-"; expr LEVEL "top" END ;
  if not expr_spec.e_assert then 
    DELETE_RULE Gram expr: "assert"; SELF END ;
  DELETE_RULE Gram expr: "~"; a_LIDENT END ;
  DELETE_RULE Gram expr: `LABEL _; SELF END ; 
  DELETE_RULE Gram expr: `OPTLABEL _; SELF END ; 
  DELETE_RULE Gram expr: "?"; a_LIDENT END ;
  if not expr_spec.e_array_update then
    DELETE_RULE Gram expr: SELF; "."; "("; SELF; ")" END ;
  DELETE_RULE Gram expr: SELF; "."; "{"; comma_expr; "}" END ;
  if not expr_spec.e_string_access then
    DELETE_RULE Gram expr: SELF; "."; "["; SELF; "]" END ;
  if not expr_spec.e_record_field_access then
    DELETE_RULE Gram expr: SELF; "."; SELF END ;
  DELETE_RULE Gram expr: SELF; "#"; label END ;
  (* FIXME
  if not gramspec.e_record_construction then 
    DELETE_RULE Gram expr: "{"; OCaml.test_label_eq; label_expr; "}" END ; 
   *)
  if not expr_spec.e_record_functional_update then
    DELETE_RULE Gram expr: "{"; expr LEVEL "."; "with"; label_expr; "}" END ;
  DELETE_RULE Gram expr: "new"; class_longident END ;
  DELETE_RULE Gram expr: "`"; a_ident END ;
  if not expr_spec.e_array then (
    DELETE_RULE Gram expr: "[|"; "|]" END ;
    DELETE_RULE Gram expr: "[|"; sem_expr; "|]" END
  ) ;
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

  if match type_spec with Some { t_polymorphic = true } -> false | _ -> true then (
    DELETE_RULE Gram type_ident_and_parameters: "("; LIST1 type_parameter SEP ","; ")"; a_LIDENT END ;
    DELETE_RULE Gram type_ident_and_parameters: type_parameter; a_LIDENT END
  ) ;
  (* FIXME (better: fix camlp4) DELETE_RULE Gram type_ident_and_parameters: a_LIDENT; LIST0 type_parameter END ; *)
  DELETE_RULE Gram type_declaration: type_ident_and_parameters; opt_eq_ctyp; LIST0 constrain END ;
  DELETE_RULE Gram opt_eq_ctyp: END ;
  DELETE_RULE Gram type_parameter: "+"; "'"; a_ident END ;
  DELETE_RULE Gram type_parameter: "-"; "'"; a_ident END ;

  DELETE_RULE Gram ctyp: SELF; "as"; "'"; a_ident END ;
  DELETE_RULE Gram ctyp: "?"; a_LIDENT; ":"; ctyp LEVEL "star"; "->"; SELF END ;
  (* FIXME DELETE_RULE Gram ctyp: lident_colon; ctyp LEVEL "star"; "->"; SELF END ; *)
  DELETE_RULE Gram ctyp: a_OPTLABEL; ctyp LEVEL "star"; "->"; SELF END ;
  DELETE_RULE Gram ctyp: SELF; "("; SELF; ")" END ;
  DELETE_RULE Gram ctyp: `ANTIQUOT (("" | "typ" | "anti"), _) END ;
  DELETE_RULE Gram ctyp: `ANTIQUOT ("tup", _) END ;
  DELETE_RULE Gram ctyp: `ANTIQUOT ("id", _) END ;
  DELETE_RULE Gram ctyp: `QUOTATION _ END ;
  DELETE_RULE Gram ctyp: "#"; class_longident END ;
  DELETE_RULE Gram ctyp: "<"; opt_meth_list; opt_dot_dot; ">" END ;
  DELETE_RULE Gram ctyp: "["; ">"; "]" END ;
  DELETE_RULE Gram ctyp: "["; ">"; OPT "|"; row_field; "]" END ;
  DELETE_RULE Gram ctyp: "["; OPT "|"; row_field; "]" END ;
  DELETE_RULE Gram ctyp: "[<"; OPT "|"; row_field; "]" END ;
  DELETE_RULE Gram ctyp: "[<"; OPT "|"; row_field; ">"; name_tags; "]" END ;
(*   DELETE_RULE Gram ctyp:  END ; *)

  DELETE_RULE Gram type_kind: "private"; SELF END ;
  if match type_spec with Some { t_record = true } -> false | _ -> true then
    DELETE_RULE Gram type_kind: "{"; label_declaration; "}" END ;
  DELETE_RULE Gram type_kind: ctyp; "="; "private"; SELF END ;
  DELETE_RULE Gram type_kind: ctyp; "="; "{"; label_declaration; "}" END ;
  DELETE_RULE Gram type_kind: ctyp; "="; OPT "|"; constructor_declarations END ;
  (* DELETE_RULE Gram ctyp:  END ; *)


  if match let_spec, letrec_spec with Some {l_and = true}, Some {lr_and = true} -> false | _ -> true then 
    DELETE_RULE Gram binding: SELF; "and"; SELF END ;
  DELETE_RULE Gram binding: `ANTIQUOT (("binding" | "list"), _) END ;
  DELETE_RULE Gram binding: `ANTIQUOT (("" | "anti"), _); "="; expr END ;
  DELETE_RULE Gram binding: `ANTIQUOT (("" | "anti"), _) END ;
(*     DELETE_RULE Gram binding: let_binding END ; *)
(*     DELETE_RULE Gram fun_binding: ":>"; ctyp; "="; expr END ;  *)

  if match pat_spec with Some {p_alias = true} -> false | _ -> true then
    DELETE_RULE Gram patt: SELF; "as"; a_LIDENT END ;
  if match pat_spec with Some {p_or = true} -> false | _ -> true then
    DELETE_RULE Gram patt: SELF; "|"; SELF END ;
  if match pat_spec with Some {p_tuple = true} -> false | _ -> true then
    DELETE_RULE Gram patt: SELF; ","; LIST1 NEXT SEP "," END ;
  if match pat_spec with Some {p_constructor = true} -> false | _ -> true then (
(*       DELETE_RULE Gram patt: patt_constr; SELF END ; *)
(*       DELETE_RULE Gram patt: patt_constr END ; *)
    DELETE_RULE Gram patt: "("; ")" END ;
  ) ;
  if match pat_spec with Some {p_list = true} -> false | _ -> true then (
    DELETE_RULE Gram patt: SELF; "::"; SELF END ;
    DELETE_RULE Gram patt: "["; "]" END ;
    DELETE_RULE Gram patt: "["; sem_patt_for_list; "::"; SELF; "]" END ;
    DELETE_RULE Gram patt: "["; sem_patt_for_list; "]" END ;
  ) ;
  if match pat_spec with Some {p_array = true} -> false | _ -> true then (
    DELETE_RULE Gram patt: "[|"; "|]" END ;
    DELETE_RULE Gram patt: "[|"; sem_patt; "|]" END ;
  ) ;
  if match pat_spec with Some {p_record = true} -> false | _ -> true then
    DELETE_RULE Gram patt: "{"; label_patt; "}" END ;
  DELETE_RULE Gram patt: `ANTIQUOT (("" | "pat" | "anti"), _) END ;
  DELETE_RULE Gram patt: `ANTIQUOT (("" | "pat" | "anti"), _) END ;
  DELETE_RULE Gram patt: `ANTIQUOT ("tup", _) END ;
  DELETE_RULE Gram patt: `ANTIQUOT ("`bool", _) END ;
  DELETE_RULE Gram patt: `QUOTATION _ END ;
  if match pat_spec with Some {p_constant = true} -> false | _ -> true then (
    DELETE_RULE Gram patt: "-"; a_INT END ;
    DELETE_RULE Gram patt: "false" END ;
    DELETE_RULE Gram patt: "true" END ;
    DELETE_RULE Gram patt: a_FLOAT END ;
    DELETE_RULE Gram patt: a_STRING END ;
    DELETE_RULE Gram patt: a_CHAR END ;
  ) ;
    
  if match pat_spec with Some {p_type_annotation = true} -> false | _ -> true then
    DELETE_RULE Gram patt: "("; SELF; ":"; ctyp; ")" END ;

  if match pat_spec with Some {p_wildcard = true} -> false | _ -> true then
    DELETE_RULE Gram patt: "_" END ;
  DELETE_RULE Gram patt: "`"; a_ident END ;
  DELETE_RULE Gram patt: "#"; type_longident END ;
  if match pat_spec with Some {p_var = true} -> false | _ -> true then
    DELETE_RULE Gram patt: ident END ;
  DELETE_RULE Gram patt: a_INT32 END ;
  DELETE_RULE Gram patt: a_INT64 END ;
  DELETE_RULE Gram patt: a_NATIVEINT END ;
  DELETE_RULE Gram patt: a_CHAR; ".."; a_CHAR END ;


(*
  let infixop5 = Gram.Entry.mk "infixop5" in
  let infixop6 = Gram.Entry.mk "infixop6" in
 *)
  EXTEND Gram
(* expr: LEVEL "simple" [ (* LEFTA *)
      [ "..." -> let s = Loc.to_string _loc in <:expr< failwith ("Not yet implemented: " ^ $str:s$) >> ]
    ] ; *)
    type_declaration: [ 
      [ (n, tpl) = type_ident_and_parameters; tk = opt_eq_ctyp -> Ast.TyDcl (_loc, n, tpl, tk, []) ]
    ] ;
  (*
    infixop5:
      [ [ x = [ "&" | "&&" ] -> <:expr< $lid:x$ >> ] ]
    ;
    infixop6:
      [ [ x = [ "or" | "||" ] -> <:expr< $lid:x$ >> ] ]
    ;
    expr:
      [ "||" RIGHTA
        [ e1 = SELF; op = infixop6; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | "&&" RIGHTA
        [ e1 = SELF; op = infixop5; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | "<" LEFTA
        [ e1 = SELF; op = infixop0; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | "^" RIGHTA
        [ e1 = SELF; op = infixop1; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | "+" LEFTA
        [ e1 = SELF; op = infixop2; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | "*" LEFTA
        [ e1 = SELF; "land"; e2 = SELF -> <:expr< $e1$ land $e2$ >>
        | e1 = SELF; "lor"; e2 = SELF -> <:expr< $e1$ lor $e2$ >>
        | e1 = SELF; "lxor"; e2 = SELF -> <:expr< $e1$ lxor $e2$ >>
        | e1 = SELF; "mod"; e2 = SELF -> <:expr< $e1$ mod $e2$ >>
        | e1 = SELF; op = infixop3; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | "**" RIGHTA
        [ e1 = SELF; "asr"; e2 = SELF -> <:expr< $e1$ asr $e2$ >>
        | e1 = SELF; "lsl"; e2 = SELF -> <:expr< $e1$ lsl $e2$ >>
        | e1 = SELF; "lsr"; e2 = SELF -> <:expr< $e1$ lsr $e2$ >>
        | e1 = SELF; op = infixop4; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      ] ;
   *)
  END ;;
  include Syntax
end


module MakeFromSpec (Spec : sig val spec : EzyFeatures.program_feats end) = struct
  include Make (Spec)
    (Camlp4OCamlParser.Make
       (Camlp4OCamlRevisedParser.Make
          (Camlp4.OCamlInitSyntax.Make
             (Ast)(Gram)(Quotation))))
end

let import_convert_str : Camlp4_import.Parsetree.structure -> Parsetree.structure =
  fun ast -> Obj.magic ast

module P = struct
  let file fs =
    (* let module Syntax = Camlp4.PreCast.Syntax in *)
    let module Syntax = MakeFromSpec (struct let spec = fs end) in
    (* let module AstConversion = Camlp4.Struct.Camlp4Ast2OCamlAst.Make (Syntax.Ast) in *)
    let module AstConversion = MyCamlp4Ast2OCamlAst.Make (Syntax.Ast) in 
    let module Printer = Camlp4.Printers.OCamlr.Make(Syntax) in
    let import_loc : Syntax.Loc.t -> Location.t =
      fun loc -> Obj.magic (Syntax.Loc.to_ocaml_location loc) in
    fun inputfile ast_impl_magic_number ->
      let loc = Syntax.Loc.mk inputfile in
      let ic = open_in inputfile in
      try Misc.try_finally
        (fun () ->
           let ast, rest = Syntax.Gram.parse Syntax.implem loc (Stream.of_channel ic) in
           logger#info "Parsed tree: %a" (fun ppf -> List.iter (Printer.print_implem ~input_file:inputfile)) ast ;
           match rest with
             | Some loc ->
                 EzyErrors.raise_fatal ~loc:(import_loc loc) (EzyErrors.Syntax_error None)
             | None ->
                 (*import_convert_str*) (List.flatten (List.map AstConversion.str_item ast)))
        (fun () -> close_in ic)
      with Syntax.Loc.Exc_located (loc, exn) ->
        EzyErrors.raise_fatal ~loc:(import_loc loc) (EzyErrors.Syntax_error (Some exn))
  let phrase fs sb = assert false
end
let () = let module M = EzyParser.Register (P) in ()

