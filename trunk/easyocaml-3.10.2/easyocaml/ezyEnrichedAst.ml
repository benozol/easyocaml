open EzyAst
open EzyAsttypes
open EzyErrors
open EzyOcamlmodules
open EzyUtils

  (* TODO: text EzyFeatures.t_and for type declarations *)

let logger = new Logger.logger "enr_ast"

type exp_data = 
    {
      ea_ty: EzyTypingCoreTypes.Ty.t ;
      ea_env: EzyEnv.t ;
    }

type id_data = Path.t
    (*{
      ia_path: Path.t ;
    }*)

type pat_data =
    {
      pa_ty: EzyTypingCoreTypes.Ty.t ;
      pa_ident: Ident.t ;
      pa_env: EzyEnv.t ;
    }

type name_data = Ident.t
    (*{
      na_ident: Ident.t ;
    }*)

type generated_expression = (exp_data, id_data, name_data, pat_data) expression

type generated_structure = (exp_data, id_data, name_data, pat_data) structure

type generated_pattern = (exp_data, id_data, name_data, pat_data) pattern


(******************************************************************************)
(*                                  IMPORT                                    *)
(******************************************************************************)

open EzyUtils.Infix

let import_error loc err =
  EzyErrors.raise_fatal ~loc (EzyErrors.Import_error err)

let import_constant loc = function
  | Asttypes.Const_int i ->
      Const_int i
  | Asttypes.Const_char c ->
      Const_char c
  | Asttypes.Const_string s ->
      Const_string s
  | Asttypes.Const_float f ->
      Const_float f
  | _ ->
      raise (import_error loc EzyErrors.Not_supported_constant)

module StringMap = Map.Make(String)
exception TyVarNotFound of string
exception Invalid_type_constructor of (Longident.t * int * int)
exception Unbound_type_constructor of Longident.t

let rec import_core_type creative lookup_ctor tyvarmap ct =
  let module Ty = EzyTypingCoreTypes.Ty in
  let module TyVar = EzyTypingCoreTypes.TyVar in
  let label = ExtLocation.Source ct.Parsetree.ptyp_loc in
  match ct.Parsetree.ptyp_desc with
    | Parsetree.Ptyp_any ->
        tyvarmap, Ty.Var (TyVar.fresh ())
    | Parsetree.Ptyp_var var ->
        begin try
           tyvarmap, Ty.Var (StringMap.find var tyvarmap)
        with Not_found ->
          if creative then
            let tyvar = TyVar.fresh () in
            StringMap.add var tyvar tyvarmap, Ty.Var tyvar
          else 
            raise (TyVarNotFound var)
        end
    | Parsetree.Ptyp_tuple ctys ->
        let tyvarmap, tys' = import_core_types creative lookup_ctor tyvarmap ctys in
        tyvarmap, Ty.Tuple (label, tys')
    | Parsetree.Ptyp_arrow ("", tx, ty) ->
        let tyvarmap, tx' = import_core_type creative lookup_ctor tyvarmap tx in
        let tyvarmap, ty' = import_core_type creative lookup_ctor tyvarmap ty in
        tyvarmap, Ty.Arrow (label, tx', ty')
    | Parsetree.Ptyp_constr (lid, params) ->
        begin try
          let path, tparams = lookup_ctor lid in
          let expected_params, found_params = List.length tparams, List.length params in
          if expected_params = found_params then
            let tyvarmap, params' = import_core_types creative lookup_ctor tyvarmap params in
            tyvarmap, Ty.Constr (label, path, params')
          else
            raise (Invalid_type_constructor (lid, expected_params, found_params))
        with Not_found ->
          raise (Unbound_type_constructor lid)
        end
    | Parsetree.Ptyp_poly ([], ct) ->
        import_core_type creative lookup_ctor tyvarmap ct
    | Parsetree.Ptyp_arrow (str, _, _) ->
        logger#error "EzyAst.import_core_type Ptyp_arrow (str, _, _)" ;
        raise (import_error ct.Parsetree.ptyp_loc EzyErrors.Not_supported_core_type)
    | Parsetree.Ptyp_object _ ->
        logger#error "EzyAst.import_core_type Ptyp_object _" ;
        raise (import_error ct.Parsetree.ptyp_loc EzyErrors.Not_supported_core_type)
    | Parsetree.Ptyp_class _ ->
        logger#error "EzyAst.import_core_type Ptyp_class _" ;
        raise (import_error ct.Parsetree.ptyp_loc EzyErrors.Not_supported_core_type)
    | Parsetree.Ptyp_poly _ ->
        logger#error "EzyAst.import_core_type Ptyp_poly (_::_, _)" ;
        raise (import_error ct.Parsetree.ptyp_loc EzyErrors.Not_supported_core_type)
    | Parsetree.Ptyp_variant _ ->
        logger#error "EzyAst.import_core_type Ptyp_variant _" ;
        raise (import_error ct.Parsetree.ptyp_loc EzyErrors.Not_supported_core_type)
    | Parsetree.Ptyp_alias _ ->
        logger#error "EzyAst.import_core_type Ptyp_alias _" ;
        raise (import_error ct.Parsetree.ptyp_loc EzyErrors.Not_supported_core_type)

and import_core_types creative lookup_ctor tyvarmap ctys =
  List.foldmap (import_core_type creative lookup_ctor) tyvarmap ctys
  
let rec import_pattern pf pat =
  let longident lid = { lid_name = lid; lid_data = () } in
  let pattern desc : imported_pattern = 
    { ppat_desc = desc; ppat_loc = pat.Parsetree.ppat_loc; ppat_data = () } in
  let module F = EzyFeatures in
  let sub_pf =
    if pf.F.p_nested then pf else
      let empty_pf = F.all_pattern_features false in
      { empty_pf with F.p_wildcard = true; p_var = true } in
  match pat.Parsetree.ppat_desc, pf with
    | Parsetree.Ppat_var var, { F.p_var = true } ->
        pattern (Ppat_var { nm_name = var; nm_loc = pat.Parsetree.ppat_loc; nm_data = () })
    | Parsetree.Ppat_any, { F.p_wildcard = true } ->
        pattern Ppat_any
    | Parsetree.Ppat_constant c, { F.p_constant = true } ->
        pattern (Ppat_constant (import_constant pat.Parsetree.ppat_loc c))
    | Parsetree.Ppat_tuple ps, { F.p_tuple = true } ->
        pattern (Ppat_tuple (List.map (import_pattern sub_pf) ps))
    | Parsetree.Ppat_construct (k, opt_pat, explicit_arity), { F.p_constructor = true } ->
        let opt_pat' = match opt_pat with None -> None | Some pat -> Some (import_pattern sub_pf pat) in
        pattern (Ppat_construct (longident k, opt_pat', explicit_arity))
    | Parsetree.Ppat_record fs, { F.p_record = true } ->
        let fs' = List.map (fun (f, p) -> longident f, import_pattern sub_pf p) fs in
        pattern (Ppat_record fs')
    | Parsetree.Ppat_or (p1, p2), { F.p_or = true } ->
        pattern (Ppat_or (import_pattern sub_pf p1, import_pattern sub_pf p2))
    | Parsetree.Ppat_alias (p, nm), { F.p_alias = true } ->
        pattern (Ppat_alias (import_pattern sub_pf p, { nm_name = nm; nm_data = (); nm_loc = Location.none } ))
    | Parsetree.Ppat_constraint (p, ct), { F.p_type_annotation = true } ->
        pattern (Ppat_constraint (import_pattern sub_pf p, ct))
    | desc, _ -> raise (import_error pat.Parsetree.ppat_loc (EzyErrors.Not_supported_pattern desc))

let rec import_var_binding ef loc (pat, expr) =
  match pat.Parsetree.ppat_desc with
    | Parsetree.Ppat_var var ->
        { nm_name = var; nm_loc = loc; nm_data = () }, import_expression ef expr
    | _ -> 
        EzyErrors.raise_fatal ~loc (EzyErrors.Import_error (EzyErrors.Not_supported_pattern pat.Parsetree.ppat_desc))

and import_rule pf ef (pat, exp) = import_pattern pf pat, import_expression ef exp
and import_rules pf ef rules = List.map (import_rule pf ef) rules

and import_expression ef x =
  let loc = x.Parsetree.pexp_loc in
  let name loc nm = { nm_name = nm; nm_loc = loc; nm_data = () } in 
  let longident lid = { lid_name = lid; lid_data = () } in
  let build_expr ?(loc=loc) desc = { pexp_loc = loc; pexp_desc = desc; pexp_data = () } in
  let module F = EzyFeatures in
  match x.Parsetree.pexp_desc, ef with
    | Parsetree.Pexp_ident (Longident.Lident "raise") as raise_desc, { F.e_raise = false } ->
        raise (import_error loc (EzyErrors.Not_supported_expression raise_desc))
    | Parsetree.Pexp_ident (Longident.Lident _ as lident), { F.e_simple_var = true }
    | Parsetree.Pexp_ident (Longident.Ldot _ as lident), { F.e_qualified_var = true } ->
        build_expr (Pexp_ident (longident lident))

    | Parsetree.Pexp_constant c, { F.e_constant = true } ->
        build_expr (Pexp_constant (import_constant loc c))

    | Parsetree.Pexp_let (Nonrecursive, ([_] as bindings), body), { F.e_let_in = Some { F.l_pattern = pf } }
    | Parsetree.Pexp_let (Nonrecursive, bindings, body), { F.e_let_in = Some {F.l_pattern = pf; l_and = true } } ->
        build_expr (Pexp_let (import_rules pf ef bindings, import_expression ef body))

    | Parsetree.Pexp_let (Recursive, ([_] as bindings), body), { F.e_let_rec_in = Some _ }
    | Parsetree.Pexp_let (Recursive, bindings, body), { F.e_let_rec_in = Some { F.lr_and = true } } ->
        build_expr (Pexp_letrec (List.map (import_var_binding ef x.Parsetree.pexp_loc) bindings, import_expression ef body))

    | Parsetree.Pexp_function ("", None, rules), { F.e_function = Some { F.f_pattern = pf } } ->
        build_expr (Pexp_function (import_rules pf ef rules))

    | Parsetree.Pexp_apply (head, args), _ when List.for_all ((=) "" << fst) args ->
        begin match args with
          | [] -> assert false
          | [_, arg] ->
              build_expr (Pexp_apply (import_expression ef head, import_expression ef arg))
          | args ->
              logger#info
                "A different parser than Ocaml's default should be used to avoid inaccuracy for the location of multiple application (%a)."
                Location.print loc ;
              let rec aux sofar = function
                | [] -> sofar
                | (_, arg) :: rem_args ->
                    let loc = Location.span head.Parsetree.pexp_loc arg.Parsetree.pexp_loc in
                    let app = build_expr ~loc (Pexp_apply (sofar, import_expression ef arg)) in
                    aux app rem_args in
              aux (import_expression ef head) args
        end

     | Parsetree.Pexp_match (exp, rules), { F.e_match = Some pf } ->
         build_expr (Pexp_match (import_expression ef exp, import_rules pf ef rules))

     | Parsetree.Pexp_try (exp, rules), { F.e_try = Some pf } ->
         build_expr (Pexp_try (import_expression ef exp, import_rules pf ef rules))

     | Parsetree.Pexp_tuple exps, { F.e_tuple = true } ->
         build_expr (Pexp_tuple (List.map (import_expression ef) exps))

     | Parsetree.Pexp_construct (lid, opt_exp, explicit_arity), { F.e_constructor = true } ->
         build_expr (Pexp_construct (longident lid, Option.map ~f:(import_expression ef) opt_exp, explicit_arity))

     | Parsetree.Pexp_record (fs, (Some _ as opt_exp)), { F.e_record_functional_update = true }
     | Parsetree.Pexp_record (fs, (None as opt_exp)), { F.e_record_construction = true } ->
         let f (f, exp) = longident f, import_expression ef exp in
         build_expr (Pexp_record (List.map f fs, Option.map ~f:(import_expression ef) opt_exp))

     | Parsetree.Pexp_field (exp, lid), { F.e_record_field_access = true } ->
         build_expr (Pexp_field (import_expression ef exp, longident lid))

     | Parsetree.Pexp_setfield (exp1, lid, exp2), { F.e_record_field_update = true } ->
         build_expr (Pexp_setfield (import_expression ef exp1, longident lid, import_expression ef exp2))

     | Parsetree.Pexp_ifthenelse (exp1, exp2, (None as opt_exp3)), { F.e_if_then = true }
     | Parsetree.Pexp_ifthenelse (exp1, exp2, (Some _ as opt_exp3)), { F.e_if_then_else = true } ->
         build_expr (Pexp_ifthenelse (import_expression ef exp1, import_expression ef exp2, Option.map ~f:(import_expression ef) opt_exp3))

     | Parsetree.Pexp_sequence (exp1, exp2), { F.e_sequence = true } ->
         build_expr (Pexp_sequence (import_expression ef exp1, import_expression ef exp2))

     | Parsetree.Pexp_while (exp1, exp2), { F.e_while = true } ->
         build_expr (Pexp_while (import_expression ef exp1, import_expression ef exp2))

     | Parsetree.Pexp_for (str, exp1, exp2, direction_flag, exp3), { F.e_for = true } ->
         build_expr (Pexp_for (name x.Parsetree.pexp_loc str, import_expression ef exp1, import_expression ef exp2, direction_flag, import_expression ef exp3))

     | Parsetree.Pexp_assert exp, { F.e_assert = true } ->
         build_expr (Pexp_assert (import_expression ef exp))

     | Parsetree.Pexp_assertfalse, { F.e_assert = true } ->
         build_expr Pexp_assertfalse

     | Parsetree.Pexp_constraint (exp, Some ty, None), { F.e_type_annotation = true } ->
         build_expr (Pexp_constraint (import_expression ef exp, ty))

     | desc, _ ->
        raise (import_error loc (EzyErrors.Not_supported_expression desc))

let import_strit prf strit =
  let loc = strit.Parsetree.pstr_loc in
  let build_strit desc = {
    pstr_loc = loc ;
    pstr_desc = desc ;
  } in
  let module F = EzyFeatures in
  match strit.Parsetree.pstr_desc, prf.F.pr_struct_features with
    | Parsetree.Pstr_eval e, { F.s_eval_expr = true } ->
        build_strit (Pstr_eval (import_expression prf.F.pr_expr_features e))
    | Parsetree.Pstr_value (Asttypes.Nonrecursive, ([_] as bindings)), { F.s_let = Some { F.l_pattern = pf } }
    | Parsetree.Pstr_value (Asttypes.Nonrecursive, bindings), { F.s_let= Some { F.l_pattern = pf; l_and = true } } ->
        build_strit (Pstr_value (import_rules pf prf.F.pr_expr_features bindings))
    | Parsetree.Pstr_value (Asttypes.Recursive, ([_] as bindings)), { F.s_let_rec = Some _ }
    | Parsetree.Pstr_value (Asttypes.Recursive, bindings), { F.s_let_rec = Some { F.lr_and = true } } ->
        build_strit (Pstr_rec_value (List.map (import_var_binding prf.F.pr_expr_features loc) bindings))
    | Parsetree.Pstr_type tbindings, { F.s_type = Some tf } ->
        let build_tbinding (name, td) =
          begin match td.Parsetree.ptype_params, prf.F.pr_struct_features.F.s_type with
            | _ :: _, Some { F.t_polymorphic = false } ->
              raise (import_error loc (EzyErrors.Not_supported_type_declaration td))
            | _ ->
              let kind =
                match td.Parsetree.ptype_kind, td.Parsetree.ptype_manifest, tf  with
                  | Parsetree.Ptype_abstract, Some td, { F.t_synonym = true } ->
                      Synonym td
                  | Parsetree.Ptype_variant (ctors, Asttypes.Public), None, { F.t_variant = true } ->
                      Variant ctors
                  | Parsetree.Ptype_record (fls, Asttypes.Public), None, { F.t_record = true } ->
                      Record fls
                  | _ ->
                      raise (import_error td.Parsetree.ptype_loc (EzyErrors.Not_supported_type_declaration td)) in
              let name' = { nm_name = name; nm_loc = Location.none; nm_data = ()} in
              let params' = List.map (fun str -> { nm_name = str; nm_loc = Location.none; nm_data = () }) td.Parsetree.ptype_params in
              let td' = { type_params = params'; type_kind = kind } in
              name', td'
          end in
        build_strit (Pstr_type (List.map build_tbinding tbindings))
    | Parsetree.Pstr_exception (name, ct), { F.s_exception = true } ->
        let name' = {
          nm_name = name ;
          nm_loc = Location.none ;
          nm_data = () ;
        } in
        build_strit (Pstr_exception (name', ct))
    | Parsetree.Pstr_open lid, { F.s_open = true } ->
        let lid' = {
          lid_name = lid ;
          lid_data = () ;
        } in
        build_strit (Pstr_open lid')
    | _ as desc, _ -> raise (import_error loc (EzyErrors.Not_supported_structure_item desc))

let import_structure prf str =
  logger#debug "Ocaml ast: %a"
    Printast.implementation str ;
  List.map (import_strit prf) str


let eap ppf ea = EzyTypingCoreTypes.Ty.print ppf ea.ea_ty
let pap ppf pa = EzyTypingCoreTypes.Ty.print ppf pa.pa_ty


(******************************************************************************)
(*                                COMPARISION                                 *)
(******************************************************************************)

module Equality = struct

  open EzyTypingCoreTypes
  open EzyUtils
  open EzyOcamlmodules
  module Tt = Typedtree

  let logger = new Logger.logger "ast_comp"

  module Conf = struct
    type t = {
      identtbl: Ident.t Ident.tbl ;
      tyvarmap: int TyVarMap.t
    }
    let null = { identtbl = Ident.empty ; tyvarmap = TyVarMap.empty }
  end

  module Error = struct
    type t = string
    let set_context context msg =
      context ^ ": " ^ msg
  end

  module M = struct
    include StateErrorMonadBasis (Conf) (Error)
    let failf ?loc fmt =
      let opt_loc_print ppf = function Some loc -> Format.fprintf ppf "%a: " Location.print loc | _ -> () in
      Format.kfprintf (fun _ -> fail (Format.flush_str_formatter ())) Format.str_formatter ("%a" ^^ fmt) opt_loc_print loc
  end
  module StateErrorMonad = Monad.Make (M)
  open StateErrorMonad.Monad_infix

  let (>>) m n = m >>= fun () -> n

  let rec on_list ?loc f xs ys err =
    match xs, ys with
      | [], [] -> M.ok
      | x :: rem, y :: ren ->
          f x y >>
          on_list ?loc f rem ren err
      | _ -> M.failf ?loc err

  let on_opt ?loc f opt_x opt_y err =
    match opt_x, opt_y with
      | Some x, Some y ->
          f x y
      | None, None ->
          M.ok
      | _ ->
          M.failf ?loc err

  let eq_ident ?loc (id1:Ident.t) (id2:Ident.t) =
    M.inspect >>= fun conf ->
    begin try
      if Ident.find_same id1 conf.Conf.identtbl = id2
      then M.ok
      else
        M.failf ?loc "ident clash: %s vs. %s" (Ident.name id1) (Ident.name id2)
    with Not_found ->
      M.inject $ fun conf ->
      { conf with Conf.identtbl = Ident.add id1 id2 conf.Conf.identtbl }
    end

  let rec eq_path ?loc p p' msg = 
    match p, p' with
      | Path.Pident id, Path.Pident id' ->
          eq_ident ?loc id id'
      | Path.Pdot (q, str, _), Path.Pdot (q', str', _) -> 
          if str = str' then
            eq_path ?loc q q' msg
          else
            M.failf ?loc "%s: %s vs. %s" msg (Path.name p) (Path.name p')
      | Path.Papply (q1, q2), Path.Papply (q1', q2') ->
          eq_path ?loc q1 q1' msg >>
          eq_path ?loc q2 q2' msg
      | _ -> M.failf ?loc "%s: path clash (%s vs. %s)" msg (Path.name p) (Path.name p')

  let rec eq_type ?loc env oenv ety ty =
    let ety = EzyEnv.expand_type env ety in
    let ty = Ctype.full_expand oenv ty in (* FIXME expand with Ctype.expand_abbrev *)
    match ety, ty.Types.desc with
      | _, Types.Tlink ty ->
          eq_type ?loc env oenv ety ty
      | Ty.Var tyv, Types.Tvar ->
          M.inspect >>= fun conf ->
          begin try
            if (TyVarMap.find tyv conf.Conf.tyvarmap = ty.Types.id)
            then M.ok
            else M.failf ?loc "variable mismatch: %a" TyVar.print tyv
          with Not_found ->
            let f conf = { conf with Conf.tyvarmap = TyVarMap.add tyv ty.Types.id conf.Conf.tyvarmap } in
            M.inject f
          end
      | Ty.Constr (label, k, tys), Types.Tconstr (k', tys', _ ) ->
(*           let loc = match label with ExtLocation.Source loc -> Some loc | _ -> None in *)
          let rec for_all = function
            | [], [] ->
                M.ok
            | h1 :: t1, h2 :: t2 ->
                eq_type ?loc env oenv h1 h2 >>
                for_all (t1, t2)
            | l1, l2 ->
                M.failf ?loc "type %a vs %a: constructor parameter count: %d vs %d" Path.print k Path.print k' (List.length l1) (List.length l2) in
          let msg = format_str "Constructor (%a) %s vs (%a) %s"
                      (format_list Ty.print ", ") tys (Path.name k)
                      (format_list Types.print ", ") tys' (Path.name k') in
          eq_path ?loc k k' msg >>
          for_all (tys, tys')
      | Ty.Arrow (label, ety1, ety2), Types.Tarrow ("", ty1, ty2, _) -> 
          let loc = match label with ExtLocation.Source loc -> Some loc | _ -> None in 
          eq_type ?loc env oenv ety1 ty1 >>
          eq_type ?loc env oenv ety2 ty2 
      | Ty.Tuple (_, tys), Types.Ttuple tys' -> 
          on_list ?loc (eq_type ?loc env oenv) tys tys' "tuple"
      | _ -> M.failf ?loc "type: unknown %a vs. %a" Ty.print ety Types.print ty

  (* let eq_type eenv oenv ?(context="") tx ty =
    let tx = EzyEnv.full_expand_type eenv tx in
    M.between (eq_type ~context oenv tx ty)
      (fun _ _ -> logger#debug "Type equal %a vs. %a" Ty.print tx Types.print ty)
      (fun _ _ -> logger#debug "Types not equal %a vs. %a" Ty.print tx Types.print ty) *)

  let eq_constant c c' = M.ok
    (* let equality x x' msg =
      if x = x'
      then M.ok
      else M.fail (format_str msg x x') in
    match c, c' with
      | Const_int n, Asttypes.Const_int n' ->
          equality n n' "wrong number (%d vs %d)"
      | Const_char c, Asttypes.Const_char c' ->
          equality c c' "wrong characters (%c vs %c)"
      | Const_string s, Asttypes.Const_string s' ->
          equality s s' "wrong strings (%s vs %s)"
      | Const_float f, Asttypes.Const_float f' ->
          equality f f' "wrong floats (%s vs %s)"
      | _, _  -> M.fail "constant" *)

  let rec eq_pattern s epat opat =
    let loc = epat.ppat_loc in
    eq_type ~loc epat.ppat_data.pa_env opat.Tt.pat_env (TyVarSubst.apply_to_ty s epat.ppat_data.pa_ty) opat.Tt.pat_type >>
    match epat.ppat_desc, opat.Tt.pat_desc with
      | Ppat_any, Tt.Tpat_any ->
          M.ok
      | Ppat_var var, Tt.Tpat_var ident ->
          eq_ident ~loc var.nm_data ident
      | Ppat_constant c, Tt.Tpat_constant c' ->
          eq_constant c c' 
      | Ppat_tuple pats, Tt.Tpat_tuple pats' ->
          on_list ~loc (eq_pattern s) pats pats' "pattern tuple"
      | Ppat_construct (lid, opt_pat, flag), Tt.Tpat_construct (lid', pats') ->
          begin match opt_pat, flag, pats' with
            | Some {ppat_desc = Ppat_tuple pats}, true, _ ->
                on_list ~loc (eq_pattern s) pats pats' "constructor arguments"
            | Some pat, _, [pat'] ->
                eq_pattern s pat pat'
            | None, _, [] -> M.ok
            | _ -> M.failf ~loc:epat.ppat_loc "constructor arguments mismatch"
          end
      | Ppat_record fs, Tt.Tpat_record fs' ->
          on_list ~loc (eq_pattern s) (List.map snd fs) (List.map snd fs') "pattern record"
      | Ppat_or (pat1, pat2), Tt.Tpat_or (pat1', pat2', _) ->
          eq_pattern s pat1 pat1' >>
          eq_pattern s pat2 pat2'
      | Ppat_constraint (epat, _), _ ->
          eq_pattern s epat opat
      | Ppat_alias (pat, name), Tt.Tpat_alias (pat', id) ->
          eq_ident ~loc:name.nm_loc name.nm_data id >>
          eq_pattern s pat pat'
      | _ -> M.failf ~loc "pattern: %a vs %a" (print_pat ()) epat Tt.print_pat opat

  let eq_pattern s epat opat =
    M.between (eq_pattern s epat opat)
      (fun st ->
         logger#trace "Comparing pattern %a: %s"
           (print_pat ()) epat
           (match st with Result.Ok _ -> "ok" | Result.Error _ -> "failed"))

  let rec eq_binding s (var, expr) = function
    | ({Tt.pat_desc=Tt.Tpat_var var'}, expr') ->
        eq_ident ~loc:var.nm_loc var.nm_data var' >>
        m_eq_expression s expr expr'
    | _ -> M.failf ~loc:(Location.span var.nm_loc expr.pexp_loc) "eq_binding"

  and m_eq_expression s eexpr oexpr =
    logger#trace "m_eq_expression %a" (print_expr ()) eexpr ;
    let loc = eexpr.pexp_loc in
    eq_type ~loc eexpr.pexp_data.ea_env oexpr.Tt.exp_env (TyVarSubst.apply_to_ty s eexpr.pexp_data.ea_ty) oexpr.Tt.exp_type >>
    match eexpr.pexp_desc, oexpr.Tt.exp_desc with
      | Pexp_ident { lid_data = path1 }, Tt.Texp_ident (path2, _) ->
          eq_path ~loc path1 path2 (format_str "Identifier %s" (Path.name path1))
      | Pexp_ident _, _ ->
          M.failf ~loc "ident otre"
      | Pexp_constant c, Tt.Texp_constant c' ->
          eq_constant c c'
      | Pexp_constant _, _ ->
          M.failf ~loc "constant otre"
      | Pexp_let (rules, body), Tt.Texp_let (Nonrecursive, rules', body') ->
          let aux (pat, expr) (pat', expr') =
            eq_pattern s pat pat' >>
            m_eq_expression s expr expr' in
          on_list ~loc aux rules rules' "m_eq_expression let rules" >>
          m_eq_expression s body body'
      | Pexp_letrec (bindings, body), Tt.Texp_let (Recursive, bindings', body') ->
          on_list ~loc (eq_binding s) bindings bindings' "m_eq_expression letrec bindings" >>
          m_eq_expression s body body'
      | Pexp_function rules, Tt.Texp_function (rules', _) ->
          eq_rules s rules rules'
      | Pexp_apply _ , Tt.Texp_apply (head, args) ->
          let rec aux exp rev_args =
            match exp, rev_args with
              | { pexp_desc = Pexp_apply (exp1, exp2) }, (Some arg, Tt.Required) :: rem_args ->
                  m_eq_expression s exp2 arg >>
                  aux exp1 rem_args
              | _, [] ->
                  m_eq_expression s exp head
              | _ -> M.failf ~loc "m_eq_expression Pexp_apply (_, args)" in
          aux eexpr (List.rev args)
      | Pexp_match (exp, rules), Tt.Texp_match (exp', rules', _)
      | Pexp_try (exp, rules), Tt.Texp_try (exp', rules') ->
          m_eq_expression s exp exp' >>
          eq_rules s rules rules'
      | Pexp_tuple exps, Tt.Texp_tuple exps' ->
          on_list ~loc (m_eq_expression s) exps exps' "m_eq_expression Tuple _"
      | Pexp_construct ({ lid_data = path }, opt_exp, flag), Tt.Texp_construct (_ , opt_exp') ->
          begin match opt_exp, flag, opt_exp' with
            | None, _, [] -> M.ok
            | None, _, _ -> M.failf ~loc "construct %s, None vs %d" (Path.name path) (List.length opt_exp')
            | Some { pexp_desc = Pexp_tuple exps }, true, exps' ->
                on_list ~loc (m_eq_expression s) exps exps' "construct args"
            | Some exp, false, [exp'] ->
                m_eq_expression s exp exp'
            | _ -> M.failf ~loc ("construct")
          end
      | Pexp_record (fs, opt_exp), Tt.Texp_record (fs', opt_exp') ->
          let _, exps = List.split fs in
          let _, exps' = List.split fs' in
          on_list ~loc (m_eq_expression s) exps exps' "Pexp_record field exps" >>
          on_opt ~loc (m_eq_expression s) opt_exp opt_exp' "Pexp_record optional expression"
      | Pexp_field (exp, f), Tt.Texp_field (exp', f') ->
          m_eq_expression s exp exp'
      | Pexp_setfield (exp0, f, exp1), Tt.Texp_setfield (exp0', f', exp1') ->
          m_eq_expression s exp0 exp0' >>
          m_eq_expression s exp1 exp1'
      | Pexp_ifthenelse (exp0, exp1, exp2), Tt.Texp_ifthenelse (exp0', exp1', exp2') ->
          m_eq_expression s exp0 exp0' >>
          m_eq_expression s exp1 exp1' >>
          on_opt ~loc (m_eq_expression s) exp2 exp2' "Pexp_ifthenelse (_, _, option)"
      | Pexp_ifthenelse _, _ ->
          M.failf ~loc "ifthenelse otre"
      | Pexp_sequence (exp0, exp1), Tt.Texp_sequence (exp0', exp1')
      | Pexp_while (exp0, exp1), Tt.Texp_while (exp0', exp1') ->
          m_eq_expression s exp0 exp0' >>
          m_eq_expression s exp1 exp1'
      | Pexp_for (var, exp0, exp1, _, exp2), Tt.Texp_for (id, exp0', exp1', _, exp2') ->
          eq_ident ~loc:var.nm_loc var.nm_data id >>
          m_eq_expression s exp0 exp0' >>
          m_eq_expression s exp1 exp1' >>
          m_eq_expression s exp2 exp2'
      | Pexp_assert exp, Tt.Texp_assert exp' ->
          m_eq_expression s exp exp'
      | Pexp_assertfalse, Tt.Texp_assertfalse ->
          M.ok
      | Pexp_constraint (exp, _), _ ->
          m_eq_expression s exp oexpr
      | _ -> M.failf ~loc "no match %a vs %a" (print_expr ()) eexpr Tt.print_expr oexpr

  and eq_rule s (pat, exp) (pat', exp') =
    eq_pattern s pat pat' >>
    m_eq_expression s exp exp'
  and eq_rules s rs rs' =
    on_list (eq_rule s) rs rs' "rules"

  (* let eq_rule s ((pat, expr) as erule) orule =
    M.between (eq_rule s erule orule)
      (fun _ _ -> logger#debug "Comparing rule %a -> %a: ok" (print_pat ()) pat (print_expr ()) expr)
      (fun _ _ -> logger#debug "Comparing rule %a -> %a: failed" (print_pat ()) pat (print_expr ()) expr) *)

  (* let m_eq_expression s eexpr oexpr =
    M.between (m_eq_expression s eexpr oexpr)
      (fun _ _ -> logger#debug "Comparing expression %a: ok" (print_expr ()) eexpr)
      (fun _ _ -> logger#debug "Comparing expression %a: failed" (print_expr ()) eexpr) *)

  let rec m_eq_structure_item s estrit strit =
    let loc = estrit.pstr_loc in
    match estrit, strit with
      | { pstr_desc = Pstr_eval exp }, Tt.Tstr_eval exp' ->
          m_eq_expression s exp exp' >>
          M.return true
      | { pstr_desc = Pstr_value bindings }, Tt.Tstr_value (_, bindings') ->
          on_list ~loc (eq_rule s) bindings bindings' "m_eq_structure Pstr_value _" >>
          M.return true
      | { pstr_desc = Pstr_rec_value bindings }, Tt.Tstr_value (_, bindings') ->
          on_list ~loc (eq_binding s) bindings bindings' "m_eq_structure Pstr_rec_value _" >>
          M.return true
      | { pstr_desc = Pstr_open _ }, _ ->
          M.return false
      | { pstr_desc = Pstr_type _ }, Tt.Tstr_type _
      | { pstr_desc = Pstr_exception _ }, Tt.Tstr_exception _  ->
          M.return true
      | _ -> M.failf ~loc:estrit.pstr_loc "eq_structure"

  let m_eq_structure_item s estrit strit =
    let short ppf strit =
      match strit.pstr_desc with
        | Pstr_eval expr -> Format.fprintf ppf "@[Pstr_eval (%a):@ %a@]" Location.print strit.pstr_loc (EzyAst.print_expr ()) expr
        | Pstr_value bs ->
            let aux ppf (p, _) = EzyAst.print_pat () ppf p in
            Format.fprintf ppf "@[Pstr_value (%a):@ @[%a@]@]"
              Location.print strit.pstr_loc
              (format_list aux ",@ ") bs
        | Pstr_rec_value bs ->
            let aux ppf (n, _) = Format.pp_print_string ppf n.nm_name in
            Format.fprintf ppf "@[Pstr_rec_value (%a):@ @[%a@]@]"
              Location.print strit.pstr_loc
              (format_list aux ",@ ") bs
        | _ -> Format.pp_print_string ppf "<not interesting>" in
    M.between (m_eq_structure_item s estrit strit)
      (fun st ->
         logger#debug "@[%s for: @ %a@]"
           (match st with Result.Ok _ -> "Succeeded" | Result.Error _ -> "Failed")
           short estrit)

  let m_eq_structure (s : TyVarSubst.t) estr ostr =
    let rec aux = function
      | [], [] -> M.ok
      | estrit :: erem, ostrit :: orem ->
          m_eq_structure_item s estrit ostrit >>= fun b ->
          if b then aux (erem, orem) else aux (erem, ostrit :: orem)
      | _ -> M.fail "eq_structure" in
    M.between
      (aux (estr, ostr))
      (fun st ->
         logger#info "@[%s comparing ecaml/ocaml typing"
           (match st with Result.Ok _ -> "Success" | Result.Error _ -> "Failed"))

  let eq_expression s eexpr expr =
    M.perform Conf.null (m_eq_expression eexpr expr s)
      (fun () st -> None)
      (fun msg st -> Some msg)

  let eq_structure (s: TyVarSubst.t) estr str = 
    M.perform Conf.null (m_eq_structure s estr str)
      (fun () st -> None)
      (fun msg st -> Some msg)
end
