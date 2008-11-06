open EzyUtils

(** {3 Data types describing the syntactic features} *)

type pattern_feats = {
  p_wildcard : bool;
  p_var : bool;
  p_constant : bool;
  p_constructor : bool;
  p_list: bool ;
  p_array: bool ;
  p_tuple : bool;
  p_type_annotation : bool;
  p_or : bool;
  p_record : bool;
  p_alias : bool;
  p_nested : bool
}

type fun_feats = {
  f_pattern : pattern_feats ;
  f_fun : bool ;
}

type let_feats = {
  l_pattern : pattern_feats ;
  l_and : bool ; (* multiple bindings [let ... and ... ] *)
  l_args : bool ; (* functions by [let f x y .. in ... ] *)
}

type letrec_feats = {
  lr_and : bool ;
  lr_args : bool ;
}

type expr_feats = {
  e_simple_var : bool;
  e_qualified_var : bool;
  e_constant : bool;
  e_let_in : let_feats option; 
  e_let_rec_in : letrec_feats option; 
  e_function : fun_feats option;
  e_match : pattern_feats option;
  e_try : pattern_feats option;
  e_raise : bool;
  e_tuple : bool;
  e_constructor : bool;
  e_record_construction : bool;
  e_record_functional_update : bool;  (* { e with x = e' ... } *)
  e_record_field_access : bool;
  e_record_field_update : bool;
  e_reference_update: bool ; (* PARSING: expr := expr *)
  e_if_then : bool;
  e_if_then_else : bool;
  e_array : bool ;
  e_array_update : bool;
  e_string_access : bool;
  e_sequence : bool;
  e_while : bool;
  e_for : bool;
  e_assert : bool;
  e_type_annotation : bool
}

type type_feats = {
  t_synonym : bool;
  t_variant : bool;
  t_record : bool;
  t_polymorphic : bool;    (* are type parameters allowed? *)
  t_and : bool;
}

type struct_feats = {
  s_annot_mandatory : bool;             (* every toplevel definition has a type annotation. TODO *)
  s_eval_expr : bool;                   (* e;; OR let _ = e;; *)
  s_let : let_feats option;             (* let x = e OR let f x ... = e *)
  s_let_rec : letrec_feats option;
  s_type : type_feats option;
  s_exception : bool;
  s_open : bool;
  s_semisemi_optional : bool; (* end every structure item with [;;] *)
}

type program_feats = {
  pr_expr_features : expr_feats ;
  pr_struct_features : struct_feats ;
}

let print_pattern_feats ppf pf =
  Format.fprintf ppf
    "{@[wildcard: %b@ var: %b@ constant: %b@ constructor: %b@ list: %b@ array: %b@ tuple: %b@ type annot: %b@ or: %b@ record: %b@ alias: %b@ nested: %b@]}"
    pf.p_wildcard pf.p_var pf.p_constant pf.p_constructor pf.p_list pf.p_array pf.p_tuple pf.p_type_annotation pf.p_or pf.p_record pf.p_alias pf.p_nested

let print_fun_feats ppf ff =
  Format.fprintf ppf
    "{@[pattern: @[%a@]@ fun: %b@]}"
    print_pattern_feats ff.f_pattern ff.f_fun

let print_let_feats ppf lf =
  Format.fprintf ppf
    "{@[pattern: %a@ and: %b args: %b@]}"
    print_pattern_feats lf.l_pattern lf.l_and lf.l_args

let print_letrec_feats ppf lrf =
  Format.fprintf ppf
    "{@[and: %b@ args: %b@]}"
    lrf.lr_and lrf.lr_args

let opt_print p ppf = function
  | None -> Format.pp_print_string ppf "-"
  | Some x -> p ppf x

let print_expr_feats ppf ef =
  Format.fprintf ppf
    "{@[simple var: %b@ qual var: %b@ constant: %b@ let in: %a@ let rec in: %a@ function: %a@ match: %a@ try: %a@ raise: %b@ tuple: %b@ constructor: %b@ record constr: %b@ rec func update: %b@ rec field acc: %b@ rec field update: %b@ reference update: %b@ if then: %b@ if then else: %b@ array: %b@ array update: %b@ string acc: %b@ sequence: %b@ while: %b@ for: %b@ assert: %b@ type annot: %b@]}"
   
  ef.e_simple_var
  ef.e_qualified_var
  ef.e_constructor
  (opt_print print_let_feats) ef.e_let_in
  (opt_print print_letrec_feats) ef.e_let_rec_in
  (opt_print print_fun_feats) ef.e_function
  (opt_print print_pattern_feats) ef.e_match
  (opt_print print_pattern_feats) ef.e_try
  ef.e_raise
  ef.e_tuple
  ef.e_constructor
  ef.e_record_construction
  ef.e_record_functional_update
  ef.e_record_field_access
  ef.e_record_field_update
  ef.e_reference_update
  ef.e_if_then
  ef.e_if_then_else
  ef.e_array
  ef.e_array_update
  ef.e_string_access
  ef.e_sequence
  ef.e_while
  ef.e_for
  ef.e_assert
  ef.e_type_annotation

let print_type_feats ppf tf =
  Format.fprintf ppf "{@[synonym: %b@ variant: %b@ record: %b@ polymorphic: %b and: %b@]}"
    tf.t_synonym tf.t_variant tf.t_record tf.t_polymorphic tf.t_and

let print_struct_feats ppf sf =
  Format.fprintf ppf "{@[annot mand: %b@ eval: %b@ let: %a@ let rec: %a@ type: %a@ exception: %b@ open: %b@ semi semi optional: %b@]}"
    sf.s_annot_mandatory sf.s_eval_expr (opt_print print_let_feats) sf.s_let (opt_print print_letrec_feats) sf.s_let_rec (opt_print print_type_feats) sf.s_type sf.s_exception sf.s_open sf.s_semisemi_optional

let print_program_feats ppf pf =
  Format.fprintf ppf "{@[expr: %a@ structure items: %a@]}"
    print_expr_feats pf.pr_expr_features print_struct_feats pf.pr_struct_features

(** {3 Functions to generate complete and minimal syntactic feature descriptions} *)

let all_pattern_features b =
  {
    p_wildcard = b ;
    p_var = b ;
    p_constant = b ;
    p_constructor = b ;
    p_list = b ;
    p_array = b ;
    p_tuple = b ;
    p_type_annotation = b ;
    p_or = b ;
    p_record = b ;
    p_alias = b ;
    p_nested = b 
  }

let all_let_feats b = {
  l_pattern = all_pattern_features b ;
  l_and = b ;
  l_args = b ;
}
let all_letrec_feats b = {
  lr_and = b ;
  lr_args = b ;
}

let all_expr_features b = {
  e_simple_var = b ;
  e_qualified_var = b ;
  e_constant = b ;
  e_let_in = if b then Some {l_pattern = all_pattern_features b; l_and = true; l_args = true} else None ;
  e_let_rec_in = if b then Some { lr_and = true; lr_args = true } else None ;
  e_function = if b then Some { f_pattern = all_pattern_features b; f_fun = true } else None ;
  e_match = if b then Some (all_pattern_features b) else None ;
  e_try = if b then Some (all_pattern_features b) else None ;
  e_raise = b ;
  e_tuple = b ;
  e_array = b ;
  e_array_update = b ;
  e_string_access = b ;
  e_constructor = b ;
  e_record_construction = b ;
  e_record_functional_update = b ;
  e_record_field_access = b ;
  e_record_field_update = b ;
  e_reference_update = b ;
  e_if_then = b ;
  e_if_then_else = b ;
  e_sequence = b ;
  e_while = b ;
  e_for = b ;
  e_assert = b ;
  e_type_annotation = b
}

let all_type_features b = {
  t_synonym = b ;
  t_variant = b ;
  t_record = b ;
  t_polymorphic = b ;
  t_and = b;
}

let all_struct_features b = {
  s_annot_mandatory = b ;
  s_eval_expr = b ;
  s_let = if b then Some (all_let_feats b) else None ;
  s_let_rec = if b then Some (all_letrec_feats b) else None ;
  s_type = if b then Some (all_type_features b) else None ;
  s_exception = b ;
  s_open = b ;
  s_semisemi_optional = b ;
}

let all_program_features b = {
  pr_expr_features = all_expr_features b ;
  pr_struct_features = all_struct_features b ;
}


(** {3 Functions to combine syntactic feature descriptions} *)

let pattern_features_union op f1 f2 = {
  p_wildcard = op f1.p_wildcard f2.p_wildcard ;
  p_var = op f1.p_var f2.p_var ;
  p_constant = op f1.p_constant f2.p_constant ;
  p_list = op f1.p_list f2.p_list ;
  p_array = op f1.p_array f2.p_array ;
  p_tuple = op f1.p_tuple f2.p_tuple ;
  p_constructor = op f1.p_constructor f2.p_constructor ;
  p_type_annotation = op f1.p_type_annotation f2.p_type_annotation ;
  p_or = op f1.p_or f2.p_or ;
  p_record = op f1.p_record f2.p_record ;
  p_alias = op f1.p_alias f2.p_alias ;
  p_nested = op f1.p_nested f2.p_nested ;
}

let let_features_union op lf1 lf2 = {
  l_pattern = pattern_features_union op lf1.l_pattern lf2.l_pattern ;
  l_and = op lf1.l_and lf2.l_and ;
  l_args = op lf1.l_args lf2.l_args ;
}

let letrec_features_union op lrf1 lrf2 = {
  lr_and = op lrf1.lr_and lrf2.lr_and ;
  lr_args = op lrf1.lr_args lrf2.lr_args ;
}

let maximum prf =
  let aux union sofar opf = 
    match sofar, opf with
      | None, None -> None
      | Some pf, None | None, Some pf -> Some pf
      | Some pf1, Some pf2 -> Some (union (||) pf1 pf2) in
  let max_pf =
    List.fold_left (aux pattern_features_union) None [
      Option.map ~f:(fun lf -> lf.l_pattern) prf.pr_struct_features.s_let ;
      Option.map ~f:(fun lf -> lf.l_pattern) prf.pr_expr_features.e_let_in ;
      Option.map ~f:(fun ff -> ff.f_pattern) prf.pr_expr_features.e_function ;
      prf.pr_expr_features.e_match ;
    ] in
  let max_ff = prf.pr_expr_features.e_function in
  let max_lf =
    List.fold_left (aux let_features_union) None [
      prf.pr_struct_features.s_let ;
      prf.pr_expr_features.e_let_in ;
    ] in
  let max_lrf =
    List.fold_left (aux letrec_features_union) None [
      prf.pr_struct_features.s_let_rec ;
      prf.pr_expr_features.e_let_rec_in ;
    ] in
  let max_ef = prf.pr_expr_features in
  let max_tf = prf.pr_struct_features.s_type in
  let max_sf = prf.pr_struct_features in
  let max_prf= prf in
  max_pf, max_ff, max_lf, max_lrf, max_ef, max_tf, max_sf, max_prf
