open EzyLangLevel

let _ =
  let min_patt =
    { (all_pattern_feats false) with
        p_var = true } in
  let match_pf =
    { (all_pattern_feats false) with
        p_wildcard = true;
        p_constant = true;
        p_list = true; } in
  let let_feats =
    { l_pattern = min_patt;
      l_args = false;
      l_and = true; } in
  let let_rec_feats =
    { lr_and = true;
      lr_args = false } in
  let func_feats = 
    { f_fun = false; f_pattern = min_patt } in
  let program_feats = 
    { pr_expr_feats =
        { (all_expr_feats false) with
            e_constant = true;
            e_function = func_feats;
            e_if_then_else = true;
            e_list = true;
            e_match = true;
            e_simple_var = true; };
      pr_struct_feats = 
        { (all_struct_feats false) with
            s_annot_optional = false;
            s_let = Some let_feats;
            s_let_rec = Some let_rec_feats;
            s_semisemi_optional = true } in
  configure program_feats ["Mypervasives", true; "Mylist", true]
