
module Make (SpecificError: Sig.TypeWithToString) = struct

  module SymbolDesc = struct
    type t = Sig.Grammar.ParseErrorTypes.symbol_desc ==
      [ Entry of string and option string
      | Token of string
      | Keyword of string
      | Unknown ];
    value to_string = fun
      [ Entry (name, None) -> "[" ^ name ^ "]"
      | Entry (name, Some level) -> "[" ^ name ^ " level " ^ level ^ "]"
      | Token descr -> descr
      | Keyword kwd -> "\"" ^ kwd ^ "\""
      | Unknown -> "???" ];
  end;

  module Expected = struct
    type t = Sig.Grammar.ParseErrorTypes.expected ==
      [ Symbol of SymbolDesc.t
      | Or_list of list t
      | Then_list of list string ];
    value rec to_string = fun
      [ Symbol s -> SymbolDesc.to_string s
      | Or_list exps -> String.concat " or " (List.map to_string exps)
      | Then_list strs -> String.concat " then " strs ];
  end;

  module SpecificError = SpecificError;

  type t = 
    [ Expected of Expected.t and option SymbolDesc.t and string
    | Illegal_begin of SymbolDesc.t
    | Specific_error of SpecificError.t ];

  exception E of t;

  value to_string = fun
    [ Expected (exp, None, context) ->
        Expected.to_string exp ^ " expected (in [" ^ context ^ "])"
    | Expected (exp, Some sd, context) ->
        Expected.to_string exp ^ " expected after " ^ SymbolDesc.to_string sd ^ " (in [" ^ context ^ "])"
    | Illegal_begin sd ->
        "illegal begin of " ^ SymbolDesc.to_string sd
    | Specific_error err ->
        SpecificError.to_string err ];

  value decode code =
    try
      let unmarshal_parse_error : string -> int -> t = Marshal.from_string in
      let i = String.index code '\000' in
      (String.sub code 0 i, Some (unmarshal_parse_error code (succ i)))
    with [ Not_found -> (code, None) ];

end;
