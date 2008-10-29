(* More code regarding construction of parsing errors is in module Failed. *)

(* Easily access constructors for ParseError.t's constituents *)
module C = Sig.Grammar.ParseErrorTs;

module Make (SpecificError: Sig.TypeWithToString) = struct

  module SymbolDesc = struct
    type t = Sig.Grammar.ParseErrorTs.symbol_desc;
    value to_string = 
      fun
      [ C.Entry (name, None) -> "[" ^ name ^ "]"
      | C.Entry (name, Some level) -> "[" ^ name ^ " level " ^ level ^ "]"
      | C.Token descr -> descr
      | C.Keyword kwd -> "\"" ^ kwd ^ "\""
      | C.Unknown -> "???" ];
  end;

  module Expected = struct
    type t = Sig.Grammar.ParseErrorTs.expected;
    value rec to_string = fun
      [ C.Symbol s -> SymbolDesc.to_string s
      | C.Or_list exps -> String.concat " or " (List.map to_string exps)
      | C.Then_list strs -> String.concat " then " strs ];
  end;

  module SpecificError = SpecificError;

  type t = 
    [ Expected of Expected.t and option SymbolDesc.t and string
    | Illegal_begin of SymbolDesc.t
    | Failed
    | Specific_error of SpecificError.t ];

  exception E of t;

  value to_string = fun
    [ Expected (exp, None, context) ->
        Expected.to_string exp ^ " expected (in [" ^ context ^ "])"
    | Expected (exp, Some sd, context) ->
        Expected.to_string exp ^ " expected after " ^ SymbolDesc.to_string sd ^ " (in [" ^ context ^ "])"
    | Illegal_begin sd ->
        "illegal begin of " ^ SymbolDesc.to_string sd
    | Failed ->
        "failed"
    | Specific_error err ->
        SpecificError.to_string err ];

  value encode: t -> string =
    fun err ->
      to_string err ^ "\000" ^ Marshal.to_string err [];

  value encode_specific : SpecificError.t -> string =
    fun specific ->
      encode (Specific_error specific);

  value raise_stream_error err =
    raise (Stream.Error (encode err));

  value decode code =
    try
      let unmarshal_parse_error : string -> int -> t = Marshal.from_string in
      let i = String.index code '\000' in
      (String.sub code 0 i, Some (unmarshal_parse_error code (succ i)))
    with [ Not_found -> (code, None) ];

end;
