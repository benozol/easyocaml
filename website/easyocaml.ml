open Lwt
open Eliom_services
open Eliom_parameters
open Eliom_predefmod
open XHTML.M

let (|>) x f = f x

let ocamlrun = "/usr/bin/ocamlrun"
let installation str = "/home/benus/install/" ^ str
let easyocamldir str = "/home/benus/install/easyocaml/" ^ str
let language_levels = [
  "", "None", "No language level. Full EasyOCaml.";
  "lang-minimal", "minimal", "Available functions: cons, iter, map, fold from List and +, -, *, /, succ, pred form Pervasives. Hard restrictions on the syntax.";
  "lang-advanced", "advanced", "Access to the modules Pervasives (preopened), Char, Random, String. No imperative language features and mandatory type annotations for toplevel values.";
]

type lang = [ `En | `Fr | `De ]
let string_to_lang: string -> lang = function
  | "english" -> `En
  | "française" -> `Fr
  | "deutsch" -> `De
  | _ -> invalid_arg "string_to_lang"
let lang_to_string: lang -> string = function
  | `En -> "english"
  | `Fr -> "française"
  | `De -> "deutsch"
let lang_to_envvar: lang -> string = function
  | `En -> "en"
  | `Fr -> "fr"
  | `De -> "de"


let easyocamlc lang level program =

  if lang = `Fr then
    return "Ja, french would be nice. But I am not able to speak it. I would be happy if you could help me here."
  else

  (* /usr/bin/ocamlrun installation/bin/ocamlc -easy -I installation/lib/ocaml x.ml *)
  (* let errorprinter = "htmlErrorPrinter.cmo" in *)

  let file =
    let ic = Unix.open_process_in "mktemp" in
    let file = input_line ic ^ ".ml" in
    let oc = open_out file in
    output_string oc program;
    close_out oc;
    file in

  let cmd_ecamlc =
    let params = [
      ocamlrun;
      installation "bin/ocamlc";
      "-I " ^ installation "lib/ocaml";
      "stdlib.cma";
      "-easy";
      "-easyerrorprinter " ^ easyocamldir "htmlErrorReport.cmo"; 
      if level = "" then "" else " -easylevel " ^ level;
      file;
    ] in
    String.concat " " params in

  let env_vars =
    let li = [
      "LANGUAGE", lang_to_envvar lang;
      "LANG", lang_to_envvar lang;
      "EASYOCAML_USER_DIR", easyocamldir "";
    ] in
    Array.of_list (List.map (fun (k,v) -> k ^ "=" ^ v) li) in

  let ic =
    Array.iter print_endline env_vars;
    print_endline cmd_ecamlc;
    let _, _, ic = Unix.open_process_full cmd_ecamlc env_vars in
    Unix.descr_of_in_channel ic |>
    Lwt_unix.of_unix_file_descr |>
    Lwt_chan.in_channel_of_descr in

  let buf = Buffer.create 100 in
  catch
    (fun () ->
       let rec aux () =
         Lwt_chan.input_line ic >>= fun line ->
         Buffer.add_string buf (line ^ "\n") ;
         aux () in
       aux ())
    (function End_of_file -> return () | x -> raise x) >>= fun () ->
  return (Buffer.contents buf)


(* Services *)

let easyocaml_main =
  new_service
    ~path:[]
    ~get_params:Eliom_parameters.unit
     ()

let easyocaml_demo =
  let lang_param = Eliom_parameters.radio (Eliom_parameters.user_type string_to_lang lang_to_string) "language" in
  let level_param = Eliom_parameters.string "level" in
  let program_param = Eliom_parameters.string "program" in
  new_post_service
    ~fallback:easyocaml_main
    ~post_params:(lang_param ** (level_param ** program_param))
    ()


(* Handler *)

let ocsigen = div [a ~a:[a_href (uri_of_string "http://ocsigen.org/")] [
  img ~alt:"Ocsigen powered" ~src:(uri_of_string "http://ocsigen.org/ocsigen8-100x30.png") ()
]]

let document sp headline content =
  return
    (html
       (head (title (pcdata headline)) [
         Xhtml.css_link ~uri:(Xhtml.make_uri ~service:(static_dir sp) ~sp ["style.css"]) ();
         Xhtml.css_link ~uri:(Xhtml.make_uri ~service:(static_dir sp) ~sp ["easyocaml.css"]) ();
         Xhtml.js_script ~uri:(Xhtml.make_uri ~service:(static_dir sp) ~sp ["easyocaml.js"]) ();
       ])
       (body content))

let easyocaml_main_handler sp () () =
  document sp "EasyOCaml" [
    h2 [pcdata "EasyOCaml"];
    Xhtml.post_form easyocaml_demo sp
      (fun (language, (level, program)) -> 
         let radio lang =
           span [
             Xhtml.user_type_radio ~name:language ~value:lang lang_to_string; 
             pcdata (lang_to_string lang);
             br () ;
           ] in
         [ p ([pcdata "Human language:"; br ()] @ List.map radio [`En; `Fr; `De]);
           p [
             pcdata "Language level (hover for informations): "; br ();
             let first, rest =
               let option_for_level (level, name, descr) = [a_title descr], level, Some (pcdata name), false in
               match List.map option_for_level language_levels with
                 | (attrs, value, content, sel) :: rest ->
                     let mk_option (a,b,c,d) = Xhtml.Option (a,b,c,d) in
                     mk_option (attrs, value, content, true), List.map mk_option rest
                 | _ -> failwith "no language levels defined" in
             Xhtml.string_select ~name:level first rest
           ];
           p [
             pcdata "Program code:"; br ();
             Xhtml.textarea ~name:program ~rows:14 ~cols:80 ();
           ];
           p [Xhtml.string_input ~input_type:`Submit ~value:"check" ()];
           ocsigen
         ]) ()
  ]

let easyocaml_demo_handler sp () (language, (level, program)) =
  easyocamlc (match language with Some l -> l | None -> `En) level program >>= fun html ->
  document sp "EasyOCaml demo" [
    h2 [pcdata "EasyOCaml demo"];
    (unsafe_data html)
  ]


let _ =
  Xhtml.register easyocaml_main easyocaml_main_handler;
  Xhtml.register easyocaml_demo easyocaml_demo_handler;
