(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ocamlbuild.ml,v 1.1.4.1 2007/11/22 18:34:13 ertai Exp $ *)
(* Original author: Nicolas Pouillard *)
Ocamlbuild_unix_plugin.setup ();
Ocamlbuild_pack.Main.main ()
