#!/bin/sh
# $Id: boot.sh,v 1.1.4.3 2007/05/14 13:59:36 pouillar Exp $
cd `dirname $0`/..
set -ex
TAGLINE='true: -use_stdlib'
./boot/ocamlrun boot/myocamlbuild.boot \
  -tag-line "$TAG_LINE" \
  boot/stdlib.cma boot/std_exit.cmo

boot/ocamlrun boot/myocamlbuild.boot \
  -tag-line "$TAG_LINE" -log _boot_log1 \
  ocamlbuild/ocamlbuildlightlib.cma ocamlbuild/ocamlbuildlight.byte

rm -f _build/myocamlbuild

boot/ocamlrun boot/myocamlbuild.boot \
  -just-plugin -install-lib-dir _build/ocamlbuild -byte-plugin

cp _build/myocamlbuild boot/myocamlbuild

./boot/ocamlrun boot/myocamlbuild tools/ocamlmklib.byte


ocamlbuild=./boot/ocamlrun boot/myocamlbuild

$ocamlbuild boot/camlheader 

# other libs
$ocamlbuild otherlibs/unix/unix.cma
$ocamlbuild otherlibs/dynlink/dynlink.cma
$ocamlbuild otherlibs/str/str.cma

# camlp4
#. ./build/camlp4-targets.sh
$ocamlbuild camlp4/boot/Camlp4.cmo
$ocamlbuild camlp4/Camlp4Parsers/Camlp4OCamlRevisedParser.cmo
$ocamlbuild camlp4/Camlp4Parsers/Camlp4OCamlParser.cmo

$ocamlbuild ecamlc
