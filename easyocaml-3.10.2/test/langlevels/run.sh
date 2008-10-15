#!/bin/bash

THIS_DIR=$(cd `dirname $0`; pwd -P)
TOP_DIR=$(cd "$THIS_DIR/../.."; pwd -P)

export EASYOCAML_USER_DIR="$THIS_DIR/easyocaml-sample-lib"
#export LOGLEVEL="INFO"

OCAMLC="$TOP_DIR/ocamlc"
OCAML="$TOP_DIR/ocaml"
OPTS="-easy -easylevel lang-foo -easyteachpack teachpack-bar -easyteachpack teachpack-spam"

cat "$1"

echo "---"

echo "running ocamlc"
"$OCAMLC" $OPTS "$1"

echo "---"

echo "running toplevel ocaml (non-interactive)"
"$OCAML" $OPTS "$1"

echo "---"

echo "running toplevel ocaml (interactive)"
echo "#use \"$1\";;" | "$OCAML" $OPTS
