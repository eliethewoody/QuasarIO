#!/bin/bash

echo "starting compilation..."
PWD="`pwd`"
echo "${PWD}"
#rm ./*.cmi
#rm ./*.cmo
#rm ./*.o
#rm ./*.cmx
#rm -rf ./_build
rm -rf ./bin
mkdir ./bin
cd ./bin 
cp ../*.ml ./
cp ../*.mli ./
ocamlc -c rencralga.mli
ocamlc -c rencralga.ml
ocamlc -c io.mli
ocamlc -c io.ml
ocamlc -c core.ml
ocamlc -o project.dev unix.cma str.cma rencralga.cmo io.cmo core.cmo
rm ../*.dev
cp ./*.dev ../
echo "done! executable file: project.dev"
bash ../run.sh