#!/bin/bash
echo "we are here"
PWD="`pwd`"
echo "${PWD}"
echo "copying source files to bin..."
cd ..
echo "we are here ${PWD}"
ls -la
rm -rf ./bin 
mkdir ./bin 
cp ./src/*.ml ./bin
cp ./src/*.mli ./bin
cd ./bin 
echo "we are here ${PWD}"
ls -la
echo "done!"
echo "starting compilation..."
echo "we are here ${PWD}"
ocamlc -c rencralga.mli
ocamlc -c rencralga.ml
ocamlc -c io.mli
ocamlc -c io.ml
ocamlc -c core.ml
ocamlc -o project.dev unix.cma str.cma rencralga.cmo io.cmo core.cmo
cp ./*.dev ../
cd ..
echo "we are here ${PWD}"
chmod a+x ./*.dev
echo "done!"
echo "removing source files from bin..."
cd ./bin
echo "we are here ${PWD}"
ls -la
rm ./*.ml
rm ./*.mli
rm ./*.cmi
rm ./*.cmo
ls -la
cd ..
echo "we are here ${PWD}"
echo "done!"
./project.dev
echo ""
