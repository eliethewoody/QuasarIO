# /bin/bash 

echo "starting compilation..."
cd /home/eliethewoody/Documents/QuasarIO/src
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
ocamlopt rencralga.mli
ocamlopt -o project.dev unix.cmxa str.cmxa rencralga.ml core.ml
rm ../*.dev
cp ./*.dev ../
echo "done! executable file: project.dev"