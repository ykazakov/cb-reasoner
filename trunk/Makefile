OCAMLBUILD = ocamlbuild

main: 
	ocamlbuild cb.native; \
	mv _build/src/main/cb.native ./bin/cb; \
	unlink cb.native

clean: 
	ocamlbuild -clean; \
	rm -f bin/cb
