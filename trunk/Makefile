OCAMLBUILD = ocamlbuild

main: 
	ocamlbuild -lib unix cb.native; \
	mv _build/src/main/cb.native ./bin/cb

clean: 
	ocamlbuild -clean; \
	rm bin/cb
