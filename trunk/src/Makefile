OCAMLBUILD = ocamlbuild

.PHONY: main clean

main: cb.native

cb.native:
	ocamlbuild cb.native

clean: 
	ocamlbuild -clean;\
	rm -f cb.native
