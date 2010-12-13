OCAMLBUILD = ocamlbuild
OCAMLOPT = $(shell which ocamlopt)

.PHONY: main clean

main:
# for cross-compilation, different versions of ocamlbuild and ocamlopt can be used
	$(OCAMLBUILD) -byte-plugin -ocamlopt $(OCAMLOPT) cb.native

clean: 
	ocamlbuild -clean;\
	rm -f cb.native