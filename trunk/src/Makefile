BUILDDIR ?= _build
OCAMLBUILD ?= ocamlbuild
OCAMLBUILD := $(OCAMLBUILD) -build-dir $(BUILDDIR)

.PHONY: main clean clib

main:
	$(OCAMLBUILD) main/cb.native

all:
	$(OCAMLBUILD) main/cb.native jni cwrap/libcb.a

cwrap:	
	$(OCAMLBUILD) cwrap/libcb.a

jni:
	$(OCAMLBUILD) jni
	
clean: 
	$(OCAMLBUILD) -clean;\
	rm -f cb.native
