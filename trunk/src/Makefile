OCAMLOPT = $(shell PATH=$(PATH) which ocamlopt)
OCAMLBUILD = ocamlbuild -byte-plugin -ocamlopt $(OCAMLOPT)

.PHONY: main clean java clib

main:
	$(OCAMLBUILD) main/cb.native

all:
	$(OCAMLBUILD) main/cb.native clib/libcb.a java/jni/libjcb.jnilib java/cb.jar java/cb-src.jar

clib:	
	$(OCAMLBUILD) clib/libcb.a

java:	
	$(OCAMLBUILD) java/jni/libjcb.jnilib java/cb.jar java/cb-src.jar
	
clean: 
	ocamlbuild -clean;\
	rm -f cb.native
