.PHONY: all clean byte native fn

OCB_FLAGS = -tag bin_annot -I src  -use-ocamlfind -lib unix 
OCB = ocamlbuild $(OCB_FLAGS)

all: clean native 

clean:
	$(OCB) -clean
	rm -f a.out main *.ll *.s *.out *.log *.diff bash

native:
	$(OCB) bash.native
	mv bash.native bash 

byte:
	$(OCB) bash.byte

test: native
	./testall.sh
