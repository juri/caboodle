OCAMLFIND = ocamlfind
OCAMLOPT = ocamlopt

CP = cp

source := ../src/path.ml ../src/level.ml ../src/commondefs.ml ../src/meshcreator.ml
localsource := $(subst ../src/,,$(source))
objects := $(patsubst %.ml,%.cmx,$(localsource))

.PHONY : copyfiles clean

tests: leveltest.cmx path.cmx level.cmx commondefs.cmx meshcreator.cmx
	$(OCAMLFIND) $(OCAMLOPT) -package oUnit unix.cmxa oUnit.cmxa -o tests commondefs.cmx level.cmx leveltest.cmx

copyfiles:
	$(CP) $(source) .

path.cmx: copyfiles
	$(OCAMLOPT) -c path.ml

level.cmx: copyfiles commondefs.cmx
	$(OCAMLOPT) -c commondefs.cmx level.ml

commondefs.cmx: copyfiles
	$(OCAMLOPT) -c commondefs.ml

meshcreator.cmx: copyfiles
	$(OCAMLOPT) -c meshcreator.ml

leveltest.cmx: leveltest.ml level.cmx commondefs.cmx
	$(OCAMLFIND) $(OCAMLOPT) -package oUnit -c -o leveltest.cmx commondefs.cmx level.cmx leveltest.ml

clean:
	rm -f *.cmi *.cmx *.o tests $(localsource)
