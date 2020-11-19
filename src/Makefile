OCAMLC = ocamlc
OCAMLOPT = ocamlopt

OPTLIBS = lablgtk.cmxa lablglade.cmxa cairo.cmxa cairo_lablgtk.cmxa gtkInit.cmx nums.cmxa unix.cmxa str.cmxa
LIBS = lablgtk.cma lablglade.cma cairo.cma cairo_lablgtk.cma gtkInit.cmo nums.cma unix.cma

INCLUDEFLAGS = -I +lablgtk2 -I +cairo

caboodle: caboodle.ml path.cmx level.cmx commondefs.cmx meshcreator.cmx
	$(OCAMLOPT) $(INCLUDEFLAGS) -o caboodle $(OPTLIBS) commondefs.cmx path.cmx level.cmx meshcreator.cmx caboodle.ml

path.cmx: path.ml path.mli
	$(OCAMLOPT) -c path.mli
	$(OCAMLOPT) -c path.ml

level.cmx: level.ml level.mli commondefs.cmx
	$(OCAMLOPT) -c commondefs.cmx level.mli
	$(OCAMLOPT) -c commondefs.cmx level.ml

commondefs.cmx: commondefs.ml
	$(OCAMLOPT) -c commondefs.ml

meshcreator.cmx: meshcreator.ml meshcreator.mli commondefs.cmx level.cmx
	$(OCAMLOPT) -c meshcreator.mli
	$(OCAMLOPT) -c commondefs.cmx meshcreator.ml

caboodleprof: caboodle.ml
	$(OCAMLOPT) -p $(INCLUDEFLAGS) -o caboodleprof $(OPTLIBS) caboodle.ml

