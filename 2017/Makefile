.PHONY: clean

OCB = ocamlbuild
OCB_FLAGS = -tag bin_annot -I src -I lib -pkgs Core,ppx_jane -lflags -thread -use-ocamlfind

% : src/%.ml
	$(OCB) $(OCB_FLAGS) $@.native

clean:
	$(OCB) -clean
