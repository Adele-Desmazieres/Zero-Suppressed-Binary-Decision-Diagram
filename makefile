EXEC=main
OCAMLCFLAGS=-c


.PHONY : all
all:
	ocamlc $(OCAMLCFLAGS) bigint.mli
	ocamlc $(OCAMLCFLAGS) decisiondiagram.mli
	ocamlc $(OCAMLCFLAGS) compression.mli
	
	ocamlopt $(OCAMLCFLAGS) bigint.ml
	ocamlopt $(OCAMLCFLAGS) decisiondiagram.ml
	ocamlopt $(OCAMLCFLAGS) compression.ml
	ocamlopt $(OCAMLCFLAGS) main.ml
	
	ocamlopt -o $(EXEC) bigint.cmx decisiondiagram.cmx compression.cmx main.cmx


.PHONY : clean
clean:
	rm -rf $(EXEC) *.cmi *.cmx *.o *~