main.native: *.ml *.mli *.mly *.mll
	ocamlbuild -use-ocamlfind main.native
	mv main.native praline

main.byte: *.ml *.mli *.mly *.mll
	ocamlbuild -libs str,graph,unix -cflags -I,+ocamlgraph -lflags -I,+ocamlgraph main.byte

main.p.native: *.ml *.mli
	ocamlbuild -libs str,graph -cflags -I,+ocamlgraph -lflags -p,-I,+ocamlgraph main.p.native

doc: *.ml *.mli main.native
	ocamldoc -I +ocamlgraph -I _build/ -html -d docdir *.mli

sanitize:
	rm -f *.o *.cmo *.cmi *.cmx

clean:
	ocamlbuild -clean


