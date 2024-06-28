OCB_FLAGS = -use-menhir -use-ocamlfind -pkgs core,js_of_ocaml,js_of_ocaml-ppx

all: main.js

main.byte: FORCE
	ocamlbuild $(OCB_FLAGS) main.byte

main.js: main.byte
	js_of_ocaml _build/main.byte -o ../../out/ocaml/main.js

clean:
	ocamlbuild -clean
	rm -f main.js

.PHONY: all clean FORCE
