.PHONY=all build run clean

test_program=hanoi.grc

all: build run

build: lexer.byte

lexer.byte: lexer.ml 
	ocamlc -o lexer.byte lexer.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

run:
	./lexer.byte < ${test_program}

clean:
	rm lexer.byte lexer.cmi lexer.cmo lexer.ml