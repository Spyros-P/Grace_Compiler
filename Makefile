.PHONY: default all clean FORCE

LLVMCONFIG=llvm-config
LLVMLDFLAGS=-L`$(LLVMCONFIG) --libdir`
LLVMPACKAGES=llvm,llvm.analysis,llvm.all_backends,llvm.scalar_opts

OCAMLBUILD=ocamlbuild
OCAMLBUILDFLAGS=-use-menhir -pkgs $(LLVMPACKAGES) #-lflags -cclib,$(LLVMLDFLAGS)

default: main.native

main.native: FORCE
	@$(RM) *.cmi *.mli
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $@
	@cp _build/*.cmi _build/*.mli .
	@cp ~/.opam/default/lib/llvm/llvm.cmi .
	@cp ~/.opam/default/lib/llvm/llvm.mli .
	@cp ~/.opam/default/lib/llvm/llvm_analysis.cmi .
	@cp ~/.opam/default/lib/llvm/llvm_analysis.mli .
	@cp ~/.opam/default/lib/llvm/llvm_scalar_opts.cmi .
	@cp ~/.opam/default/lib/llvm/llvm_scalar_opts.mli .
	@cp ~/.opam/default/lib/llvm/llvm_all_backends.cmi .
	@cp ~/.opam/default/lib/llvm/llvm_all_backends.mli .

libmylib.a: mylib.c mylib.h
	gcc -c mylib.c -o mylib.o
	ar rcs libmylib.a mylib.o
	@$(RM) mylib.o

clean:
	$(OCAMLBUILD) -clean
	$(RM) a.ll a.s a.out
	$(RM) *~ 
	$(RM) *.cmi *.mli
