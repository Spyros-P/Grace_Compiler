.PHONY: default all clean FORCE

LLVMCONFIG=llvm-config
LLVMLDFLAGS=-L`$(LLVMCONFIG) --libdir`
LLVMPACKAGES=llvm #,llvm.analysis,llvm.all_backends,llvm.scalar_opts

OCAMLBUILD=ocamlbuild
OCAMLBUILDFLAGS=-use-menhir -pkgs $(LLVMPACKAGES) #-lflags -cclib,$(LLVMLDFLAGS)

default: main.native

main.native: FORCE
	@$(RM) *.cmi *.mli
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $@
	@cp _build/*.cmi _build/*.mli .
	@cp ~/.opam/default/lib/llvm/llvm.cmi .

clean:
	$(OCAMLBUILD) -clean
	$(RM) a.ll a.s a.out
	$(RM) *~ 
	$(RM) *.cmi *.mli
