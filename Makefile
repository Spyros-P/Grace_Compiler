.PHONY: default all clean distclean MINIMAL FOR_DEVELOPERS

LLVMCONFIG=llvm-config
LLVMLDFLAGS=-L`$(LLVMCONFIG) --libdir`
LLVMPACKAGES=llvm,llvm.analysis,llvm.all_backends,llvm.scalar_opts,llvm.ipo,llvm.vectorize

OCAMLBUILD=ocamlbuild
OCAMLBUILDFLAGS=-use-menhir -pkgs $(LLVMPACKAGES) -lflags -cclib,$(LLVMLDFLAGS)

ifndef OPAM_GRACE_PATH
	OPAM_GRACE_PATH=~/.opam/default
endif

default: main.native lib/libmylib.a

main.native: MINIMAL

MINIMAL:
	@$(RM) *.cmi *.mli
	@$(RM) main.native
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) main.native
	@$(RM) main.native
	@cp _build/main.native .

FOR_DEVELOPERS:
	@$(RM) *.cmi *.mli
	@$(RM) main.native
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) main.native
	@$(RM) main.native
	@cp _build/main.native .
	@cp _build/*.cmi _build/*.mli .
	@cp ${OPAM_GRACE_PATH}/lib/llvm/llvm.cmi .
	@cp ${OPAM_GRACE_PATH}/lib/llvm/llvm.mli .
	@cp ${OPAM_GRACE_PATH}/lib/llvm/llvm_analysis.cmi .
	@cp ${OPAM_GRACE_PATH}/lib/llvm/llvm_analysis.mli .
	@cp ${OPAM_GRACE_PATH}/lib/llvm/llvm_scalar_opts.cmi .
	@cp ${OPAM_GRACE_PATH}/lib/llvm/llvm_scalar_opts.mli .
	@cp ${OPAM_GRACE_PATH}/lib/llvm/llvm_all_backends.cmi .
	@cp ${OPAM_GRACE_PATH}/lib/llvm/llvm_all_backends.mli .
	@cp ${OPAM_GRACE_PATH}/lib/llvm/llvm_ipo.cmi .
	@cp ${OPAM_GRACE_PATH}/lib/llvm/llvm_ipo.mli .
	@cp ${OPAM_GRACE_PATH}/lib/llvm/llvm_vectorize.cmi .
	@cp ${OPAM_GRACE_PATH}/lib/llvm/llvm_vectorize.mli .

lib/libmylib.a: lib/mylib.c lib/mylib.h
	gcc -c lib/mylib.c -o lib/mylib.o -Wno-builtin-declaration-mismatch
	ar rcs lib/libmylib.a lib/mylib.o

clean:
	$(RM) -r _build
	$(RM) *.cmi *.mli
	$(RM) lib/mylib.o

distclean: clean
	$(RM) lib/libmylib.a
	$(RM) main.native
