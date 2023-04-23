# Variables
BUILD_DIR := _build_make
LEXER_SRC := lexer.mll
PARSER_SRC := parser.mly
AST_SRC := ast.ml
MAIN_SRC := main.ml
EXECUTABLE := compiler

# Compiler and flags
OCAMLC := ocamlc
OCAMLLEX := ocamllex
MENHIR := menhir
CFLAGS := -I $(BUILD_DIR)

# Rules
.PHONY: all clean _all _clean

build:
	@rm -f *.cmi
	@rm -f *.mli
	ocamlbuild -use-menhir main.native
	@cp _build/*.cmi .
	@cp _build/*.mli .

clean:
	rm -f *.cmi
	rm -f *.mli

# beta version
_all: $(EXECUTABLE)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/lexer.ml: $(LEXER_SRC) | $(BUILD_DIR)
	$(OCAMLLEX) $< -o $@

$(BUILD_DIR)/parser.ml $(BUILD_DIR)/parser.mli: $(PARSER_SRC) | $(BUILD_DIR)
	$(MENHIR) --base _build_make/parser $<

$(BUILD_DIR)/ast.cmo: ast.ml $(BUILD_DIR)/parser.mli | $(BUILD_DIR)
	$(OCAMLC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/parser.cmi: $(BUILD_DIR)/parser.mli $(BUILD_DIR)/ast.cmo | $(BUILD_DIR)
	$(OCAMLC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/parser.cmo: $(BUILD_DIR)/parser.ml $(BUILD_DIR)/parser.cmi | $(BUILD_DIR)
	$(OCAMLC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/lexer.cmo: $(BUILD_DIR)/lexer.ml $(BUILD_DIR)/parser.cmi | $(BUILD_DIR)
	$(OCAMLC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/main.cmo: main.ml | $(BUILD_DIR)
	$(OCAMLC) $(CFLAGS) -c $< -o $@

$(EXECUTABLE): $(BUILD_DIR)/ast.cmo $(BUILD_DIR)/lexer.cmo $(BUILD_DIR)/parser.cmo $(BUILD_DIR)/main.cmo
	$(OCAMLC) $(CFLAGS) -o $@ $^

# beta version
_clean:
	rm -rf $(BUILD_DIR) $(EXECUTABLE)
