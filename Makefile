.PHONY: ast

# Usage: make ast FILE=path/to/file.ml
ast:
	@test -n "$(FILE)" || (echo "Usage: make ast FILE=path/to/file.ml" >&2; exit 1)
	ocamlfind ocamlc -dparsetree -c $(FILE)
	@rm -f $(basename $(FILE)).cmi $(basename $(FILE)).cmo

clean:
	@rm -rf examples/*.exe examples/*.generated*
