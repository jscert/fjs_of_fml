CC := ocamlc -c -nopervasives

all: stdlib_fml.cmi stdlib_fml.cmo

%.cmi: stdlib_fml.mli
	$(CC) $<

stdlib_fml.cmo: stdlib_fml.ml stdlib_fml.cmi
	$(CC) $<

clean:
	rm -f *.cmi *.cmo

.PHONY: all clean
