#
# Usage:
#    make all  # not implemented yet, will build everything
#    make full    # build *.log.js, *.unlog.js, *.token.js
#    make lineof  # build lineof.js
#    make interp  # build interp.js
#
# requires: opam switch 4.02.1; eval `opam config env`

# TODO: test/lambda is not longer supported

###############################################################
# Paths

STDLIB_DIR  := stdlib_ml
TESTS_DIR   := tests
JSREF_DIR   := jsref
JSREF_PATH  := $(TESTS_DIR)/$(JSREF_DIR)
JSREF_ML    := $(wildcard $(JSREF_PATH)/*.ml) 
JSREF_MLI   := $(wildcard $(JSREF_PATH)/*.mli)


###############################################################

# ASSEMBLY_JS must respect dependencies order
ASSEMBLY_JS_FILES := \
	BinNums.unlog.js \
	Bool0.unlog.js \
	List0.unlog.js \
	Datatypes.unlog.js \
	Fappli_IEEE_bits.unlog.js \
	Fappli_IEEE.unlog.js \
	LibBool.unlog.js \
	LibReflect.unlog.js \
	LibOperation.unlog.js \
	LibList.unlog.js \
	LibString.unlog.js \
	LibOption.unlog.js \
	LibTactics.unlog.js \
	LibProd.unlog.js \
	LibFunc.unlog.js \
	Heap.unlog.js \
	Shared.unlog.js \
	Compare.js \
	Debug.js \
	JsNumber.js \
	JsSyntax.unlog.js \
	JsSyntaxAux.unlog.js \
	Translate_syntax.js \
	JsSyntaxInfos.unlog.js \
	JsCommon.unlog.js \
	JsCommonAux.unlog.js \
	JsPreliminary.unlog.js \
	JsInit.unlog.js \
	JsInterpreterMonads.unlog.js \
	JsInterpreter.log.js
ASSEMBLY_JS := $(STDLIB_DIR)/stdlib.js $(addprefix tests/jsref/,$(ASSEMBLY_JS_FILES));


###############################################################

DISPLAYED_JS_FILES := \
	JsInterpreter.unlog.js

DISPLAYED_JS := $(addprefix tests/jsref/,$(DISPLAYED_JS_FILES));


###############################################################
# Global options

all: everything

.PHONY: all clean .log.js .unlog.js .token.js
   # all gen log unlog 

# Do not delete intermediate files.
.SECONDARY:


###############################################################
# Tools

CC          := ocamlc -c
OCAMLDEP    := ocamldep -one-line
OCAMLBUILD := ocamlbuild -j 4 -classic-display -use-ocamlfind -X tests -X $(STDLIB_DIR)

GENERATOR := ./main.byte

LINEOF := ./lineof.byte


###############################################################
# Dependencies

ifeq ($(filter clean%,$(MAKECMDGOALS)),)
#-include $(JSREF_ML:.ml=.ml.d)
-include $(JSREF_PATH)/.depends
endif


###############################################################
# Rules

##### Compilation of STDLIB

$(STDLIB_DIR)/stdlib.cmi: $(STDLIB_DIR)/stdlib.mli
	$(CC) $<

##### Rule for binaries

%.byte: *.ml _tags
	$(OCAMLBUILD) $@

##### Rule for dependencies

$(JSREF_PATH)/.depends: $(JSREF_ML)
	$(OCAMLDEP) -all -I $(<D) $(<D)/* > $@

##### Rule for cmi

tests/%.cmi: tests/%.ml main.byte stdlib
	./main.byte -mode cmi -I $(<D) $<

tests/%.cmi: tests/%.mli stdlib
	ocamlc -I $(JSREF_PATH) -I stdlib_ml -open Stdlib $<

##### Rule for log/unlog/token

tests/%.log.js: tests/%.ml main.byte stdlib tests/%.cmi
	./main.byte -mode log -I $(<D) $<

tests/%.unlog.js: tests/%.ml main.byte stdlib tests/%.cmi
	./main.byte -mode unlog -I $(<D) $<

tests/%.token.js: tests/%.ml main.byte stdlib tests/%.cmi
	./main.byte -mode token -I $(<D) $<

##### Rule for lineof.js

$(JSREF_PATH)/lineof.js: lineof.byte $(JSREF_ML:.ml=.token.js)
	./lineof.byte -o $@ $(JSREF_ML:.ml=.token.js)

##### Rule for assembly.js

# later add as dependencies the unlog files: $(JSREF_ML:.ml=.unlog.js) 
$(JSREF_PATH)/assembly.js: assembly.byte $(ASSEMBLY_JS)
	./assembly.byte -o $@ $(ASSEMBLY_JS)
# -stdlib $(STDLIB_DIR)/stdlib.js 

##### Rule for displayed_sources.js

$(JSREF_PATH)/displayed_sources.js: displayed_sources.byte $(DISPLAYED_JS)
	./displayed_sources.byte -o $@ $(DISPLAYED_JS)


#### maybe useful ??

tests/jsref/%.log.js: tests/jsref/%.ml 


#####################################################################
# Short targets

everything: gen assembly lineof display

main: main.byte

cmi: $(JSREF_ML:.ml=.cmi) $(JSREF_MLI:.mli=.cmi) 

gen: $(JSREF_ML:.ml=.log.js) $(JSREF_ML:.ml=.unlog.js) $(JSREF_ML:.ml=.token.js)

log: $(JSREF_ML:.ml=.log.js) $(JSREF_ML:.ml=.token.js)

unlog: $(JSREF_ML:.ml=.unlog.js) 

lineof: $(JSREF_PATH)/lineof.js

assembly: $(JSREF_PATH)/assembly.js

display: $(JSREF_PATH)/displayed_sources.js

stdlib: $(STDLIB_DIR)/stdlib.cmi



#####################################################################
# Clean

DIRTY_EXTS := cmi,token.js,log.js,unlog.js,d,ml.d,mli.d,js.pre

clean_genjs:
	rm -f $(JSREF_PATH)/lineof.js
	rm -f $(JSREF_PATH)/assembly.js

clean_tests:
	bash -c "rm -f $(TESTS_DIR)/*.{$(DIRTY_EXTS)}"
	bash -c "rm -f $(TESTS_DIR)/$(JSREF_DIR)/*.{$(DIRTY_EXTS)}"
	bash -c "rm -f $(JSREF_PATH)/.depends"

clean_stdlib:
	rm -f $(STDLIB_DIR)/*.cmi

clean: clean_genjs clean_tests clean_stdlib
	rm -rf _build
	rm -f *.native *.byte



#####################################################################
# Extra

debug: main.d.byte

native: _tags
	$(OCAMLBUILD) main.native

##### Shorthand

tests/%.all: tests/%.log.js tests/%.unlog.js tests/%.token.js
	touch $@

#####################################################################
# Original Build of JSRef Coq to "Humanified" OCaml

#tests/%.ml: tests/%.v
#	$(MAKE) -C $(CURDIR)/../../lib/tlc/src
#	cd $(<D) && coqc -I $(CURDIR)/../../lib/tlc/src $(<F)
#	cd $(@D) && rm *.mli
#	cd $(@D) && $(CURDIR)/../ml-add-cstr-annots.pl *.ml

#$(JSREF_PATH)/%.ml:
#	$(MAKE) -C $(CURDIR)/../.. interpreter
#	cp ../../interp/src/extract/*.ml $(JSREF_PATH)/
#	../convert-ml-strings.pl $(JSREF_PATH)/*.ml
#	cd $(@D) && $(CURDIR)/../ml-add-cstr-annots.pl *.ml
