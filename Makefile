OCAMLYACC=menhir
PACKS=llvm llvm.analysis # all relevant -I options here
SRCDIR=src
SOURCES=$(addprefix $(SRCDIR)/,\
				misc.ml mySet.ml myMap.ml pretty.ml \
				syntax.ml parser.mly lexer.mll \
				environment.ml normal.ml closure.ml \
				flat.ml c99.ml vm.ml llvmIR.ml arm_spec.ml arm_noreg.ml \
				mips_spec.ml mips_noreg.ml \
				cfg.ml dfa.ml live.ml reg.ml \
				opt.ml arm_reg.ml main.ml)
OCAMLFLAGS=$(INCLUDES) -annot -bin-annot
OCAMLOPTFLAGS=$(INCLUDES) -annot -bin-annot

RESULT=minimlc

-include OCamlMakefile

all: bc

.PHONY: test
test: all
	./test.sh
