
SYNTAX_SOURCES=Syntax.hs Lex.hs Tokens.hs AST.hs
SOURCES=$(SYNTAX_SOURCES) Semantic.hs Semantic3.hs
#SymbolTable.hs
#DEBUG_DIR=./../../../debug/parser
#DEBUG_DIR=~/programms/svn/debug/parser
DEBUG_DIR=../../debug/mcad2

#EXE_PREFIX=TestEval
#EXE_PREFIX=TestSyntax
#EXE_PREFIX=TestStage2
#EXE_PREFIX=Compile
#EXE_PREFIX=TestDecompose
#EXE_PREFIX=TestHGraphSemantic
EXE_PREFIX=TestHGraphToVerilog
#EXE_PREFIX=TestSemantic

#EXE_EXTRA_SOURCES=Decompose.hs
#EXE_EXTRA_SOURCES=Semantic.hs Semantic3.hs
#EXE_EXTRA_SOURCES=HGraphSemantic.hs
EXE_EXTRA_SOURCES=HGraphSemantic.hs HGraphToVerilog.hs

EXE_SUFFIX=.exe
EXE_PATH=$(DEBUG_DIR)/$(EXE_FILE)
EXE_FILE=$(EXE_PREFIX)$(EXE_SUFFIX)
EXE_SOURCES=$(EXE_PREFIX).hs $(EXE_EXTRA_SOURCES)
#GHC_FLAGS=-fastm

all: $(EXE_PATH)

$(EXE_PATH): $(SOURCES) $(EXE_SOURCES)
	ghc --make $(SOURCES) $(EXE_SOURCES) -o $(EXE_PATH) -outputdir $(DEBUG_DIR)/obj


Syntax.hs: Syntax.y Lex.hs
	happy Syntax.y

Lex.hs: Lex.x
	alex Lex.x

clean:
	rm -f $(DEBUG_DIR)/obj/*.hi
	rm -f $(DEBUG_DIR)/obj/*.o
	rm -f $(DEBUG_DIR)/*.exe

test_clean:
	cd $(DEBUG_DIR); $(MAKE) clean

test:	$(EXE_PATH)
	cd $(DEBUG_DIR); $(MAKE) -e EXE_NAME=$(EXE_FILE)

$(DEBUG_DIR)/vp.exe: VPSimple.hs TestVPSimple.hs
	ghc --make VPSimple.hs TestVPSimple.hs -o $(DEBUG_DIR)/vp.exe -outputdir $(DEBUG_DIR)/obj

vp: $(DEBUG_DIR)/vp.exe

vptest: $(DEBUG_DIR)/vp.exe
	cd $(DEBUG_DIR); $(MAKE) -e EXE_NAME=$(DEBUG_DIR)/vp.exe

#Syntax Test {{{

syntax:	$(DEBUG_DIR)/TestSyntax.exe
	echo "type make test_syntax"

test_syntax: $(DEBUG_DIR)/TestSyntax.exe
	cd $(DEBUG_DIR); $(MAKE) -e EXE_NAME=TestSyntax.exe

$(DEBUG_DIR)/TestSyntax.exe: $(SYNTAX_SOURCES) TestSyntax.hs
	ghc --make $(SOURCES) TestSyntax.hs -o $(DEBUG_DIR)/TestSyntax.exe -outputdir $(DEBUG_DIR)/obj

#}}}



