##
## $Header$
##

HC = ghc
HFLAGS = -Wall

sources = \
	PrimNum.hs \
	ParseResult.hs \
	SExpSyntax.hs \
	SExpParser.hs \
	Syntax.hs \
	Parser.hs \
	Evaluator.hs \


tests = \
	SExpTest.hs \
	EvalTest.hs \
	simple.hs

all_sources = $(sources) $(tests)

targets = $(all_sources:.hs=.o)


%.o: %.hs
	$(HC) $(HFLAGS) -c $< -o $@

all: $(targets)
	hlint --utf8 --color $(sources)

rebuild: clean all

push: rebuild
	hg commit
	hg kwshrink
	hg kwexpand
	hg push

clean:
	$(RM) *.o
	$(RM) *.hi
	$(RM) *.hs~
