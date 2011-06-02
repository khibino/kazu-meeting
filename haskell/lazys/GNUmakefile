##
## $Header$
##

HC = ghc
HFLAGS = -Wall

sources = \
	Syntax.hs \
	Evaluator.hs \
	TestEval.hs \
	SExpSyntax.hs \
	SExpParser.hs


targets = $(sources:.hs=.o)


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
