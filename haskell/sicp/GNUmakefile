##
## $Header$
##

HC = ghc
HFLAGS = -Wall

sources = \
	Q2_35.hs Q2_36.hs Q2_37.hs Q2_39.hs \
	Painter.hs \
	Q2_44.hs Q2_45.hs \
	Vect.hs \
	Q2_46.hs \
	Frame.hs \
	Q2_47.hs \
	Segment.hs \
	Q2_48.hs \
	FuncPainter.hs \
	Q2_49.hs \
	Algebra.hs \
	Set.hs 

targets = $(sources:.hs=.o)


%.o: %.hs
	$(HC) $(HFLAGS) -c $< -o $@

rebuild: clean all
	hlint --utf8 --color $(sources)

all: $(targets)


push: rebuild
	hg commit
	hg kwshrink
	hg kwexpand
	hg push

clean:
	$(RM) *.o
	$(RM) *.hi
	$(RM) *.hs~
