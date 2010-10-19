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
	AlgebraD.hs \
	Set.hs \
	BinTree.hs \
	Q2_63.hs \
	Huffman.hs \
	InsatiableE.hs \
	Complex.hs \
	ComplexN.hs \
	Poly.hs \
	HoshiST.hs \
	Q3_01.hs \
	Q3_02.hs \
	Q3_05.hs

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
