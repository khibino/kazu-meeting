##
## $Header$
##

HC = ghc
HFLAGS = -Wall -fno-warn-type-defaults

targets = \
	Q2_35.o Q2_36.o Q2_37.o Q2_39.o \
	Painter.o \
	Q2_44.o Q2_45.o \
	Vect.o \
	Q2_46.o \
	Frame.o \
	Q2_47.o \
	Segment.o \
	Q2_48.o \
	FuncPainter.o \
	Q2_49.o \
	Algebra.o


%.o: %.hs
	$(HC) $(HFLAGS) -c $< -o $@

rebuild: clean all

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
