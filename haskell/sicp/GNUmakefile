##
## $Header$
##

HC = ghc
HFLAGS = -Wall -fno-warn-type-defaults

targets = \
	Q2_35.o Q2_36.o Q2_37.o Q2_39.o \
	SplitPainter.o \
	Q2_44.o Q2_45.o

%.o: %.hs
	$(HC) $(HFLAGS) -c $< -o $@

rebuild: clean all

all: $(targets)


push: rebuild
	hg commit
	hg shrink
	hg kwexpand
	hg push

clean:
	$(RM) *.o
	$(RM) *.hi
	$(RM) *.hs~
