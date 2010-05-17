##
## $Header$
##

HC = ghc
HFLAGS = -Wall -fno-warn-type-defaults

%.o: %.hs
	$(HC) $(HFLAGS) -c $< -o $@

rebuild: clean all

all: Q2_35.o Q2_36.o Q2_37.o Q2_39.o CList.o

clean:
	$(RM) *.o
	$(RM) *.hi
	$(RM) *.hs~
