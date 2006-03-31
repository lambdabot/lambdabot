#
# Simple Makefile for command line tool
#

GHC=            ghc
HC_OPTS=        -O

.PHONY: all

all: hoogle

hoogle: 
	cd src && $(GHC) $(HC_OPTS) --make -o hoogle_  CmdLine.hs
	mv src/hoogle_ .
	cp src/hoogle.txt .


clean:
	rm -rf hoogle_
	find src -name '*.hi' -o -name '*.o' -exec rm -rf {} \;
	rm -f src/*.o src/*.hi src/*/*.o src/*/*.hi
	rm -f hoogle.txt
