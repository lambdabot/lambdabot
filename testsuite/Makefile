HC_OPTS=        -fasm -O2 -fth -fglasgow-exts -i..
PP=             ./logpp
GHC=            ghc $(HC_OPTS) --make -pgmF $(PP) -F -threaded

all: run-utests

.PHONY: run-utests
run-utests: 
	$(GHC) -o $@ UnitTestsMain.hs

.PHONY: clean
clean:
	rm -f *.o *.hi $(BINS) run-utests
