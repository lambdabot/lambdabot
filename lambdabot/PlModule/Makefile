all:	
	#ghc -package HUnit -i.. -O --make -fglasgow-exts -funfolding-use-threshold -o Test Test.hs
	~/ghc-6.5.20050324/bin/i386-unknown-linux/ghc -cpp -funbox-strict-fields -iHUnit -i.. --make -O -fglasgow-exts -o Test Test.hs

tests:	all
	./Test tests

clean:
	rm *.o *.hi
