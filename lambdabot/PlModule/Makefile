all:	
	ghc -package HUnit -i.. -O --make -fglasgow-exts -o Test Test.hs
	#~/ghc-6.5.20050324/bin/i386-unknown-linux/ghc -iHUnit -i.. --make -O -fglasgow-exts -o Test Test.hs

tests:	all
	./Test tests

clean:
	rm *.o *.hi
