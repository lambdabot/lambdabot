all:	
	#ghc -package HUnit -i.. -O --make -fglasgow-exts -funfolding-use-threshold -o Test Test.hs
	ghc -Wall -cpp -funbox-strict-fields -i../.. --make -O -fglasgow-exts -o Test Test.hs

tests:	all
	./Test tests

clean:
	rm *.o *.hi
