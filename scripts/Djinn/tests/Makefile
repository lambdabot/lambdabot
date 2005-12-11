.PHONY: all
all:	fast

Test:	Test.hs ../*.hs
	ghc -O2 -i.. --make Test.hs -o Test

.PHONY: time
time:	Test
	uname -a >> test.time
	time ./Test testfiles.fast > /dev/null 2>> test.time

.PHONY: fast
fast:	Test testfiles.fast
	./Test testfiles.fast

.PHONY: slow
slow:	Test testfiles.all
	./Test testfiles.all

.PHONY: clean
clean:
	rm -f Test *.o *.hi
