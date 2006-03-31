BINDIR = /usr/local/bin
#
GHC = ghc
GHCFLAGS = -Wall
#GHCFLAGS = -Wall -prof -auto-all -O2

.PHONY:all
all:	djinn

djinn:	*.hs
	${GHC} ${GHCFLAGS} --make Djinn.hs -o djinn

MLJT:	*.hs
	${GHC} ${GHCFLAGS} --make MLJT.hs -o MLJT

Help.hs:	verbose-help
	echo 'module Help where' > Help.hs
	echo 'verboseHelp :: String' >> Help.hs
	echo 'verboseHelp = "\' >> Help.hs
	sed -e 's/\\/\\\\/g' -e 's/"/\\"/' -e 's/^/\\/' -e 's/$$/\\n\\/' verbose-help >> Help.hs
	echo '\"' >> Help.hs

.PHONY:	check
check:	djinn
	./djinn examples > out
	diff out examples.out

.PHONY: test
test:	MLJT
	cd tests; ${MAKE}

.PHONY:	newversion
newversion:
	mv Djinn.hs Djinn.hs.old
	sed "s/20..-..-../`date +%Y-%m-%d`/" Djinn.hs.old > Djinn.hs

.PHONY:	dist
dist:
	darcs dist

.PHONY:	install
install:	djinn
	cp djinn $(BINDIR)

.PHONY: clean
clean:
	rm -f djinn MLJT a.out *.o *.hi *.old out */*.o */*.hi
	cd tests; ${MAKE} clean
