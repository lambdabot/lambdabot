# $Id: Makefile,v 1.6 2003/07/31 19:13:15 eleganesh Exp $


DYNMODS=Dict Dummy Eval Hello Karma Quote Seen State \
        System Topic Type GHCi Cmafihe #Haddock

EXCLUDES=ChessModule.hs SearchMailingList.hs SearchMLModule.hs EvalModule/tests.hs FactModule.hs HaddockModule.hs

EXCLUDESFIND=$(addprefix -not -path ./,$(EXCLUDES))

SOURCES = $(shell find . -path'./_darcs' -prune -depth -o $(EXCLUDESFIND) -name \*.hs -o -name \*.lhs) \
          GHCLibraryPath.hs StaticModules.hs

# GHCFLAGS = -fglasgow-exts -cpp -lHSrts -package posix -package network -l
GHCFLAGS = -fglasgow-exts -cpp -package posix -package network -lHSrts -lHSparsec -package parsec #-ihaddock

HTOOLKIT =

all: lambdabot modules

FORCE:

lambdabot: ./Main.o
	ghc $(GHCFLAGS) $(HTOOLKIT) --make Main.hs -o lambdabot

modules: $(addsuffix Module.o,$(DYNMODS))

%.hi: %.o
	touch $@

%.o: %.hs
	ghc $(GHCFLAGS) -c $<

%.o: %.lhs
	ghc $(GHCFLAGS) -c $<

GHCiModule.o: GHCiModule.hs Shell.o
	ghc $(GHCFLAGS) -c $<

GhciModule.o: GhciModule.hs Shell.o
	ghc $(GHCFLAGS) -c $<

Shell.o: shell_stub.o Shell.hs
	ghc $(GHCFLAGS) -fffi -c Shell.hs shell_stub.o 

#FactModule.o: FactModule.hs
#	ghc $(GHCFLAGS) $(HTOOLKIT) -c $<

GHCLibraryPath.hs: GHCLibraryPath.hs.in
	sed -e"s#@GHC_LIB_PATH@#`ghc --print-libdir`#" < $< > $@

genStaticModules: GenStaticModules.hs
	ghc --make GenStaticModules -o genStaticModules

StaticModules.hs: genStaticModules StaticModules.conf
	./genStaticModules

clean:
	rm -f lambdabot genStaticModules
	rm -f   *.o   *.hi
	rm -f */*.o */*.hi
	rm -f *~ */*~
	rm -f GHCLibraryPath.hs StaticModules.hs
	rm depend

depend: $(SOURCES)
	ghc $(GHCFLAGS) $(HTOOLKIT) -M -optdep-f -optdepdepend $(SOURCES) \
	    || rm depend

include depend
