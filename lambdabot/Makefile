# $Id: Makefile,v 1.6 2003/07/31 19:13:15 eleganesh Exp $


DYNMODS=Dict Dummy Eval Hello Karma Quote Seen State \
        System Topic Type GHCi Cmafihe #Haddock

EXCLUDES=ChessModule.hs SearchMailingList.hs SearchMLModule.hs EvalModule/tests.hs FactModule.hs HaddockModule.hs GenStaticModules.hs

EXCLUDESFIND=$(addprefix -not -path ./,$(EXCLUDES))

SOURCES = $(shell find . -path './_darcs' -prune -depth -o $(EXCLUDESFIND) -name \*.hs -o -name \*.lhs)

GHC=	ghc

# GHCFLAGS = -fglasgow-exts -cpp -lHSrts -package posix -package network -l
GHCFLAGS = -fglasgow-exts -cpp -package posix -package network -lHSrts -lHSparsec -package parsec #-ihaddock

HTOOLKIT =

all: lambdabot modules

FORCE:

lambdabot: ./Main.o
	$(GHC) $(GHCFLAGS) $(HTOOLKIT) --make Main.hs -o lambdabot

modules: $(addsuffix Module.o,$(DYNMODS))

%.hi: %.o
	touch $@

%.o: %.hs
	$(GHC) $(GHCFLAGS) -c $<

%.o: %.lhs
	$(GHC) $(GHCFLAGS) -c $<

GHCiModule.o: GHCiModule.hs Shell.o
	$(GHC) $(GHCFLAGS) -c $<

GhciModule.o: GhciModule.hs Shell.o
	$(GHC) $(GHCFLAGS) -c $<

Shell.o: shell_stub.o Shell.hs
	$(GHC) $(GHCFLAGS) -fffi -c Shell.hs shell_stub.o 

#FactModule.o: FactModule.hs
#	$(GHC) $(GHCFLAGS) $(HTOOLKIT) -c $<

GHCLibraryPath.hs: GHCLibraryPath.hs.in
	sed -e"s#@GHC_LIB_PATH@#`$(GHC) --print-libdir`#" < $< > $@

genStaticModules: GenStaticModules.hs
	$(GHC) --make GenStaticModules -o genStaticModules

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
	$(GHC) $(GHCFLAGS) $(HTOOLKIT) -M -optdep-f -optdepdepend $(SOURCES) \
	    || rm depend

include depend
