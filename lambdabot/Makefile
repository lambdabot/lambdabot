# $Id: Makefile,v 1.6 2003/07/31 19:13:15 eleganesh Exp $

include config.mk

DYNMODS=Dict Dummy Eval Hello Karma Quote Seen State \
        System Topic Type Cmafihe Plugs #Haddock GHCi

EXCLUDES=ChessModule.hs SearchMailingList.hs SearchMLModule.hs \
	 EvalModule/tests.hs FactModule.hs HaddockModule.hs GenStaticModules.hs

EXCLUDESFIND=	$(addprefix ! -path ./,$(EXCLUDES))

SOURCES=	$(shell find . -path './_darcs' -prune -depth -o $(EXCLUDESFIND) -name \*.hs -o -name \*.lhs)

HTOOLKIT =

all: lambdabot modules

FORCE:

#
# Link the bot (we use --make to extract the static objs form the mess)
#
lambdabot: $(SOURCES) ./Main.o 
	$(GHC) $(LD_OPTS) -v0 --make Main.hs -o $@

#
# Dependency generation
#
depend: StaticModules.hs GHCLibraryPath.hs $(SOURCES)
	@echo -n "Rebuilding dependencies ..."
	@$(GHC) -cpp $(HC_OPTS) $(PKG_OPTS) $(HTOOLKIT) -M -optdep-f -optdepdepend $(SOURCES) || rm depend
	@echo "done."

modules: $(addsuffix Module.o,$(DYNMODS))

# 
# Main rules, with support for 'way' management, from Yi.
#

%.$(way_)hi : %.$(way_)o
	@:

%.$(way_)o: %.hs
	$(GHC) $(HC_OPTS) $(PKG_OPTS) -c $< -o $@ -ohi $(basename $@).$(way_)hi

%.$(way_)o : %.lhs
	$(GHC) $(HC_OPTS) $(PKG_OPTS) -c $< -o $@  -ohi $(basename $@).$(way_)hi

#
# Dynamic modules need special treatment
#

#GhciModule.o: Shell.o GhciModule.hs
#	$(GHC) $(HC_OPTS) $(PKG_OPTS) -c $< -o $@  -ohi $(basename $@).$(way_)hi

#FactModule.o: FactModule.hs
#	$(GHC) $(GHCFLAGS) $(HTOOLKIT) -c $<


GHCLibraryPath.hs: GHCLibraryPath.hs.in
	@sed -e"s,@GHC_LIB_PATH@,`$(GHC) --print-libdir`," < $< > $@

genStaticModules: GenStaticModules.hs
	$(GHC) -o $@ $<

StaticModules.hs: genStaticModules StaticModules.conf
	@./genStaticModules

clean:
	rm -f lambdabot genStaticModules
	rm -f   *.o   *.hi
	rm -f */*.o */*.hi
	rm -f *~ */*~
	rm -f GHCLibraryPath.hs StaticModules.hs
	rm depend

distclean: clean
	rm config.mk config.log config.status configure 
	rm -rf autom4te.cache

include depend
