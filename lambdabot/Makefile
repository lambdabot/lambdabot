# $Id: Makefile,v 1.6 2003/07/31 19:13:15 eleganesh Exp $

include config.mk

DYNMODS=Dict Dummy Eval Karma Quote Seen State System Topic \
	Type Plugs Babel Version More Pl Help

# Not currently used, but keep building them:
DYNMODS+=Hello Cmafihe

# Not used, not built:
# Shell GHCi Chess SearchML Fact Haddock

# The following will not be built:
EXCLUDES=ChessModule.hs \
	EvalModule/tests.hs \
	FactModule.hs \
	HaddockModule.hs \
	GenStaticModules.hs \
	PlModule/Test.hs

EXCLUDESFIND=	$(addprefix ! -path ./,$(EXCLUDES))

SOURCES=	$(shell find . -path './_darcs' -prune -depth -o $(EXCLUDESFIND) -name \*.hs -o -name \*.lhs)

HTOOLKIT =

all: lambdabot modules

FORCE:

# If $(way) is set then we define $(way_) and $(_way) from it in the
# obvious fashion.
ifeq "$(way)" "p"
  way_ := $(way)_
  _way := _$(way)
endif

#
# building the profiled way
#
ifeq "$(way)" "p"
PROF_OPTS	= -prof -auto-all
LD_OPTS		+= $(PROF_OPTS)
HC_OPTS		+= $(PROF_OPTS)
HC_OPTS 	+= -hisuf $(way_)hi -hcsuf $(way_)hc -osuf $(way_)o
endif

.PHONY: modules
PLUGIN_OBJS= $(addsuffix Module.$(way_)o,$(DYNMODS))
modules: $(PLUGIN_OBJS)

#
# Link the bot (we use --make to extract the static objs form the mess)
#
# This may cause depend to be regenerated, leading to a redundant
# relink. It would be nicer if we could move all plugins somewhere else
#
lambdabot: $(filter-out $(PLUGIN_OBJS), $(SOURCES))
	$(GHC) $(LD_OPTS) -v0 --make Main.hs -o $@

#
# Dependency generation
#
depend: StaticModules.hs $(SOURCES)
	@echo -n "Rebuilding dependencies ... "
	@$(GHC) -cpp $(HC_OPTS) $(PKG_OPTS) $(HTOOLKIT) -M -optdep-f -optdepdepend $(SOURCES) || rm depend
	@echo "done."

StaticModules.hs: GenStaticModules.hs StaticModules.conf
	@echo -n "Generating static modules list ... "
	@echo main | ghci -v0 GenStaticModules.hs
	@echo "done."

# 
# Main rules, with support for 'way' management, from Yi.
#

%.$(way_)hi : %.$(way_)o
	@:

%.$(way_)o: %.hs
	$(GHC) $(HC_OPTS) $(PKG_OPTS) -c $< -o $@ -ohi $(basename $@).$(way_)hi

%.$(way_)o : %.lhs
	$(GHC) $(HC_OPTS) $(PKG_OPTS) -c $< -o $@  -ohi $(basename $@).$(way_)hi

#GhciModule.o: Shell.o GhciModule.hs
#	$(GHC) $(HC_OPTS) $(PKG_OPTS) -c $< -o $@  -ohi $(basename $@).$(way_)hi

#FactModule.o: FactModule.hs
#	$(GHC) $(GHCFLAGS) $(HTOOLKIT) -c $<

#genStaticModules: GenStaticModules.hs
#	$(GHC) -o $@ $<

#StaticModules.hs: genStaticModules StaticModules.conf
#	@./genStaticModules

clean:
	rm -f lambdabot genStaticModules
	rm -f   *.o *.$(way_)o  *.$(way_)hi
	rm -f   */*.$(way_)o  */*.$(way_)hi
	rm -f *~ */*~
	rm -f GHCLibraryPath.hs StaticModules.hs
	rm -f depend depend.bak

distclean: clean
	rm -f config.mk config.h config.log config.status configure 
	rm -rf autom4te.cache

-include depend
