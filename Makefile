# Lambdabot makefile

ifneq "$(dynamic)" "yes"
static=yes
endif

MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

include config.mk

#
#  All directories to look for sources
#
ALL_DIRS=	.  Lib Plugin \
               Plugin/Dict  Plugin/Lambda \
               Plugin/Quote Plugin/Pl \
               Plugin/Vixen Plugin/Dummy

# Not used, not built, bit dodgy as we are relying on ALL_DIRS to ignore
# the subdirs of EXCLUDED_MODS. The following additional srcs will not be built
EXCLUDED_MODS=	Hello
EXCLUDED_SRCS=	Setup.hs $(addprefix Plugin/, $(addsuffix .hs,$(EXCLUDED_MODS)))
EXCLUDED_SRCS+= Plugin/Lambda/tests.hs Plugin/Pl/Test.hs

#
# Generated at build time
#
EXTRA_SRCS=Lib/Regex.hs

ifeq "$(static)" "yes"
EXCLUDED_SRCS+=Boot.hs Plugin/Dynamic.hs
endif

#
# Construct a list of buildable sources, based on $ALL_DIRS, and their objects
#
ALL_SRCS=$(filter-out $(EXCLUDED_SRCS) $(EXTRA_SRCS), \
		$(wildcard $(patsubst ./%, %, \
		   $(patsubst %,%/*.hs,   $(ALL_DIRS)) \
		   $(patsubst %,%/*.lhs,  $(ALL_DIRS)) ))) $(EXTRA_SRCS)

#
# way management
#
# If $(way) is set then we define $(way_) and $(_way) from it in the
# obvious fashion.
ifeq "$(way)" "p"
  way_ := $(way)_
  _way := _$(way)
endif

# building the profiled way
ifeq "$(way)" "p"
PROF_OPTS	= -prof -auto-all
LD_OPTS		+= $(PROF_OPTS)
HC_OPTS		+= $(PROF_OPTS)
HC_OPTS 	+= -hisuf $(way_)hi -hcsuf $(way_)hc -osuf $(way_)o
# and statically link all the modules.
endif

#
# Just a list of the bot dependencies. Use ghci to trace the dependency
# tree from Main.hs.
#
ALL_OBJS=	$(addsuffix .$(way_)o,$(basename $(ALL_SRCS)))

#
# Now, get down to business
#

ifneq "$(way)" "ghci"

all: BotPP lambdabot modules runplugs djinn unlambda hoogle

else

HC_OPTS += -DGHCi
all: BotPP Lib/Regex.o runplugs djinn unlambda hoogle ghci

endif

ghci:
	ghci $(HC_OPTS) Main.hs

BotPP: scripts/BotPP.hs
	$(GHC) -package fps -o $@ $<

#
# TODO should be just PLUGIN_OBJS
#
.PHONY: modules
modules: BotPP $(ALL_OBJS)

#
# Dependency generation
# Need to remove -prof -auto-all from the ghc flags:
#
depend: BotPP $(ALL_SRCS)
	@echo -n "Rebuilding dependencies ... "
	$(GHC) -cpp $(HC_OPTS) $(PKG_OPTS) -M -optdep-f -optdepdepend $(ALL_SRCS) || rm depend
	@echo "done."

#
# Link the bot.
# TODO: depends should only be $(BOT_OBJS), not plugins too.
#
lambdabot: BotPP $(ALL_OBJS)
	@if test "x$(way)" = "xp" ; then \
		for i in *.p_o ; do \
			ln -f -s $$i `echo $$i | sed 's/p_//'` ; \
		done ; \
	fi
ifeq "$(static)" "yes"
	$(GHC) $(PKG_OPTS) $(LD_OPTS) -v0 $(ALL_OBJS) -o $@
else
	$(GHC) $(PKG_OPTS) $(LD_OPTS) -v0 -main-is Boot.main Boot.$(way_)o Shared.$(way_)o -o $@
endif
	strip $@

# and for i in $(ALL_OBJS) ; do ln -s $i $i_p.o ...

#
# Boot is the bootstrap loader. It shouldn't be linked *statically* lambdabot
#
Boot.o: Boot.hs 
	$(GHC) $(HC_OPTS) $(PKG_OPTS) -package plugins -main-is Boot.main -c $< -o $@ -ohi $(basename $@).$(way_)hi

#
# Main rules, with support for 'way' management, from Yi.
#
%.$(way_)hi : %.$(way_)o
	@:

%.$(way_)o: %.hs
	$(GHC) $(HC_OPTS) $(PKG_OPTS) -c $< -o $@ -ohi $(basename $@).$(way_)hi

%.$(way_)o : %.lhs
	$(GHC) $(HC_OPTS) $(PKG_OPTS) -c $< -o $@  -ohi $(basename $@).$(way_)hi

ifneq "$(GLASGOW_HASKELL)" "602"
%.$(way_)o-boot : %.hs-boot
	$(GHC) $(HC_OPTS) -c $< -o $@ -ohi $(basename $@).$(way_)hi-boot

%.$(way_)hi-boot : %.$(way_)o-boot
	@:
endif

%.raw-hs : %.hs
	@$(GHC) $(HC_OPTS) $(PKG_OPTS) -D__HADDOCK__ -E -optP-P $< -o $@

%.raw-hs : %.lhs
	@$(GHC) $(HC_OPTS) $(PKG_OPTS) -D__HADDOCK__ -E -optP-P $< -o $@

%_hsc.c %_hsc.h %.hs : %.hsc
	hsc2hs $<
	@touch $(patsubst %.hsc,%_hsc.c,$<)

#
# clean rules
#
clean:
	rm -f *.o  */*.o  */*/*.o  *.p_o  */*.p_o  */*/*.p_o 
	rm -f *.hi */*.hi */*/*.hi *.p_hi */*.p_hi */*/*.p_hi
	rm -f *~ */*~
	rm -f lambdabot lambdabot.prof depend depend.bak
	rm -rf $(CLEANS)
	cd scripts/Djinn && $(MAKE) clean
	cd scripts/hoogle && $(MAKE) clean

distclean: clean
	rm -f config.mk config.h config.log config.status
	rm -rf autom4te.cache

runplugs: scripts/RunPlugs.hs scripts/ShowQ.hs
	$(GHC) -O -c scripts/ShowQ.hs
	mv scripts/ShowQ.o scripts/ShowQ.hi .	
	$(GHC) -O -package posix -package plugins -o $@ $<

djinn: scripts/Djinn/Djinn.hs
	( cd scripts/Djinn && $(GHC) -O --make -o $@ Djinn.hs && mv $@ ../.. )

unlambda: scripts/Unlambda.hs
	$(GHC) -O -package posix -o $@ $<

hoogle:
	( cd scripts/hoogle && $(MAKE) && mv hoogle ../../ && cp hoogle.txt ../.. )

CLEANS+= runplugs djinn unlambda hoogle hoogle.txt BotPP
CLEANS+= Lib/Regex_hsc.c Lib/Regex.hs

-include depend
