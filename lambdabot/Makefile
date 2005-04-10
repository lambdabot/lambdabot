# Lambdabot makefile

include config.mk

# 
#  All directories to look for sources
#
ALL_DIRS=	.  DictModule EvalModule QuoteModule BabelBot PlModule

# Not used, not built, bit dodgy as we are relying on ALL_DIRS to ignore
# the subdirs of EXCLUDED_MODS. The following additional srcs will not be built
EXCLUDED_MODS=	Fact Haddock Hello Cmafihe
EXCLUDED_SRCS=	$(addsuffix Module.hs,$(EXCLUDED_MODS))
EXCLUDED_SRCS+= EvalModule/tests.hs PlModule/Test.hs GenModules.hs

#
# Generated at build time
#
EXTRA_SRCS=Modules.hs

#
# Construct a list of buildable sources, based on $ALL_DIRS, and their objects
#
ALL_SRCS=$(filter-out $(EXCLUDED_SRCS) $(EXTRA_SRCS), \
		$(wildcard $(patsubst ./%, %, \
		   $(patsubst %,%/*.hs,   $(ALL_DIRS)) \
		   $(patsubst %,%/*.lhs,  $(ALL_DIRS)) ))) $(EXTRA_SRCS)

#
# Just a list of the bot dependencies. Use ghci to trace the dependency
# tree from Main.hs.
#
ALL_OBJS=	$(addsuffix .$(way_)o,$(basename $(ALL_SRCS)))

#
# With this, 'make lambdabot' should only build the static code, and 'make
# modules' only makes the plugins. However, we need to cache this somehow.
#
#BOT_SRCS=$(patsubst %, %.hs, \
#		$(shell echo ":q" | \
#		          ghci -v1 Main.hs $(PKG_OPTS) 2> /dev/null | \
#			    sed -n '/Ok/{ s/.*://; s/,//g; s/.$$//; p; q;}' ))
#
#BOT_OBJS=	$(addsuffix .$(way_)o,$(basename $(BOT_SRCS)))

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
PROF_OPTS	= -prof -auto-all -DSTATIC=1
LD_OPTS		+= $(PROF_OPTS)
HC_OPTS		+= $(PROF_OPTS)
HC_OPTS 	+= -hisuf $(way_)hi -hcsuf $(way_)hc -osuf $(way_)o

# and statically link all the modules.
endif

#
# Now, get down to business
#
all: lambdabot modules

#
# TODO should be just PLUGIN_OBJS
#
.PHONY: modules
modules: $(ALL_OBJS)

#
# Dependency generation
#
depend: $(ALL_SRCS)
	@echo -n "Rebuilding dependencies ... "
	@$(GHC) -cpp $(HC_OPTS) $(PKG_OPTS) $(HTOOLKIT) -M -optdep-f \
		-optdepdepend $(ALL_SRCS) || rm depend
	@echo "done."

#
# Slight magic. Note how we're passing values defined in config.mk
# as commmand line args to GenModules.lhs
#
# Modules is imported recursively, so we break the loop with a .hs-boot
# file (.hi-boot with <604). That code is below:
#

Modules.hs: config.mk GenModules
	@echo -n "Generating module list ... "
	@echo $(MODULE_HI_BOOT) > Modules.$(RECURSIVE_MODULE_SUFFIX)
	@./GenModules $(PLUGINS) "," $(STATICS)
	@echo "done."

GenModules: GenModules.hs
	@$(GHC) GenModules.hs -o GenModules


#
# If you ever add a module, you can use this target to regenerate the
# Depends.hs file. Remember there are different depends for the ghc versions
#
.PHONY: dynamic-depends
dynamic-depends:
	@for i in *Module.hi ; do \
		echo -n `echo "$$i"` ;\
		$(GHC) --show-iface $$i | sed 's/\[boot\]//g;/orphans/{s/.*/#/;q;}' ;\
	done | ./GenModules --depends

#
# Link the bot.
# TODO: depends should only be $(BOT_OBJS), not plugins too.
#
lambdabot: $(ALL_OBJS) ofiles
	$(GHC) $(HC_OPTS) $(PKG_OPTS) $(LD_OPTS) -v0 --make Main.hs -o $@

#
# Profiling requires ln -s *.p_o to *.o so as not to confuse the dyn linker
#
.PHONY: ofiles
ofiles:	$(ALL_OBJS)
	@if test "x$(way)" = "xp" ; then \
		for i in *.p_o ; do \
			ln -f -s $$i `echo $$i | sed 's/p_//'` ; \
		done ; \
	fi

# and for i in $(ALL_OBJS) ; do ln -s $i $i_p.o ...

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

#
# clean rules
#
clean:
	rm -f *.o *.p_o  *.hi *.p_hi */*.o */*.p_o */*.hi */*.p_hi
	rm -f *~ */*~
	rm -f lambdabot lambdabot.prof depend depend.bak

distclean: clean
	rm -f config.mk config.h config.log config.status configure 
	rm -rf autom4te.cache
	rm -f GenModules Modules.hs Modules.o-boot Modules.*-boot

-include depend
