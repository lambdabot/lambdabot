# Lambdabot makefile

include config.mk

#
#  All directories to look for sources
#
ALL_DIRS=	.  Plugins \
            Plugins/Dict Plugins/Lambda Plugins/Quote Plugins/Pl Plugins/Vixen

# Not used, not built, bit dodgy as we are relying on ALL_DIRS to ignore
# the subdirs of EXCLUDED_MODS. The following additional srcs will not be built
EXCLUDED_MODS=	Haddock Hello Cmafihe
EXCLUDED_SRCS=	$(addprefix Plugins/, $(addsuffix .hs,$(EXCLUDED_MODS)))
EXCLUDED_SRCS+= Plugins/Lambda/tests.hs Plugins/Pl/Test.hs GenModules.hs

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
.PHONY: modules haddock
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
	@$(GHC) $(HC_OPTS) GenModules.hs -o GenModules

#
# Link the bot.
# TODO: depends should only be $(BOT_OBJS), not plugins too.
#
lambdabot: $(ALL_OBJS)
	@if test "x$(way)" = "xp" ; then \
		for i in *.p_o ; do \
			ln -f -s $$i `echo $$i | sed 's/p_//'` ; \
		done ; \
	fi
	$(GHC) $(HC_OPTS) $(PKG_OPTS) $(LD_OPTS) -v0 Boot.$(way_)o Shared.$(way_)o Map.$(way_)o -o $@
	strip $@

# and for i in $(ALL_OBJS) ; do ln -s $i $i_p.o ...

#
# Boot is the bootstrap loader. It shouldn't be linked *statically* lambdabot
#
Boot.o: Boot.hs 
	$(GHC) $(HC_OPTS) $(PKG_OPTS) -main-is Boot.main -c $< -o $@ -ohi $(basename $@).$(way_)hi

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

#
# Building the haddocks (only if we have haddock)
#
.PHONY: docs

ifneq "$(HADDOCK)" ""

docs: .doc-stamp

# break cyclic imports, and ones with tricky syntax extensions
NO_DOCS=Modules.hs Plugins/Pl/Transform.hs

HS_PPS= $(addsuffix .raw-hs, \
            $(filter-out $(basename $(NO_DOCS)), \
                $(basename $(ALL_SRCS))))

DOCDIR=doc

.doc-stamp: $(HS_PPS)
	@rm -rf $(DOCDIR)
	@-mkdir $(DOCDIR)
	$(HADDOCK) $(HADDOCK_OPTS) -o $(DOCDIR) $(HS_PPS) -k lambdabot
	@touch .doc-stamp
	@rm -f $(HS_PPS)

CLEANS+= $(HS_PPS) $(DOCDIR) .doc-stamp

else
# else no haddock, html is empty.
docs :
endif

#
# clean rules
#
clean:
	rm -f *.o  */*.o  */*/*.o  *.p_o  */*.p_o  */*/*.p_o 
	rm -f *.hi */*.hi */*/*.hi *.p_hi */*.p_hi */*/*.p_hi
	rm -f *~ */*~
	rm -f lambdabot lambdabot.prof depend depend.bak
	rm -rf $(CLEANS)

distclean: clean
	rm -f config.mk config.h config.log config.status configure
	rm -rf autom4te.cache
	rm -f GenModules Modules.hs Modules.o-boot Modules.*-boot

-include depend
