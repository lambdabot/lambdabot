# $Id: Makefile,v 1.6 2003/07/31 19:13:15 eleganesh Exp $

include config.mk

# 
#  All directories to look for sources
#
ALL_DIRS=	.  DictModule EvalModule QuoteModule BabelBot PlModule

#
# Dynamically loaded modules. Not to be statically linked.
# This var causes the module to be added to the compilation list, to
# actually use them at runtime, you need to edit BotConfig.hs
#
DYNMODS=Dict Dummy Eval Karma Quote Seen State System \
	Topic Type Plugs Babel Version More Pl Help

# Not used, not built, bit dodgy as we are relying on ALL_DIRS to ignore
# the subdirs of EXCLUDED_MODS
EXCLUDED_MODS=	GHCi Chess SearchML Fact Haddock Hello Cmafihe

# The following additional srcs will not be built, along with the above modules.
EXCLUDED_SRCS=	$(addsuffix Module.hs,$(EXCLUDED_MODS))
EXCLUDED_SRCS+= EvalModule/tests.hs PlModule/Test.hs Shell.hs GenStaticModules.hs

#
# Generated at runtime
#
EXTRA_SRCS=

#
# Construct a list of buildable sources, based on $ALL_DIRS, and their
# objects
#
ALL_SRCS=	$(filter-out $(EXCLUDED_SRCS), \
			$(wildcard $(patsubst ./%, %, \
			   $(patsubst %,%/*.hs,   $(ALL_DIRS)) \
			   $(patsubst %,%/*.lhs,  $(ALL_DIRS)) ))) $(EXTRA_SRCS)

#
# Just a list of the bot dependencies. Use ghci to trace the dependency
# tree from Main.hs.
#
ALL_OBJS=	$(addsuffix .$(way_)o,$(basename $(ALL_SRCS)))

# Top-level plugin modules
PLUGIN_OBJS=	$(addsuffix Module.$(way_)o, $(DYNMODS))

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
PROF_OPTS	= -prof -auto-all
LD_OPTS		+= $(PROF_OPTS)
HC_OPTS		+= $(PROF_OPTS)
HC_OPTS 	+= -hisuf $(way_)hi -hcsuf $(way_)hc -osuf $(way_)o
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
	@echo -n "Generating static modules list ... "
	@echo main | ghci -v0 GenStaticModules.hs
	@echo "done."
	@echo -n "Rebuilding dependencies ... "
	@$(GHC) -cpp $(HC_OPTS) $(PKG_OPTS) $(HTOOLKIT) -M -optdep-f \
		-optdepdepend $(ALL_SRCS) || rm depend
	@echo "done."

#
# Link the bot.
# TODO: depends should only be $(BOT_OBJS), not plugins too.
#
lambdabot: $(ALL_OBJS)
	$(GHC) $(LD_OPTS) -v0 --make Main.hs -o $@

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
# clean rules
#
clean:
	rm -f   *.o *.$(way_)o  *.$(way_)hi
	rm -f   */*.$(way_)o  */*.$(way_)hi
	rm -f *~ */*~
	rm -f lambdabot
	rm -f depend depend.bak

distclean: clean
	rm -f config.mk config.h config.log config.status configure 
	rm -rf autom4te.cache
	rm -f StaticModules.hs genStaticModules

-include depend
