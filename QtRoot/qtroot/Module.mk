# Module.mk for qtroot module
# Copyright (c) 2003 Valeri Fine
#
# Author: Valeri Fine, 20/5/2003

MODNAME      := qtroot
MODDIR       := gui/$(MODNAME)
MODDIRS      := $(MODDIR)/src
MODDIRI      := $(MODDIR)/inc

QTROOTDIR    := $(MODDIR)
QTROOTDIRS   := $(QTROOTDIR)/src
QTROOTDIRI   := $(QTROOTDIR)/inc

##### libQtRoot #####
QTROOTL      := $(QTROOTDIRI)/LinkDef.h
QTROOTDS     := $(QTROOTDIRS)/G__QtRoot.cxx
QTROOTDO     := $(QTROOTDS:.cxx=.o)
QTROOTDH     := $(QTROOTDS:.cxx=.h)
QTROOTRH     := $(MODDIRI)/TQtRootGuiFactory.h 

QTROOTH      := $(filter-out $(MODDIRI)/LinkDef%,$(wildcard $(MODDIRI)/*.h))
QTROOTS      := $(filter-out $(MODDIRS)/G__% (MODDIRS)/moc_%,$(wildcard $(MODDIRS)/*.cxx))
QTROOTO      := $(QTROOTS:.cxx=.o)

QTROOTDEP    := $(QTROOTO:.o=.d) $(QTROOTDO:.o=.d)

QTROOTMOCH   := $(QTROOTDIRI)/TQtContextMenuImp.h  $(QTROOTDIRI)/TQtObjectDialog.h 
QTROOTMOC    := $(subst $(MODDIRI)/,$(MODDIRS)/moc_,$(patsubst %.h,%.cxx,$(QTROOTMOCH)))
QTROOTMOCO   := $(QTROOTMOC:.cxx=.o)


QTROOTLIB    := $(LPATH)/libQtRoot.$(SOEXT)
QTROOTMAP    := $(QTROOTLIB:.$(SOEXT)=.rootmap)

# used in the main Makefile
ALLHDRS     += $(patsubst $(MODDIRI)/%.h,include/%.h,$(QTROOTH))
ALLLIBS     += $(QTROOTLIB)
ALLMAPS     += $(QTROOTMAP)

QTROOTCXXFLAGS   := -DQT3_SUPPORT -DQT_DLL -DQT_THREAD_SUPPORT -I. 
ifeq ($(ARCH),win32)
QTROOTCXXFLAGS   += -I$(QTDIR)/mkspecs/default 
#   win32-msvc2005
else
QTROOTCXXFLAGS   += -I$(QTDIR)/mkspecs/default 
endif

QTROOTCXXFLAGS   += $(QTINCDIR:%=-I%)

# include all dependency files
INCLUDEFILES += $(QTROOTDEP)

##### local rules #####
.PHONY:         all-$(MODNAME) clean-$(MODNAME) distclean-$(MODNAME)

include/%.h:    $(QTROOTDIRI)/%.h
		cp $< $@

$(QTROOTLIB):   $(QTROOTO) $(QTROOTDO) $(QTROOTMOCO) $(ORDER_) $(MAINLIBS) $(QTROOTLIBDEP)
		@$(MAKELIB) $(PLATFORM) $(LD) "$(LDFLAGS)" \
		   "$(SOFLAGS)" libQtRoot.$(SOEXT) $@ "$(QTROOTO) $(QTROOTMOCO) $(QTROOTDO)" \
		   "$(QTROOTLIBEXTRA) $(QTLIBDIR) $(QTLIB)"

$(QTROOTDS):    $(QTROOTRH) $(QTROOTL) $(ROOTCINTTMPDEP)
		@echo "Generating dictionary $@..."
		$(ROOTCINTTMP) -f $@ -c $(QTROOTRH) $(QTROOTL)

$(QTROOTMAP):   $(RLIBMAP) $(MAKEFILEDEP) $(QTROOTL)
		$(RLIBMAP) -o $(QTROOTMAP) -l $(QTROOTLIB) \
		   -d $(QTROOTLIBDEPM) -c $(QTROOTL)

all-$(MODNAME): $(QTROOTLIB) $(QTROOTMAP)

clean-$(MODNAME):
		@rm -f $(QTROOTO) $(QTROOTDO) $(QTROOTMOCO)

clean::         clean-$(MODNAME)

distclean-$(MODNAME): clean-$(MODNAME)
		@rm -f $(QTROOTDEP) $(QTROOTDS) $(QTROOTDH) $(QTROOTLIB) $(QTROOTMAP) $(QTROOTMOC)

distclean::     distclean-$(MODNAME)

##### extra rules ######
$(sort $(QTROOTMOCO) $(QTROOTO)): CXXFLAGS += $(QTROOTCXXFLAGS)
$(QTROOTDO): CXXFLAGS += $(QTROOTCXXFLAGS)

$(QTROOTMOC) : $(QTROOTDIRS)/moc_%.cxx: $(QTROOTDIRI)/%.h
	$(QTMOCEXE) $(QTROOTCXXFLAGS) $< -o $@
