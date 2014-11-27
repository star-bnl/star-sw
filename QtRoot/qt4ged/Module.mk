# Module.mk for qtged module
# Copyright (c) 2004 Valeri Fine
#
# Author: Valeri Fine, 01/06/2004

MODDIR    := qt4ged
MODDIRS   := $(MODDIR)/src
MODDIRI   := $(MODDIR)/inc

QTGEDDIR    := $(MODDIR)
QTGEDDIRS   := $(QTGEDDIR)/src
QTGEDDIRI   := $(QTGEDDIR)/inc

##### libGed #####
QTGEDL      := $(MODDIRI)/LinkDef.h
QTGEDDS     := $(MODDIRS)/G__QtGed.cxx
QTGEDDO     := $(QTGEDDS:.cxx=.o)
QTGEDDH     := $(QTGEDDS:.cxx=.h)

QTGEDH      := $(filter-out $(MODDIRI)/LinkDef%,$(wildcard $(MODDIRI)/*.h))
QTGEDS      := $(filter-out $(MODDIRS)/moc_%, $(filter-out $(MODDIRS)/G__%,$(wildcard $(MODDIRS)/*.cxx)))
QTGEDO      := $(QTGEDS:.cxx=.o)

QTGEDDEP    := $(QTGEDO:.o=.d) $(QTGEDDO:.o=.d)

QTGEDLIB    := $(LPATH)/libQtGed.$(SOEXT)

# --  Qt signals/slots
QTGEDMOCS  := $(subst $(QTGEDDIRI)/,$(QTGEDDIRS)/moc_,\
		   $(patsubst %.h,%.cxx,$(QTGEDH)))

QTGEDMOCO  := $(QTGEDMOCS:.cxx=.o)

QTGEDCXXFLAGS := -DQT_GUI_LIB -DQT_CORE_LIB -DQT_DLL -DQT_THREAD_SUPPORT -I. -I$(QTDIR)/mkspecs/default  $(QTINCDIR:%=-I%)

# used in the main Makefile
ALLHDRS     += $(patsubst $(MODDIRI)/%.h,include/%.h,$(QTGEDH))
ALLLIBS     += $(QTGEDLIB)

# include all dependency files
INCLUDEFILES += $(QTGEDDEP)

##### local rules #####
include/%.h:    $(QTGEDDIRI)/%.h
		cp $< $@

$(QTGEDLIB):      $(QTGEDO) $(QTGEDDO) $(QTGEDMOCO) $(MAINLIBS) $(QTGEDLIBDEP)
		@$(MAKELIB) $(PLATFORM) $(LD) "$(LDFLAGS)" \
		   "$(SOFLAGS)" libQtGed.$(SOEXT) $@ "$(QTGEDO) $(QTGEDDO) $(QTGEDMOCO)"  \
		   "$(QTGEDLIBEXTRA)"

$(QTGEDDS):       $(QTGEDH) $(QTGEDL) $(ROOTCINTTMPEXE)
		@echo "Generating dictionary $@..."
		$(ROOTCINTTMP) -f $@ -c $(QTGEDH) $(QTGEDL)

$(QTGEDDO):       $(QTGEDDS)
		$(CXX) $(NOOPT) $(CXXFLAGS) $(QTGEDCXXFLAGS) -I. $(CXXOUT)$@ -c $<

all-qtged:        $(QTGEDLIB)

map-qtged:        $(RLIBMAP)
		$(RLIBMAP) -r $(ROOTMAP) -l $(QTGEDLIB) \
		   -d $(QTGEDLIBDEP) -c $(QTGEDL)

map::           map-qtged

clean-qtged:
		@rm -f $(QTGEDO) $(QTGEDDO) $(QTGEDMOCO)

clean::         clean-qtged

distclean-qtged: clean-qtged
		@rm -f $(QTGEDDEP) $(QTGEDDS) $(QTGEDMOCS) $(QTGEDDH) $(QTGEDLIB)

distclean::     distclean-qtged

$(QTGEDO): CXXFLAGS +=  $(GQTCXXFLAGS)

$(QTGEDMOCO): %.o: %.cxx
	$(CXX) $(OPT) $(CXXFLAGS) $(QTGEDCXXFLAGS)  $(CXXOUT)$@ -c $<

$(sort $(QTGEDMOCS)) : $(QTGEDDIRS)/moc_%.cxx: $(QTGEDDIRI)/%.h 
	$(QTMOCEXE) $< -o $@

