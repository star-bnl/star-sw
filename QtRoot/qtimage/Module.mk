# Module.mk for qtimage module
# Copyright (c) 2004 Valeri Fine Brookhaven National Laboratory
#
# Author: Valeri Fine, 7/02/2004

MODNAME      := qtimage
MODDIR       := $(MODNAME)
MODDIRS      := $(MODDIR)/src
MODDIRI      := $(MODDIR)/inc

QTIMAGEDIR   := $(MODDIR)
QTIMAGEDIRS  := $(QTIMAGEDIR)/src
QTIMAGEDIRI  := $(QTIMAGEDIR)/inc

##### libqtimage #####


QTIMAGEH     := $(filter-out $(QTIMAGEDIRI)/LinkDef%,$(wildcard $(QTIMAGEDIRI)/*.h))
QTIMAGES     := $(filter-out $(QTIMAGEDIRS)/moc_%,$(filter-out $(QTIMAGEDIRS)/G__%,$(wildcard $(QTIMAGEDIRS)/*.cxx)))
QTIMAGEO     := $(QTIMAGES:.cxx=.o)

# --  Qt signals/slots
QTIMAGEMOCS  := $(subst $(QTIMAGEDIRI)/,$(QTIMAGEDIRS)/moc_,\
		   $(patsubst %.h,%.cxx,$(QTIMAGEH)))

QTIMAGEMOCO  := $(QTIMAGEMOCS:.cxx=.o)

# -- ROOT dictionary  
QTIMAGEL     := $(MODDIRI)/LinkDef.h
QTIMAGEDS    := $(MODDIRS)/G__QtImage.cxx
QTIMAGEDO    := $(QTIMAGEDS:.cxx=.o)
QTIMAGEDH    := $(QTIMAGEDS:.cxx=.h)
QTIMAGEMAP   := $(QTIMAGELIB:.$(SOEXT)=.rootmap)

QTIMAGEDEP   := $(QTIMAGEO:.o=.d) 
# -- $(QTIMAGEDO:.o=.d)

QTIMAGELIB   := $(LPATH)/libQtImage.$(SOEXT)

QTIMAGECXXFLAGS := -DQT_DLL -DQT_THREAD_SUPPORT -I. -I$(QTDIR)/mkspecs/default $(QTINCDIR:%=-I%)

# used in the main Makefile
ALLHDRS     += $(patsubst $(MODDIRI)/%.h,include/%.h,$(QTIMAGEH))
ALLMAPS     += $(SQTIMAGEMAP)
ALLLIBS     += $(QTIMAGELIB)

# include all dependency files
INCLUDEFILES += $(QTIMAGEDEP)

##### local rules #####
include/%.h:    $(QTIMAGEDIRI)/%.h
		cp $< $@

$(QTIMAGELIB):  $(QTIMAGEO) $(QTIMAGEDO) $(QTIMAGEMOCO) $(MAINLIBS) \
                $(QTIMAGELIBDEP)
		@$(MAKELIB) $(PLATFORM) $(LD) "$(LDFLAGS)" \
		   "$(SOFLAGS)" libQtImage.$(SOEXT) $@ \
		   "$(QTIMAGEO) $(QTIMAGEMOCO) $(QTIMAGEDO)" \
		   "$(QTIMAGELIBEXTRA) "

$(QTIMAGEDS):   $(QTIMAGEH) $(QTIMAGEL) $(ROOTCINTTMPDEP)
		@echo "Generating dictionary $@..."
		$(ROOTCINTTMP) -f $@ -c $(QTIMAGEH) $(QTIMAGEL)

$(QTIMAGEMAP):     $(RLIBMAP) $(MAKEFILEDEP) $(QTIMAGEL)
		$(RLIBMAP) -o $(QTIMAGEMAP) -l $(QTIMAGELIB) \
		   -d $(QTIMAGELIBDEP) -c $(QTIMAGEL)
		
#(QTIMAGEDO):   $(QTIMAGEDS)
#		$(CXX) $(NOOPT) $(CXXFLAGS) $(QTIMAGECXXFLAGS)  $(CXXOUT)$@ -c $<

all-$(MODNAME):    $(QTIMAGELIB)

clean-$(MODNAME):
		@rm -f $(QTIMAGEO) $(QTIMAGEDO) $(QTIMAGEMOCO) $(QTIMAGEMOC)

clean::         clean-$(MODNAME)

distclean-$(MODNAME): clean-$(MODNAME)
		@rm -f $(QTIMAGEDEP) $(QTIMAGEDS) $(QTIMAGEDH) $(QTIMAGELIB)

distclean::     distclean-$(MODNAME)

show-$(MODNAME):
	@echo QTIMAGEDIRI  $(QTIMAGEDIRI)
	@echo QTIMAGES:    $(QTIMAGES)
	@echo QTIMAGEMOCS: $(QTIMAGEMOCS)
	@echo QTIMAGEO:    $(QTIMAGEO)
	@echo QTIMAGEMOCO: $(QTIMAGEMOCO)

##### extra rules ######
$(sort $(QTIMAGEMOCO) $(QTIMAGEO)):  CXXFLAGS += $(GQTCXXFLAGS)
$(QTIMAGEDO): CXXFLAGS += $(GQTCXXFLAGS)

#$(QTIMAGEMOCO): %.o: %.cxx
#	$(CXX) $(OPT) $(CXXFLAGS) $(QTIMAGECXXFLAGS) $(CXXOUT)$@ -c $<

#moc_%.cpp: %.h
#            $(QTMOCEXE) $< -o $@

$(sort $(QTIMAGEMOCS)) : $(QTIMAGEDIRS)/moc_%.cxx: $(QTIMAGEDIRI)/%.h 
	$(QTMOCEXE)  $(GQTCXXFLAGS) $< -o $@
