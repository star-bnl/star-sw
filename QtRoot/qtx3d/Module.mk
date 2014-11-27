# Module.mk for qtx3d module
# Copyright (c) 2002 Valeri Fine
#
# Author: Valeri Fine, 29/2/2000

QTINCDIR      := $(QTDIR)/include
QTMOCEXE      := $(QTDIR)/bin/moc

MODDIR       := qtx3d
MODDIRS      := $(MODDIR)/src
MODDIRI      := $(MODDIR)/inc
 
QTX3DDIR     := $(MODDIR)
QTX3DDIRS    := $(QTX3DDIR)/src
QTX3DDIRI    := $(QTX3DDIR)/inc

##### libQtX3d #####

QTX3DL         := $(QTX3DDIRI)/LinkDef.h
QTX3DDS        := $(QTX3DDIRS)/G__QTX3D.cxx
QTX3DDO        := $(QTX3DDS:.cxx=.o)
QTX3DDH        := $(QTX3DDS:.cxx=.h)

QTX3DH         := $(QTX3DDIRI)/TQtViewerX3D.h $(QTX3DDIRI)/TQtX3DWidget.h

QTX3DMOC       := $(subst $(QTX3DDIRI)/,$(QTX3DDIRS)/moc_,\
		   $(patsubst %.h,%.cxx,$(QTX3DH)))
QTX3DMOCO        := $(QTX3DMOC:.cxx=.o)

QTX3DS         := $(filter-out $(QTX3DDIRS)/moc_%,\
                  $(filter-out $(QTX3DDIRS)/G__%,$(wildcard $(QTX3DDIRS)/*.cxx)))

QTX3DS2        := $(wildcard $(QTX3DDIRS)/*.c)

QTX3DCXXO      := $(QTX3DS:.cxx=.o) 

QTX3DO         := $(QTX3DCXXO)  $(QTX3DS2:.c=.o)

QTX3DDEP       := $(QTX3DO:.o=.d) $(QTX3DDO:.o=.d)

QTX3DLIB       := $(LPATH)/libQtX3d.$(SOEXT)
# QTX3DLIBEXTRA  := $(LPATH)/libX3d.$(SOEXT)

QTX3DCXXFLAGS    := -DQT_DLL -DQT_THREAD_SUPPORT -I. -I$(QTINCDIR)


# used in the main Makefile
ALLHDRS     += $(patsubst $(QTX3DDIRI)/%.h,include/%.h,$(QTX3DH))
ALLLIBS     += $(QTX3DLIB)

# include all dependency files
INCLUDEFILES += $(QTX3DDEP)

##### local rules #####
include/%.h:    $(QTX3DDIRI)/%.h
		cp $< $@

$(QTX3DLIB):      $(QTX3DO) $(QTX3DDO) $(QTX3DMOCO) $(MAINLIBS) $(QTX3DLIBDEP)
		@$(MAKELIB) $(PLATFORM) $(LD) "$(LDFLAGS)" \
		   "$(SOFLAGS)" libX3d.$(SOEXT) $@ "$(QTX3DO) $(QTX3DDO)  $(QTX3DMOCO)" \
		   "$(QTX3DLIBEXTRA)"

$(QTX3DDS):       $(QTX3DH) $(QTX3DL) $(ROOTCINTTMP)
		@echo "Generating dictionary $@..."
		$(ROOTCINTTMP) -f $@ -c $(QTX3DH) $(QTX3DL)

$(QTX3DDO):       $(QTX3DDS)
		$(CXX) $(NOOPT) $(CXXFLAGS) -I. $(QTX3DCXXFLAGS) -o $@ -c $<

all-qtx3d:        $(QTX3DLIB)

clean-qtx3d:
		@rm -f $(QTX3DO) $(QTX3DDO)

clean::         clean-qtx3d

distclean-qtx3d:  clean-qtx3d
		@rm -f $(QTX3DDEP) $(QTX3DDS) $(QTX3DDH) $(QTX3DLIB) $(QTX3DMOC)

distclean::     distclean-qtx3d

##### extra rules ######
$(sort $(QTX3DMOCO) $(QTX3DCXXO)): %.o: %.cxx
	$(CXX) $(QTOPT) $(CXXFLAGS) $(QTX3DCXXFLAGS) -o $@ -c $<

$(QTX3DMOC) : $(QTX3DDIRS)/moc_%.cxx: $(QTX3DDIRI)/%.h
	$(QTMOCEXE) $< -o $@
