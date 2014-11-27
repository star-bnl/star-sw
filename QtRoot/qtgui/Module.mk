# $Id: Module.mk,v 1.7 2013/08/30 16:00:18 perev Exp $
# Module.mk for qtgui module
# Copyright (c) 2001 Valeri Fine
#
# Author: Valeri Fine, 21/10/2001

.SUFFIXES:       .ui

MODNAME          := qtgui
QTUICEXE         := $(QTMOCEXE:/moc=/uic)

#MODDIR           := $(ROOT_SRCDIR)/$(MODNAME)
MODDIR           := $(MODNAME)
MODDIRS          := $(MODDIR)/src
MODDIRI          := $(MODDIR)/inc

ROOTQTGUIDIR     := $(MODDIR)
ROOTQTGUIDIRS    := $(ROOTQTGUIDIR)/src
ROOTQTGUIDIRI    := $(ROOTQTGUIDIR)/inc

QT4              :=  $(findstring QtCore, $(QTINCDIR))

##### libQtGui #####
QTGUIL          := $(MODDIRI)/LinkDef.h
QTGUIDS         := $(call stripsrc,$(MODDIRS)/G__QtGUI.cxx)
QTGUIDO         := $(call stripsrc,$(QTGUIDS:.cxx=.o))
QTGUIDH         := $(QTGUIDS:.cxx=.h)

# QTH1         := $(MODDIRI)/TGWin32.h $(MODDIRI)/TQtGuiFactory.h
QTGUIH1        := TQtGuiFactory.h        TBrowserCustom.h   \
                  TQtPatternSelect.h     TQtTabValidator.h  \
                  TEmbeddedPad.h         TQtColorSelect.h   \
                  TQtZoomPadWidget.h     TQGsiRootCanvas.h  \
			         TQtPad2Html.h          TQtCanvas2Html.h   \
                  TQtMarkerSelect.h      TQtPixmapBox.h   
						
QTGUIH1        := $(patsubst %,$(MODDIRI)/%,$(QTGUIH1))
QTGUIH         := $(filter-out $(MODDIRI)/LinkDef%,$(wildcard $(MODDIRI)/*.h))

QTGUIMOCH     := TQtBrowserImp.h          TQtCanvasImp.h         \
                 TQtCanvasWidget.h        TQtColorSelect.h       \
                 TQtColorSelectButton.h   TQtContextMenuImp.h    \
                 TQtControlBarImp.h       TQtIconBrowserImp.h    \
                 TQtCustomizeCanvasMenu.h TQtFloatSpinBox.h      \
                 TQtInspectImp.h          TQtRootBrowserImp.h    \
                 TQtObjectDialog.h        TQtPatternSelect.h     \
                 TQtPatternSelectButton.h TQtRootBrowserAction.h \
                 TQtZoomPadWidget.h       TQtToolBar.h           \
                 TQtMarkerSelect.h        TQtPixmapBox.h         \
                 TQtMarkerSelectButton.h  TQtStyleComboBox.h     \
                 TQtFloatSlider.h         TQtRootCommandCombo.h  \
                 qtcolorpicker.h          TQtColorPickerHelper.h \
                 TQtTextEditor.h                                 \
                 qtmmlwidget.h            TQtCommandPlugin.h
					  
                 
QTGUIMOCH        := $(patsubst %,$(MODDIRI)/%,$(QTGUIMOCH))
                 
#                 $(ROOTQTGUIDIRI)/TQGsiRootCanvas.h   

QTGUISOLUTIONH  := $(ROOTQTGUIDIRI)/QtMmlWidget $(ROOTQTGUIDIRI)/QtColorPicker  $(ROOTQTGUIDIRI)/QtMmlDocument
QTGUIMOC        :=  $(call stripsrc,$(subst $(MODDIRI)/,$(MODDIRS)/moc_,$(patsubst %.h,%.cxx,$(QTGUIMOCH))))
QTGUIMOCO       := $(QTGUIMOC:.cxx=.o)

QTGUIS          := $(filter-out $(MODDIRS)/moc_%,$(filter-out $(MODDIRS)/G__%,$(wildcard $(MODDIRS)/*.cxx)))
QTGUIO          := $(QTGUIS:.cxx=.o)

QTGUIDEP        := $(QTGUIO:.o=.d) $(QTGUIDO:.o=.d)  $(QTGUIMOCO:.o=.d)

QTGUICXXFLAGS   := -DQT3_SUPPORT -DQT_DLL -DQT_THREAD_SUPPORT -I. 
ifeq ($(ARCH),win32)
QTGUICXXFLAGS   += -I$(QTDIR)/mkspecs/default
# -- win32-msvc2005
else
QTGUICXXFLAGS   += -I$(QTDIR)/mkspecs/default 
endif

QTGUICXXFLAGS   += $(QTINCDIR:%=-I%)
QTGUILIB        := $(LPATH)/libQtRootGui.$(SOEXT)
QTGUIMAP        := $(QTGUILIB:.$(SOEXT)=.rootmap)

# used in the main Makefile
ALLHDRS     += $(patsubst $(MODDIRI)/%.h,include/%.h,$(QTGUIH))
# ALLHDRS     += $(patsubst $(ROOTQTGUIDIRI)/Qt%, include/%, $(QTGUISOLUTIONH))
ALLMAPS     += $(QTGUIMAP)
ALLLIBS     += $(QTGUILIB)

# include all dependency files
INCLUDEFILES += $(QTGUIDEP)

##### local rules #####
include/%.h:    $(ROOTQTGUIDIRI)/%.h
		cp $< $@
		      
#include/Qt%.:    $(ROOTQTGUIDIRI)/Qt%.
#		cp $< $@
$(QTGUIO):  $(ROOTQTGUIDIRS)/ui_TQtRootCommand.h

$(QTGUILIB):    $(QTGUIO) $(QTGUIDO) $(QTGUIMOCO)  $(ORDER_) $(MAINLIBS) $(QTGUILIBDEP)  $(QTGUISOLUTIONH) 
		@$(MAKELIB) $(PLATFORM) $(LD) "$(LDFLAGS)" \
		   "$(SOFLAGS)" libQtRootGui.$(SOEXT) $@   \
		   "$(QTGUIO) $(QTGUIMOCO) $(QTGUIDO)"     \
		   " $(QTLIBDIR) $(QTGUILIBEXTRA) $(QTLIB) " 

$(QTGUIDS):     $(QTGUIH1) $(QTGUIL) $(ROOTCINTTMPDEP)
		$(MAKEDIR)
		@echo "Generating dictionary $@..."
		$(ROOTCINTTMP) -f $@ -c  $(GQTCXXFLAGS) $(QTGUIH1) $(QTGUIL)

$(QTGUIMAP):     $(RLIBMAP) $(MAKEFILEDEP) $(QTGUIL)
		$(RLIBMAP) -o $@ -l $(QTGUILIB) \
		   -d $(QTGUILIBDEPM) -c $(QTGUIL)

all-$(MODNAME):      $(QTGUILIB) $(QTGUIMAP)

clean-$(MODNAME):
		rm -f $(QTGUIO) $(QTGUIDO) $(QTGUIMOC) $(QTGUIMOCO) $(ROOTQTGUIDIRS)/ui_TQtRootCommand.h

clean::         clean-$(MODNAME)

show-$(MODNAME):
		@echo "QTGUIS     $(QTGUIS)"
		@echo "QTGUIO     $(QTGUIO)"
		@echo "QTGUIDO    $(QTGUIDO)" 
		@echo "QTGUIMOC   $(QTGUIMOC)" 
		@echo "QTGUIMOCO  $(QTGUIMOCO)"
		@echo "QTGUIDEP   $(QTGUIDEP)"
		@echo .
		@echo "ROOTCINTTMPDEP  $(ROOTCINTTMPDEP)"
		@echo "ROOTCINTTMP  $(ROOTCINTTMP)"
		@echo "QTGUILIBEXTRA  $(QTGUILIBEXTRA)"


distclean-$(MODNAME): clean-$(MODNAME) 
		@rm -f $(QTGUIDEP) $(QTGUIDS) $(QTGUIDH) $(QTGUILIB)

distclean::     distclean-$(MODNAME)

##### extra rules ######
$(sort $(QTGUIMOCO) $(QTGUIO)):  CXXFLAGS += $(GQTCXXFLAGS)
$(QTGUIDO):	 CXXFLAGS += $(GQTCXXFLAGS)
ifeq ($(GCC_MAJOR),4)
ifeq ($(subst $(GCC_MINOR),,0 1),0 1)
# GCC >= 4.2
$(GQTO): CXXFLAGS += -Wno-strict-overflow
endif
endif

$(ROOTQTGUIDIRS)/TQtCommandPlugin.o :  $(ROOTQTGUIDIRS)/ui_TQtRootCommand.h

$(ROOTQTGUIDIRS)/ui_TQtRootCommand.h:  $(ROOTQTGUIDIRS)/TQtRootCommand.ui

$(QTGUIMOC) :  $(call stripsrc,$(ROOTQTGUIDIRS)/moc_%.cxx): $(ROOTQTGUIDIRI)/%.h
	$(MAKEDIR)
ifeq (,$(QT4))
	$(QTMOCEXE)  $< -o $@
else
	$(QTMOCEXE)  $(QTGUICXXFLAGS) $< -o $@
endif

$(ROOTQTGUIDIRS)/ui_%.h: $(ROOTQTGUIDIRS)/%.ui
		$(QTUICEXE)  $< -o $@
