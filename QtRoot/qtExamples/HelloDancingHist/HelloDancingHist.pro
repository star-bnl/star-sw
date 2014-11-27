TEMPLATE = app
TARGET   = HelloDancingHist
QT += qt3support

QMAKE_LFLAGS_RPATH=


HelloDancingHistIncludeFile = $$(QTROOTSYSDIR)/include
exists($$HelloDancingHistIncludeFile) {
  include ($$HelloDancingHistIncludeFile/rootcint.pri)
  embeddedPadFile = $$HelloDancingHistIncludeFile/TEmbeddedPad.h

  exists ($$embeddedPadFile) {
     DEFINES += EMBEDDEDTPAD
  }
}

!exists ($$HelloDancingHistIncludeFile) {
  HelloDancingHistIncludeFile = $$(ROOTSYS)/include/rootcint.pri
  exists ($$HelloDancingHistIncludeFile) {
    include ($$HelloDancingHistIncludeFile)
    embeddedPadFile = $(ROOTSYS)/include/TEmbeddedPad.h
    exists ($$embeddedPadFile) {
      DEFINES += EMBEDDEDTPAD
    }
  }
}

win32: LIBS += libMathCore.lib
unix: LIBS += MathCore

HEADERS		= HelloDancingHist.h \
		 
SOURCES		= HelloDancingHist.cxx \
		  main.cxx
