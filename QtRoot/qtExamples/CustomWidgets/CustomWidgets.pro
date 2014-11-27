QT += qt3support

TEMPLATE	= app
LANGUAGE	= C++

QMAKE_RPATH=

CONFIG	+= qt warn_on release

SOURCES	+= CustomWidgetsMain.cpp

FORMS	= CustomWidgets.ui


includeFile = $(QTROOTSYSDIR)/include
exists ($$includeFile) {
  include ($$includeFile/rootcint.pri)
}

!exists ($$includeFile) {
  includeFile = $(ROOTSYS)/include/rootcint.pri
  exists ($$includeFile) {
    include ($$includeFile)
  }
}

UI_DIR = .ui
MOC_DIR = .moc
OBJECTS_DIR = .obj


