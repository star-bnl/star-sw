TEMPLATE	= app

CONFIG	+= qt warn_on  thread debug

HEADERS += qtrootexample1.h
FORMS += ex1.ui
SOURCES += main.cpp qtrootexample1.cxx

includeFile = $(QTROOTSYSDIR)/include/rootcint.pri
exists ($$includeFile) {
  include ($$includeFile)
} 
!exists ($$includeFile) {
  includeFile = $(ROOTSYS)/include/rootcint.pri
  exists ($$includeFile) {
    include ($$includeFile)
  } 
  !exists ($$includeFile) {
    message (" ")
    message ("WARNING:  The $$inlcudeFile was not found !!!")
    message ("Please update your Qt layer version from http://root.bnl.gov ")
    message (" ")
    LIBS += $$system(root-config --glibs) -lGQt
    INCLUDEPATH += $(ROOTSYS)/include
  }
}


unix {
#
#  generate the link to the proper version of the ROOT resource file
#
  rootrc.target   = .rootrc
  ROOTRESOURCEFILE=rootrcqtgui
  !exists ($(ROOTSYS)/lib/libQtRootGui.$$QMAKE_EXTENSION_SHLIB) {
      message ("No ROOT Qt Extension was found. Use Qt-layer instead")
      ROOTRESOURCEFILE = rootrcqt
  }
  rootrc.commands = @rm -rf .rootrc; ln -s $$ROOTRESOURCEFILE $$rootrc.target 
    
  QMAKE_EXTRA_UNIX_TARGETS += rootrc 
  PRE_TARGETDEPS  += $$rootrc.target 
  QMAKE_CLEAN     += $$rootrc.target
}

# mac:QMAKE_INFO_PLIST=Info.plist
unix {
  UI_DIR = .ui
  MOC_DIR = .moc
  OBJECTS_DIR = .obj
}

