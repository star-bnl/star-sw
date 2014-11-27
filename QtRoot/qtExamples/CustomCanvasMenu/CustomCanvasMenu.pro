TEMPLATE	= app
LANGUAGE	= C++

QMAKE_LFLAGS_RPATH=
!exists ($$(ROOTSYS)/lib/libQtRootGui.$$QMAKE_EXTENSION_SHLIB) {
      message ("No ROOT Qt Extension was found. Use Qt-layer instead")
}
CONFIG	+= qt warn_on  thread debug 
QT +=  qt3support
SOURCES	+= main.cpp qtrootexample1.cxx
HEADERS	+= qtrootexample1.h


FORMS	= qtrootexample1.ui

IMAGES	= images/h1_t.png \
	images/h2_t.png \
	images/BnlLogo.png \
	images/OpenGLView.png \
	images/BnlLogoSmall.png

includeDir = $$(QTROOTSYSDIR)/include
incFile = $$includeDir/rootcint.pri
exists ($$includeDir) { # Win32 wants us to check the directory existence separately
   exists ($$incFile) {
     include ($$incFile)
   } 
}
!exists ($$includeDir) {
  incFile = $$(ROOTSYS)/include/rootcint.pri
  exists ($$incFile) {
    include ($$incFile)
  } 
  !exists ($$incFile) {
    message (" ")
    message ("WARNING:  The $$inlcudeFile was not found !!!")
    message ("Please update your Qt layer version from http://root.bnl.gov ")
    message (" ")
    LIBS += $$system(root-config --glibs) -lGQt
    INCLUDEPATH += $(ROOTSYS)/include
  }
}

FORMS	= qtrootexample1.ui

#unix {
#
#  generate the link to the proper version of the ROOT resource file
#
#  rootrc.target   = .rootrc
#  ROOTRESOURCEFILE=rootrcqtgui
#  !exists ($(ROOTSYS)/lib/libQtRootGui.$$QMAKE_EXTENSION_SHLIB) {
#      message ("No ROOT Qt Extension was found. Use Qt-layer instead")
#      ROOTRESOURCEFILE = rootrcqt
#  }
#  rootrc.commands = @rm -rf .rootrc; ln -s $$ROOTRESOURCEFILE $$rootrc.target 
    
#  QMAKE_EXTRA_UNIX_TARGETS += rootrc 
#  PRE_TARGETDEPS  += $$rootrc.target 
#  QMAKE_CLEAN     += $$rootrc.target
#}

# mac:QMAKE_INFO_PLIST=Info.plist
unix {
  UI_DIR = .ui
  MOC_DIR = .moc
  OBJECTS_DIR = .obj
}

