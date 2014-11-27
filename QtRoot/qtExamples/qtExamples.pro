TEMPLATE = subdirs

QMAKE_LFLAGS_RPATH=

LOCALROOTSYSDIR = $$(QTROOTSYSDIR)
LOCALROOTSYS    = $$(ROOTSYS)

QTEXAMPLEPACKAGES   = HelloCanvas           \
                      HelloWord             \
                      HelloClick            \
                      HelloQPainter         \
                      HelloZoomer


QTEXTENSIONEXAMPLES = HelloPixmap           \
                      HelloToolBar          \
                      HelloRootConsole      \
                      HelloGLViewer         \
                      HelloOpenGL           \
                      DrawFunction          \
#                      CustomCanvasMenu      \
                      HelloQtSolutions      \
                      QtGBrowser


win32 {
  exists($${LOCALROOTSYS}/bin/libQtRootGui.dll):      QTEXAMPLEPACKAGES += $$QTEXTENSIONEXAMPLES
  exists($${LOCALROOTSYSDIR}/bin) {
      exists($${LOCALROOTSYSDIR}/bin/libQtRootGui.dll)  QTEXAMPLEPACKAGES *= $$QTEXTENSIONEXAMPLES
  }
} else {
  exists($${LOCALROOTSYSDIR}/lib/libQtRootGui.$${QMAKE_EXTENSION_SHLIB}):       QTEXAMPLEPACKAGES += $${QTEXTENSIONEXAMPLES}
}

!contains( QT_VERSION, "^4.*" ) {
exists($${LOCALROOTSYS}/qtgsi) : QTEXAMPLEPACKAGES += qtgsi/example1
}

exists($$LOCALROOTSYS/include/rootcint.pri):   QTEXAMPLEPACKAGES += HelloCint

QTEXAMPLEPACKAGES += Qt4/CustomWidgets  Qt4/HelloFileBrowser  Qt4/HelloSignal 

message("This project us to build $$QTEXAMPLEPACKAGES Qt/Root examples")
SUBDIRS = $$QTEXAMPLEPACKAGES
