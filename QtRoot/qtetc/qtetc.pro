# TARGET = etc

QTROOTSYSPATHINSTALL = $$(QTROOTSYSDIR)

isEmpty(DESTDIR) {
  DESTDIR=..
}
isEmpty(QTROOTSYSPATHINSTALL) {
  QTROOTSYSPATHINSTALL = $$DESTDIR
}
etcfiles.path  = $$QTROOTSYSPATHINSTALL/etc
etcfiles.files = rootrcqt  rootrcqtgui

INSTALLS = etcfiles
