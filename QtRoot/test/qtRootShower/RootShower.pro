TEMPLATE	= lib

unix:  TARGET   = RootShower
win32: TARGET   = libRootShower

QMAKE_RPATH=

RESOURCES = RootShower.qrc
CONFIG		+= qt warn_on  thread dll
# CONFIG		+= qt warn_on dll
HEADERS		= constants.h GButtonFrame.h GShowerPad.h GTitleFrame.h MyDetector.h MyEvent.h\
                  MyParticle.h ParticlesDef.h RootShower.h RSAbout.h RSHelpText.h \
                  RSLinkDef.h RSMsgBox.h RSVersion.h SettingsDlg.h
                  
SOURCES		= GButtonFrame.cxx  GTitleFrame.cxx  MyEvent.cxx     RootShower.cxx     \
              RSAbout.cxx       RSMsgBox.cxx     ShowerMain.cxx  GShowerPad.cxx     \
              MyDetector.cxx    MyParticle.cxx   RSHelptext.cxx  SettingsDlg.cxx

CREATE_ROOT_DICT_FOR_CLASSES = MyParticle.h MyDetector.h MyEvent.h ShowerMain.h RSLinkDef.h

inlcudeFile = $(ROOTSYS)/include/rootcint.pri
exists ($$inlcudeFile) {
  include ($$inlcudeFile)
}
 
!exists ($$inlcudeFile) {
 
    message ("WARNING:  The $$inlcudeFile was not found !!!")
    error  ("Please update your Qt layer version from http://root.bnl.gov ")

}


!win32 {
  LIBS += -lEG
}
win32:LIBS      +=  $(ROOTSYS)/lib/libRIO.lib $(ROOTSYS)/lib/libEG.lib  $(ROOTSYS)/lib/libHtml.lib   $(ROOTSYS)/lib/libMathCore.lib

unix {
# -- trick to force the the trivial symbolic link under UNIX

#  rootshowerso.target       = RootShower.so
#  rootshowerso.commands     = ln -s lib$$rootshowerso.target $$rootshowerso.target
  
#  QMAKE_EXTRA_UNIX_TARGETS += rootshowerso 
#  PRE_TARGETDEPS           += $$rootshowerso.target
#  QMAKE_CLEAN              += $$rootshowerso.target
  
  
#
# -- generate the link to the proper version of the ROOT resource file
#
  rootrc.target   = .rootrc
  rootrc.commands = @rm -rf .rootrc; ln -s rootrcqt $$rootrc.target 
  
  QMAKE_EXTRA_UNIX_TARGETS += rootrc 
  PRE_TARGETDEPS  += $$rootrc.target 
  QMAKE_CLEAN     += $$rootrc.target
  
#  -- working directory 
  
  ROOTCINT_DIR = .rootcint
  UI_DIR = .ui
  MOC_DIR = .moc
  OBJECTS_DIR = .obj
}
#The following line was inserted by qt3to4
QT +=  qt3support 
