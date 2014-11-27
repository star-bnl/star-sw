TEMPLATE	= lib
CONFIG		+= qt warn_on release dll

QMAKE_RPATH=


!exists ($(ROOTSYS)/include/rootcint.pri){
     message "The Rootcint.pri was not found"
}
exists ($(ROOTSYS)/include/rootcint.pri){
     include ($(ROOTSYS)/include/rootcint.pri)
}

SOURCES	= xform.cpp

unix:  TARGET	= xform
win32: TARGET	= libxform

unix {
# -- trick to force the the trivial symbolic link under UNIX
#
#  xformso.target       = xform.so
#  xformso.commands     = ln -s libxform.so $$xformso.target
  
#  PRE_TARGETDEPS        = $$xformso.target 
  
#  QMAKE_EXTRA_UNIX_TARGETS += xformso 
 
#  QMAKE_CLEAN       +=  $$xformso.target
  
#
#  generate the link to the proper version of the ROOT resource file
#
#  rootrc.target   = .rootrc
#  rootrc.commands = @rm -rf .rootrc; ln -s rootrcqt $$rootrc.target 
  
#  QMAKE_EXTRA_UNIX_TARGETS += rootrc 
#  PRE_TARGETDEPS  += $$rootrc.target 
#  QMAKE_CLEAN     += $$rootrc.target
  
}
#  -- working directory 
ROOTCINT_DIR = .rootcint
UI_DIR = .ui
MOC_DIR = .moc
OBJECTS_DIR = .obj

