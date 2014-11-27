#
# author  Valeri Fine (fine@bnl.gov)
#-------------------------------------------------------------------------
# Qmake include file to add the rules to create RootCint Dictionary
#-------------------------------------------------------------------------
#
# $Id: rootcint.pri,v 1.4 2013/08/30 16:00:22 perev Exp $
#
# Copyright (C) 2002 by Valeri Fine.  All rights reserved.
#
# This file may be distributed under the terms of the Q Public License
# as defined by Trolltech AS of Norway and appearing in the file
# LICENSE.QPL included in the packaging of this file.
#-------------------------------------------------------------------------
#
# Usage:
# -----
# To link against of the ROOT Qt layer and generate the RootCint dictionary with qmake 
#-------------------------------------------------------------------------------------
# 1. Include this file into your project with QMAKE inlcude statement:
#
#    !exists ($(ROOTSYS)/include/rootcint.pri){
#        message "The Rootcint.pri was not found"
#    }
#    exists ($(ROOTSYS)/include/rootcint.pri){
#       include ($(ROOTSYS)/include/rootcint.pri)
#    }
#
# 2. Provide the list of the class header files followed by the appropriated LinkDef.f file
#    within your prpoject with CREATE_ROOT_DICT_FOR_CLASSES  QMAKE variable
# -----------------------------------------------
#
#   For example
#
#    . . . 
#    !exists ($(ROOTSYS)/include/rootcint.pri){
#        message "The Rootcint.pri was not found"
#    }
#    exists ($(ROOTSYS)/include/rootcint.pri){
#       include ($(ROOTSYS)/include/rootcint.pri)
#       CREATE_ROOT_DICT_FOR_CLASSES  = ${HEADERS} MyParticle.h MyDetector.h MyEvent.h ShowerMain.h 
#       CREATE_ROOT_DICT_FOR_CLASSES *= ${HEADERS} RSLinkDef.h
#    }
#    . . . 
# -----------------------------------------------
#
# 3. Run "qmake"
# 4. Run "make"
#
# -----------------------------------------------

#-- permanent components to be included into any ".pro" file to build the RootCint dictionary

win32 {
   LIBS	+=  \
      -include:_G__cpp_setupG__Hist       -include:_G__cpp_setupG__Graf1  -include:_G__cpp_setupG__G3D     \
      -include:_G__cpp_setupG__GPad       -include:_G__cpp_setupG__Tree   -include:_G__cpp_setupG__Rint    \
      -include:_G__cpp_setupG__PostScript -include:_G__cpp_setupG__Matrix -include:_G__cpp_setupG__Physics \
      -include:_G__cpp_setupG__Gui1       -include:_G__cpp_setupG__Geom1   
    
   exists( $(ROOTSYS)/lib/libTable.lib ) {
      LIBS	+= -include:_G__cpp_setupG__Table
   }   

   exists( $(ROOTSYS)/lib/libQtRootGui.lib ) {
      LIBS	+=  -include:_G__cpp_setupG__QtGUI     
   }   
   
   LIBS	+=  \                                                               
    "$(ROOTSYS)/lib/libCore.lib"   "$(ROOTSYS)/lib/libCint.lib"     "$(ROOTSYS)/lib/libHist.lib"         \
    "$(ROOTSYS)/lib/libGraf.lib"   "$(ROOTSYS)/lib/libGraf3d.lib"   "$(ROOTSYS)/lib/libGpad.lib"         \
    "$(ROOTSYS)/lib/libTree.lib"   "$(ROOTSYS)/lib/libRint.lib"     "$(ROOTSYS)/lib/libPostscript.lib"   \
    "$(ROOTSYS)/lib/libMatrix.lib" "$(ROOTSYS)/lib/libPhysics.lib"  "$(ROOTSYS)/lib/libGui.lib"          \
    "$(ROOTSYS)/lib/libGeom.lib"   "$(ROOTSYS)/lib/libTable.lib"                                         \
    "$(ROOTSYS)/lib/libGQt.lib"   
    
   exists( $(ROOTSYS)/lib/libTable.lib ) {
      LIBS	+=  "$(ROOTSYS)/lib/libTable.lib"
   }   
    
   exists( $(ROOTSYS)/lib/libQtRootGui.so ) {
      LIBS	+=  "$(ROOTSYS)/lib/libQtRootGui.lib"
   }   
   INCLUDEPATH	*= "%ROOTSYS%/include"
}


mac {
    CONFIG +=  no_smart_library_merge
    LIBS	   += $$system(root-config --glibs) 
    
    exists( $(ROOTSYS)/lib/libTable.so ) {
        LIBS	+= -lTable    
    }   

    exists( $(ROOTSYS)/lib/libQtRootGui.so ) {
        LIBS	+=  -u _G__cpp_setupG__QtGUI     
    }   
   
    LIBS	   +=  -lGQt 
    exists( $(ROOTSYS)/lib/libQtRootGui.so ) {
        LIBS	+=  -lQtRootGui   
    }   
   INCLUDEPATH	*= "$(ROOTSYS)/include" 
}

unix {
    LIBS	+= $$system(root-config --glibs) -lGQt 
    
    exists( $(ROOTSYS)/lib/libTable.so ) {
        LIBS	+= -lTable    
    }   
    
    LIBS += -lGQt 
    
    exists( $(ROOTSYS)/lib/libQtRootGui.so ) {
          LIBS	+=  -lQtRootGui  
          message ( "Found Qt extensions library !!!") 
      }   
   INCLUDEPATH	*= "$(ROOTSYS)/include"
}

!isEmpty( CREATE_ROOT_DICT_FOR_CLASSES ) {
  SOURCES	  *= ${QMAKE_TARGET}Dict.cxx 

  rootcint.target       = ${QMAKE_TARGET}Dict.cxx 
  rootcint.commands    += $(ROOTSYS)/bin/rootcint -f $$rootcint.target  -c $(CXXFLAGS) $$CREATE_ROOT_DICT_FOR_CLASSES
  rootcint.depends      = $$CREATE_ROOT_DICT_FOR_CLASSES
  
  rootcintecho.commands = @echo "Generating dictionary $$rootcint.target for $$CREATE_ROOT_DICT_FOR_CLASSES classes"

  QMAKE_EXTRA_UNIX_TARGETS += rootcintecho rootcint 

  QMAKE_CLEAN       +=  ${QMAKE_TARGET}Dict.cxx ${QMAKE_TARGET}Dict.h
}
