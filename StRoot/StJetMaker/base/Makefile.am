# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

sources	= 

headers	= 


lib_LTLIBRARIES = libStJetMakerBase.la

libStJetMakerBase_la_SOURCES = $(sources) $(headers) BaseLinkDef.h
libStJetMakerBase_la_CXXFLAGS = -I../tracks -I../tree -I../trigger $(ROOTCFLAGS)
libStJetMakerBase_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetMakerBase_la_SOURCES = BaseDict.cxx
BUILT_SOURCES = BaseDict.cxx
CLEANFILES = BaseDict.cxx BaseDict.h
BaseDict.cxx: $(headers) BaseLinkDef.h
	$(ROOTCINT) -f $@ -c -I../tracks $^
