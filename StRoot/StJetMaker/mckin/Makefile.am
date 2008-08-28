# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

sources	= \
	StjMCKin.cxx

headers	= \
	StjMCKin.h

lib_LTLIBRARIES = libStJetMakerMckin.la

libStJetMakerMckin_la_SOURCES = $(sources) $(headers) MckinLinkDef.h
libStJetMakerMckin_la_CXXFLAGS = $(ROOTCFLAGS)
libStJetMakerMckin_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetMakerMckin_la_SOURCES = MckinDict.cxx
BUILT_SOURCES = MckinDict.cxx
CLEANFILES = MckinDict.cxx MckinDict.h
MckinDict.cxx: $(headers) MckinLinkDef.h
	$(ROOTCINT) -f $@ -c $^
