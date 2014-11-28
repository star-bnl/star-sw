# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

sources	= \
	StjVertex.cxx \
	StjVertexTree.cxx 

headers	= \
	StjVertex.h \
	StjVertexTree.h

lib_LTLIBRARIES = libStJetMakerVertex.la

libStJetMakerVertex_la_SOURCES = $(sources) $(headers) VertexLinkDef.h
libStJetMakerVertex_la_CXXFLAGS = -I../.. -I../base -I../tree $(ROOTCFLAGS)
libStJetMakerVertex_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetMakerVertex_la_SOURCES = VertexDict.cxx
BUILT_SOURCES = VertexDict.cxx
CLEANFILES = VertexDict.cxx VertexDict.h
VertexDict.cxx: $(headers) VertexLinkDef.h
	$(ROOTCINT) -f $@ -c -I../base $^
