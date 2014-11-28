# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

sources	= \
	StjSpin.cxx \
	StjSpinTree.cxx 

headers	= \
	StjSpin.h \
	StjSpinTree.h

lib_LTLIBRARIES = libStJetMakerSpin.la

libStJetMakerSpin_la_SOURCES = $(sources) $(headers) SpinLinkDef.h
libStJetMakerSpin_la_CXXFLAGS = -I../.. -I../base -I../tree $(ROOTCFLAGS)
libStJetMakerSpin_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetMakerSpin_la_SOURCES = SpinDict.cxx
BUILT_SOURCES = SpinDict.cxx
CLEANFILES = SpinDict.cxx SpinDict.h
SpinDict.cxx: $(headers) SpinLinkDef.h
	$(ROOTCINT) -f $@ -c -I../base $^
