# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

sources	= \
	StjRunJetFinder.cxx

headers = \
	StjEtaToDetectorEta.h \
	StjFourVecForJetFinder.h \
	StjRunJetFinder.h

lib_LTLIBRARIES = libStJetMakerMisc.la

libStJetMakerMisc_la_SOURCES = $(sources) $(headers) MiscLinkDef.h
libStJetMakerMisc_la_CXXFLAGS = -I../.. -I../fourvec -I../jets $(ROOTCFLAGS)
libStJetMakerMisc_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetMakerMisc_la_SOURCES = MiscDict.cxx
BUILT_SOURCES = MiscDict.cxx
CLEANFILES = MiscDict.cxx MiscDict.h
MiscDict.cxx: $(headers) MiscLinkDef.h
	$(ROOTCINT) -f $@ -c -I../.. -I../fourvec -I../jets $^
