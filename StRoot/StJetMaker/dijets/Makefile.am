# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

sources	= \
	StjDijetCut.cxx \
	StjDijetCutAsymmetricPt.cxx \
	StjDijetCutDPhi.cxx \
	StjDijetCutEta.cxx \
	StjDijetCutEta3MinusEta4.cxx \
	StjDijetCutSameSideRt.cxx \
	StjDijetCutTrgBHT.cxx \
	StjDijetCutTrgBJP.cxx \
	StjDijetList.cxx \
	StjDijetListCut.cxx \
	StjDijetPrint.cxx \
	StjFormDijet.cxx \
	StjDijetListTrigger.cxx \
	StjDijetListTriggerBJP.cxx \
	StjDijetListTriggerBHT.cxx

headers	= \
	StjDijetCut.h \
	StjDijetCutAsymmetricPt.h \
	StjDijetCutDPhi.h \
	StjDijetCutEta.h \
	StjDijetCutEta3MinusEta4.h \
	StjDijetCutSameSideRt.h \
	StjDijetCutTrgBHT.h \
	StjDijetCutTrgBJP.h \
	StjDijetList.h \
	StjDijetListCut.h \
	StjDijetPrint.h \
	StjFormDijet.h \
	StjDijetListTrigger.h \
	StjDijetListTriggerBJP.h \
	StjDijetListTriggerBHT.h


lib_LTLIBRARIES = libStJetMakerDijets.la

libStJetMakerDijets_la_SOURCES = $(sources) $(headers) DijetsLinkDef.h
libStJetMakerDijets_la_CXXFLAGS = -I../tree -I../trigger -I../fourvec -I../jets $(ROOTCFLAGS)
libStJetMakerDijets_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetMakerDijets_la_SOURCES = DijetsDict.cxx
BUILT_SOURCES = DijetsDict.cxx
CLEANFILES = DijetsDict.cxx DijetsDict.h
DijetsDict.cxx: $(headers) DijetsLinkDef.h
	$(ROOTCINT) -f $@ -c -I../fourvec -I../jets $^
