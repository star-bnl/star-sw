# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

sources	= \
	StjDijetListWriter.cxx \
	StjJetListWriter.cxx \
	StjJetListReader.cxx \
	StjMCKinWriter.cxx \
	StjMCParticleListReader.cxx \
	StjMCParticleListWriter.cxx \
	StjTowerEnergyListReader.cxx \
	StjTowerEnergyListWriter.cxx \
	StjTrackListReader.cxx \
	StjTrackListWriter.cxx \
	StjTreeEntryCoordinator.cxx \
	StjTreeIndex.cxx \
	StjTreeIndexList.cxx \
	StjTreeIndexListCreator.cxx \
	StjTreeReader.cxx \
	StjTreeReaderTwoTrees.cxx \
	StjTrgHTWriter.cxx \
	StjTrgJPWriter.cxx \
	StjTrgMBWriter.cxx \
	StjTrgReader.cxx \
	StjTrgWriter.cxx \
	StjVertexReader.cxx \
	StjVertexWriter.cxx

headers	= \
	StjDijetListWriter.h \
	StjJetListWriter.h \
	StjJetListReader.h \
	StjMCKinWriter.h \
	StjMCParticleListReader.h \
	StjMCParticleListWriter.h \
	StjTowerEnergyListReader.h \
	StjTowerEnergyListWriter.h \
	StjTrackListReader.h \
	StjTrackListWriter.h \
	StjTreeEntryCoordinator.h \
	StjTreeIndex.h \
	StjTreeIndexList.h \
	StjTreeIndexListCreator.h \
	StjTreeReader.h \
	StjTreeReaderTwoTrees.h \
	StjTrgHTWriter.h \
	StjTrgJPWriter.h \
	StjTrgMBWriter.h \
	StjTrgReader.h \
	StjTrgWriter.h \
	StjVertexReader.h \
	StjVertexWriter.h


lib_LTLIBRARIES = libStJetMakerTree.la

libStJetMakerTree_la_SOURCES = $(sources) $(headers) TreeLinkDef.h
libStJetMakerTree_la_CXXFLAGS = -I../.. -I../tracks -I../towers -I../mcparticles -I../fourvec -I../mckin -I../trigger -I../vertex -I../jets -I../dijets $(ROOTCFLAGS)
libStJetMakerTree_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetMakerTree_la_SOURCES = TreeDict.cxx
BUILT_SOURCES = TreeDict.cxx
CLEANFILES = TreeDict.cxx TreeDict.h
TreeDict.cxx: $(headers) TreeLinkDef.h
	$(ROOTCINT)  -f $@ -c -I../tracks -I../towers -I../mcparticles -I../fourvec -I../trigger -I../jets -I../dijets $^
