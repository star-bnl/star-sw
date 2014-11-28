# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

sources	= \
	StjMC.cxx \
	StjMCNull.cxx \
	StjMCParticleCut.cxx \
	StjMCParticleCutEta.cxx \
	StjMCParticleCutEtaForStatus.cxx \
	StjMCParticleCutFirstMotherId.cxx \
	StjMCParticleCutStatus.cxx \
	StjMCParticleList.cxx \
	StjMCParticleListCut.cxx \
	StjMCParticleListToFourVecList.cxx \
	StjMCParticlePrint.cxx \
	StjMCParticleToFourVec.cxx \
	StjMCTree.cxx

headers	= \
	StjMC.h \
	StjMCNull.h \
	StjMCParticleCut.h \
	StjMCParticleCutEta.h \
	StjMCParticleCutEtaForStatus.h \
	StjMCParticleCutFirstMotherId.h \
	StjMCParticleCutStatus.h \
	StjMCParticleList.h \
	StjMCParticleListCut.h \
	StjMCParticleListToFourVecList.h \
	StjMCParticlePrint.h \
	StjMCParticleToFourVec.h \
	StjMCTree.h

lib_LTLIBRARIES = libStJetMakerMcparticles.la

libStJetMakerMcparticles_la_SOURCES = $(sources) $(headers) McparticlesLinkDef.h
libStJetMakerMcparticles_la_CXXFLAGS = -I../fourvec -I../tree -I../trigger $(ROOTCFLAGS)
libStJetMakerMcparticles_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetMakerMcparticles_la_SOURCES = McparticlesDict.cxx
BUILT_SOURCES = McparticlesDict.cxx
CLEANFILES = McparticlesDict.cxx McparticlesDict.h
McparticlesDict.cxx: $(headers) McparticlesLinkDef.h
	$(ROOTCINT) -f $@ -c -I../fourvec $^
