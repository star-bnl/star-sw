# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

TESTS = TestApp

check_PROGRAMS = $(TESTS) 
TestApp_SOURCES = TestMain.C
TestApp_CXXFLAGS = $(ROOTCFLAGS) $(CPPUNIT_CFLAGS) 
TestApp_LDFLAGS = -L$(ROOTLIBDIR)

TestApp_LDADD = \
	$(ROOTGLIBS) $(CPPUNIT_LIBS) $(ROOTAUXLIBS) -lTable -lGeom

lib_LTLIBRARIES = libStJetMakerTest.la
libStJetMakerTest_la_SOURCES = 	\
	TreeContentTest.C TreeContentTest.hh \
	StJetMakerTest.C StJetMakerTest.hh \
	StppAnaParsTest.C StppAnaParsTest.hh \
	StBET4pMakerImpBuilderTest.C StBET4pMakerImpBuilderTest.hh \
	StBET4pMakerImpTest.C StBET4pMakerImpTest.hh \
	StBET4pMakerTest.C StBET4pMakerTest.hh \
	LoopTest.C LoopTest.hh \
	StjTrackListCutTest.C StjTrackListCutTest.hh \
	StjTowerEnergyListCutTest.C StjTowerEnergyListCutTest.hh \
	StjeBemcEnergySumCalculatorBuilderTest.C StjeBemcEnergySumCalculatorBuilderTest.hh \
	StjTPCTreeTest.C StjTPCTreeTest.hh \
	StjBEMCTreeTest.C StjBEMCTreeTest.hh \
	StjTreeEntryCoordinatorTest.C StjTreeEntryCoordinatorTest.hh \
	StjTreeEntryMakerTest.C StjTreeEntryMakerTest.hh \
	StjTowerEnergyListToStMuTrackFourVecListTest.C StjTowerEnergyListToStMuTrackFourVecListTest.hh \
	StjTrackListToStMuTrackFourVecListTest.C StjTrackListToStMuTrackFourVecListTest.hh \
	RunJetFinderTest.C RunJetFinderTest.hh \
	StjTowerEnergyListVariationTest.C StjTowerEnergyListVariationTest.hh \
	StjTrackTowerEnergyListToFourVecListTest.C StjTrackTowerEnergyListToFourVecListTest.hh \
	StjFourVecListCutTest.C StjFourVecListCutTest.hh  \
	StjJetListCutTest.C StjJetListCutTest.hh \
	StPythiaFourPMakerTest.C StPythiaFourPMakerTest.hh

libStJetMakerTest_la_CXXFLAGS = \
	-I../.. \
	-I../../StJetFinder \
	-I.. \
	-I../StFourPMakers \
	-I../emulator \
	-I../base \
	-I../misc \
	-I../mudst \
	-I../tree \
	-I../../StSpinPool \
	-I../../StTriggerUtilities \
	-I$(STAR)/.$(STAR_HOST_SYS)/include \
	-I$(STAR)/StRoot \
	-I$(STAR)/StRoot/StJetFinder \
	-I$(STAR)/StRoot/St_base \
	-I$(STAR)/StRoot/StarClassLibrary \
	-I$(STAR)/StRoot/StEvent \
	-I$(STAR)/StRoot/StChain \
	-I$(STAR)/StRoot/Star2Root \
	-I$(STAR)/StRoot/StHbtMaker/doc/Make \
	-Iinclude_sp \
	-I$(STAR)/StRoot/StarRoot \
	-I$(STAR)/StRoot \
	-I$(STAR)/StRoot/StTriggerUtilities \
	$(ROOTCFLAGS)
libStJetMakerTest_la_LDFLAGS = -version-info 1:0:0
