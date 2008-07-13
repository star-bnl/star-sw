# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

TESTS = TestApp

check_PROGRAMS = $(TESTS) 
TestApp_SOURCES = TestMain.cpp
TestApp_CXXFLAGS = $(ROOTCFLAGS) $(CPPUNIT_CFLAGS) 
TestApp_LDFLAGS = -L$(ROOTLIBDIR)

TestApp_LDADD = \
	$(ROOTGLIBS) $(CPPUNIT_LIBS) $(ROOTAUXLIBS) -lTable -lGeom

lib_LTLIBRARIES = libStJetMakerTest.la
libStJetMakerTest_la_SOURCES = 	\
	TreeContentTest.cpp TreeContentTest.hh \
	StJetMakerTest.cpp StJetMakerTest.hh \
	StppAnaParsTest.cpp StppAnaParsTest.hh \
	StBET4pMakerImpBuilderTest.cpp StBET4pMakerImpBuilderTest.hh \
	StBET4pMakerImpTest.cpp StBET4pMakerImpTest.hh \
	StBET4pMakerTest.cpp StBET4pMakerTest.hh \
	LoopTest.cpp LoopTest.hh \
	StJetBEMCEnergyCutTest.cpp StJetBEMCEnergyCutTest.hh \
	BemcEnergySumCalculatorBuilderTest.cpp BemcEnergySumCalculatorBuilderTest.hh

libStJetMakerTest_la_CXXFLAGS = \
	-I../StRoot \
	-I../StRoot/StJetFinder \
	-I../StRoot/StJetMaker \
	-I../StRoot/StJetMaker/StFourPMakers \
	-I../StRoot/StSpinPool \
	-I../StRoot/StTriggerUtilities \
	-I$(STAR)/.$(STAR_HOST_SYS)/include \
	-I$(STAR)/StRoot \
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
