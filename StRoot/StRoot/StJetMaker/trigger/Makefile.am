# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

sources	= \
	StjTrg.cxx \
	StjTrgBEMCJetPatchTowerIdMap.cxx \
	StjTrgBEMCJetPatchTowerIdMap2005.cxx \
	StjTrgBEMCJetPatchTowerIdMap2006.cxx \
	StjTrgDisableTower.cxx \
	StjTrgDisableTowerHT.cxx \
	StjTrgDisableTowerJP.cxx \
	StjTrgPassCondition.cxx \
	StjTrgRaiseThreshold.cxx \
	StjTrgRaiseThresholdEtHT.cxx \
	StjTrgRaiseThresholdEtJP.cxx \
	StjTrgSoft.cxx \
	StjTrgSoftEtThresholdBHT.cxx \
	StjTrgSoftEtThresholdBJP.cxx \
	StjTrgSoftFactory.cxx \
	StjTrgSoftGetAdcEt.cxx \
	StjTrgSoftPass.cxx \
	StjTrgTree.cxx


headers	= \
	StjTrg.h \
	StjTrgBEMCJetPatchTowerIdMap.h \
	StjTrgBEMCJetPatchTowerIdMap2005.h \
	StjTrgBEMCJetPatchTowerIdMap2006.h \
	StjTrgDisableTower.h \
	StjTrgDisableTowerHT.h \
	StjTrgDisableTowerJP.h \
	StjTrgPassCondition.h \
	StjTrgRaiseThreshold.h \
	StjTrgRaiseThresholdEtHT.h \
	StjTrgRaiseThresholdEtJP.h \
	StjTrgSoft.h \
	StjTrgSoftEtThresholdBHT.h \
	StjTrgSoftEtThresholdBJP.h \
	StjTrgSoftFactory.h \
	StjTrgSoftGetAdcEt.h \
	StjTrgSoftPass.h \
	StjTrgTree.h

lib_LTLIBRARIES = libStJetMakerTrigger.la

libStJetMakerTrigger_la_SOURCES = $(sources) $(headers) TriggerLinkDef.h
libStJetMakerTrigger_la_CXXFLAGS = -I../.. -I../towers -I../tree $(ROOTCFLAGS)
libStJetMakerTrigger_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetMakerTrigger_la_SOURCES = TriggerDict.cxx
BUILT_SOURCES = TriggerDict.cxx
CLEANFILES = TriggerDict.cxx TriggerDict.h
TriggerDict.cxx: $(headers) TriggerLinkDef.h
	$(ROOTCINT) -f $@ -c -I../towers $^
