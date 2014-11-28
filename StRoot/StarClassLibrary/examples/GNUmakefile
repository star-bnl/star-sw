############################################################################
#
# $Id: GNUmakefile,v 1.8 2006/08/15 21:42:23 jeromel Exp $
#
# Author: Thomas Ullrich, May 1999
# --------------------------------------------------------------------------
#
# GNU makefile to compile the SCL example programs
#
# --------------------------------------------------------------------------
#
# $Log: GNUmakefile,v $
# Revision 1.8  2006/08/15 21:42:23  jeromel
# Fix rhic -> rhic.bnl.gov
#
# Revision 1.7  2000/04/06 22:26:36  ullrich
# Added new particle classes to particleTest.
#
# Revision 1.6  2000/03/16 16:30:58  ullrich
# Added new example randomTest4.
#
# Revision 1.5  1999/12/21 16:30:17  ullrich
# Added test program for StFastCircleFitter.
#
# Revision 1.4  1999/12/21 15:14:39  ullrich
# Modified to cope with new compiler version on Sun (CC5.0).
#
# Revision 1.3  1999/12/07 23:40:28  ullrich
# Modified options for Linux
#
# Revision 1.2  1999/06/04 18:04:12  ullrich
# Added dependencies for memInfoTest
#
# Revision 1.1  1999/05/19 22:23:47  ullrich
# Initial Revision
#
############################################################################
#
#   Common settings
#
CPPFLAGS      := -I.. -DST_NO_NAMESPACES
CXXFLAGS      := -g
LDFLAGS       := 

#
#   Architecture dependent variables...
#

# -------- hp_ux102 ---------

ifeq ($(STAR_SYS),hp_ux102)
	CXX          := aCC 
	CXXFLAGS     += +d 
        LD           := aCC
        LDFLAGS      += -Wl,+vnocompatwarnings -L$(CERN)/pro/lib -lpacklib
endif

# -------- Linux ---------

ifeq ($(findstring i386, $(STAR_SYS)),i386)
	CXX          := g++ 
	CPPFLAGS     += -DGNU_GCC -DST_NO_NUMERIC_LIMITS
	CXXFLAGS     += 
	LD           := g++ 
	LDFLAGS      += $(CERN_ROOT)/lib/libpacklib.a -lg2c -lnsl -lcrypt -lm -ldl -rdynamic
endif

# -------- sun4x_54, sun4x_55, sun4x_56 ---------
#

ifeq ($(findstring sun4x, $(STAR_SYS)),sun4x)
	ifdef USE_CC5
		CXX 	     := /opt/WS5.0/bin/CC
		LD	     := $(CXX)
		CPPFLAGS     := -I.. -DST_NO_MEMBER_TEMPLATES
		CXXFLAGS     += -library=iostream,no%Cstd -Qoption ld -t
		LDFLAGS      += -L$(CERN)/pro/lib -lpacklib
		LDFLAGS      += -L/opt/WS5.0/SC5.0/lib -lCstd -lF77 -lM77 -lsocket -lnsl -lsunmath -lmalloc
        else
		CXX 	     := CC
		STDHOME      := /afs/rhic.bnl.gov/star/packages/ObjectSpace/2.0m
		CPPFLAGS     += -I$(STDHOME) -I$(STDHOME)/ospace/std 
		CPPFLAGS     += -DST_NO_MEMBER_TEMPLATES -DST_NO_NUMERIC_LIMITS
		CPPFLAGS     += -DST_NO_EXCEPTIONS -DST_NO_TEMPLATE_DEF_ARGS
		CXXFLAGS     += -Qoption ld -t
		LD	     := $(CXX)
		LDFLAGS      += -L$(CERN)/pro/lib -lpacklib
		LDFLAGS      += -L$(STDHOME)/lib -lospace -lF77 -lM77 -lsocket -lnsl -lsunmath -lmalloc
	endif
endif

#
#   Rules and dependencies
#

.PRECIOUS: %.o

%:	%.cc
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< -o $@ $(LDFLAGS)

%.o:	../%.cc
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) $<
	
memInfoTest:	memInfoTest.cc StMemoryInfo.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< \
	StMemoryInfo.o  \
	-o memInfoTest $(LDFLAGS)

circleFitTest:	circleFitTest.cc StFastCircleFitter.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< \
	StFastCircleFitter.o  \
	-o circleFitTest $(LDFLAGS)

getConfigTest:	getConfigTest.cc 
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< \
	-o getConfigTest $(LDFLAGS)

helixTest%:	helixTest%.cc StHelix.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< \
	StHelix.o  \
	-o $@ $(LDFLAGS)

physicalHelixTest:  physicalHelixTest.cc StHelix.o StPhysicalHelix.o 
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< \
	StHelix.o StPhysicalHelix.o \
	-o physicalHelixTest $(LDFLAGS)

timerTest:	timerTest.cc StTimer.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< \
	StTimer.o \
	-o timerTest $(LDFLAGS)

hbookTest%:	hbookTest%.cc StHbook.o RandEngine.o RandGauss.o Random.o \
		JamesRandom.o RandomEngine.o 
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< \
	StHbook.o RandEngine.o \
	RandGauss.o Random.o \
	JamesRandom.o RandomEngine.o \
	-o $@ $(LDFLAGS)

vecTest3:  vecTest3.cc RandEngine.o RandFlat.o Random.o JamesRandom.o RandomEngine.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< \
	RandEngine.o RandFlat.o Random.o \
	JamesRandom.o RandomEngine.o \
	-o vecTest3 $(LDFLAGS)

randomTest4:  	randomTest4.cc DRand48Engine.o RandExponential.o Random.o \
		JamesRandom.o RandFlat.o RandomEngine.o RandBreitWigner.o RandGauss.o \
		RandEngine.o  RandPoisson.o RanluxEngine.o RanecuEngine.o StRandom.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< \
        DRand48Engine.o RandExponential.o Random.o \
	JamesRandom.o RandFlat.o RandomEngine.o \
	RandBreitWigner.o RandGauss.o RandEngine.o \
	RandPoisson.o RanluxEngine.o RanecuEngine.o StRandom.o \
	-o $@ $(LDFLAGS)

randomTest%:  	randomTest%.cc DRand48Engine.o RandExponential.o Random.o \
		JamesRandom.o RandFlat.o RandomEngine.o RandBreitWigner.o RandGauss.o \
		RandEngine.o  RandPoisson.o RanluxEngine.o RanecuEngine.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< \
        DRand48Engine.o RandExponential.o Random.o \
	JamesRandom.o RandFlat.o RandomEngine.o \
	RandBreitWigner.o RandGauss.o RandEngine.o \
	RandPoisson.o RanluxEngine.o RanecuEngine.o \
	-o $@ $(LDFLAGS)

particlesTest:	particlesTest.cc \
		StAlpha.o StAntiBMesonZero.o StAntiBsMesonZero.o \
		StAntiDMesonZero.o StAntiKaonZero.o StAntiLambda.o \
		StAntiLambdacPlus.o StAntiNeutrinoE.o StAntiNeutrinoMu.o \
		StAntiNeutrinoTau.o StAntiNeutron.o StAntiOmegaMinus.o \
		StAntiOmegacZero.o StAntiProton.o StAntiSigmaMinus.o \
		StAntiSigmaPlus.o StAntiSigmaZero.o StAntiSigmacPlus.o \
		StAntiSigmacPlusPlus.o StAntiSigmacZero.o StAntiXiMinus.o \
		StAntiXiZero.o StAntiXicPlus.o StAntiXicZero.o \
		StBMesonMinus.o StBMesonPlus.o StBMesonZero.o \
		StBsMesonZero.o StDMesonMinus.o StDMesonPlus.o \
		StDMesonZero.o StDeuteron.o StDsMesonMinus.o \
		StDsMesonPlus.o StElectron.o StEta.o \
		StEtaPrime.o StGamma.o StHe3.o \
		StJPsi.o StKaonMinus.o StKaonPlus.o \
		StKaonZero.o StKaonZeroLong.o StKaonZeroShort.o \
		StLambda.o StLambdacPlus.o StMuonMinus.o \
		StMuonPlus.o StNeutrinoE.o StNeutrinoMu.o \
		StNeutrinoTau.o StNeutron.o StOmegaMinus.o \
		StOmegacZero.o StOpticalPhoton.o StParticleDefinition.o \
		StParticleTable.o StPionMinus.o StPionPlus.o \
		StPionZero.o StPositron.o StProton.o \
		StRhoMinus.o StRhoPlus.o StRhoZero.o \
		StSigmaMinus.o StSigmaPlus.o StSigmaZero.o \
		StSigmacPlus.o StSigmacPlusPlus.o StSigmacZero.o \
		StTauMinus.o StTauPlus.o StTriton.o \
		StXiMinus.o StXiZero.o StXicPlus.o \
		StXicZero.o StPhi.o StOmegaMeson.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< \
	StAlpha.o StAntiBMesonZero.o StAntiBsMesonZero.o \
	StAntiDMesonZero.o StAntiKaonZero.o StAntiLambda.o \
	StAntiLambdacPlus.o StAntiNeutrinoE.o StAntiNeutrinoMu.o \
	StAntiNeutrinoTau.o StAntiNeutron.o StAntiOmegaMinus.o \
	StAntiOmegacZero.o StAntiProton.o StAntiSigmaMinus.o \
	StAntiSigmaPlus.o StAntiSigmaZero.o StAntiSigmacPlus.o \
	StAntiSigmacPlusPlus.o StAntiSigmacZero.o StAntiXiMinus.o \
	StAntiXiZero.o StAntiXicPlus.o StAntiXicZero.o \
	StBMesonMinus.o StBMesonPlus.o StBMesonZero.o \
	StBsMesonZero.o StDMesonMinus.o StDMesonPlus.o \
	StDMesonZero.o StDeuteron.o StDsMesonMinus.o \
	StDsMesonPlus.o StElectron.o StEta.o \
	StEtaPrime.o StGamma.o StHe3.o \
	StJPsi.o StKaonMinus.o StKaonPlus.o \
	StKaonZero.o StKaonZeroLong.o StKaonZeroShort.o \
	StLambda.o StLambdacPlus.o StMuonMinus.o \
	StMuonPlus.o StNeutrinoE.o StNeutrinoMu.o \
	StNeutrinoTau.o StNeutron.o StOmegaMinus.o \
	StOmegacZero.o StOpticalPhoton.o StParticleDefinition.o \
	StParticleTable.o StPionMinus.o StPionPlus.o \
	StPionZero.o StPositron.o StProton.o \
	StRhoMinus.o StRhoPlus.o StRhoZero.o \
	StSigmaMinus.o StSigmaPlus.o StSigmaZero.o \
	StSigmacPlus.o StSigmacPlusPlus.o StSigmacZero.o \
	StTauMinus.o StTauPlus.o StTriton.o \
	StXiMinus.o StXiZero.o StXicPlus.o \
	StXicZero.o  StPhi.o StOmegaMeson.o \
	-o particlesTest $(LDFLAGS)

clean:
	rm -rf *.o hbook.ntp getConfigTest hbookTest1 hbookTest2 helixTest1 \
		helixTest2 helixTest3 matrixTest particlesTest physicalHelixTest \
		promptTest randomTest1 randomTest2 randomTest3 timerTest vecTest \
		vecTest2 vecTest3 units JamesRand.conf Templates.DB memInfoTest \
		SunWS_cache circleFitTest
