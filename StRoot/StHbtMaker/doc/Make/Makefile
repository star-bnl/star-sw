STROOT        := ../StRoot

STARCLASSPATH := $(STROOT)/StarClassLibrary
HBTPATH       := $(STROOT)/StHbtMaker
HBTSUBS       := -I$(STROOT) -I$(HBTPATH) -I$(HBTPATH)/Base -I$(HBTPATH)/Cut \
                 -I$(HBTPATH)/Infrastructure/ -I$(HBTPATH)/CorrFctn \
		 -I$(HBTPATH)/Fit -I$(HBTPATH)/Reader -I$(STARCLASSPATH)

VPATH         := $(STROOT) $(HBTPATH) $(HBTPATH)/Base $(HBTPATH)/Cut \
                 $(HBTPATH)/Infrastructure/ $(HBTPATH)/CorrFctn \
		 $(HBTPATH)/Fit $(HBTPATH)/Reader $(STARCLASSPATH)

HBTOBJ        := \
StHbtEvent.o \
StHbtIO.o \
StHbtIOBinary.o \
StHbtPair.o \
StHbtV0.o \
StHbtParticle.o \
StHbtPicoEvent.o \
gamovCorrect.o \
StHbtMaker.o \
ExitSepCorrFctn.o \
MinvCorrFctn.o \
OpeningAngleCorrFctn.o \
ParityCorrFctn.o \
QinvCorrFctn.o \
QinvCorrFctnC.o \
QvecCorrFctn.o \
ParityEventCut.o \
dummyV0Cut.o \
eventCutMonitor_Mult.o \
franksTrackCut.o \
helensV0Cut.o \
mikesEventCut.o \
mikesPairCut.o \
mikesTrackCut.o \
trackCutMonitor_DCA.o \
trackCutMonitor_P.o \
trackCutMonitor_P_vs_Dedx.o \
trackCutMonitor_Pt.o \
trackCutMonitor_Y_vs_Pt.o \
StHbtAnalysis.o \
StHbtCoulomb.o \
StHbtCutMonitorHandler.o \
StHbtManager.o \
StHbtAsciiReader.o \
StHbtBinaryReader.o \
franks1HistoD.o \
franks2HistoD.o 

STARCLASSOBJ        := \
StMatrixF.o \
StMemoryInfo.o \
StMemoryPool.o \
StMuonMinus.o \
StMuonPlus.o \
StNeutrinoE.o \
StNeutrinoMu.o \
StNeutrinoTau.o \
StNeutron.o \
StOmegaMinus.o \
StOmegacZero.o \
StOpticalPhoton.o \
StParticleDefinition.o \
StParticleTable.o \
StPhysicalHelix.o \
StPhysicalHelixD.o \
StPionMinus.o \
StPionPlus.o \
StPionZero.o \
StPositron.o \
StProton.o \
StRhoMinus.o \
StRhoPlus.o \
StRhoZero.o \
StSigmaMinus.o \
StSigmaPlus.o \
StSigmaZero.o \
StSigmacPlus.o \
StSigmacPlusPlus.o \
StSigmacZero.o \
StTauMinus.o \
StTauPlus.o \
StThreeVectorD.o \
StThreeVectorF.o \
StTimer.o \
StTriton.o \
StXiMinus.o \
StXiZero.o \
StXicPlus.o \
StXicZero.o \
StBMesonMinus.o \
StBMesonPlus.o \
StBMesonZero.o \
StBsMesonZero.o \
StDMesonMinus.o \
StDMesonPlus.o \
StDMesonZero.o \
StDsMesonMinus.o \
StDsMesonPlus.o \
StElectron.o \
StEta.o \
StEtaPrime.o \
StFastCircleFitter.o \
StGamma.o \
StHe3.o \
StHelix.o \
StHelixD.o \
StJPsi.o \
StKaonMinus.o \
StKaonPlus.o \
StKaonZero.o \
StKaonZeroLong.o \
StKaonZeroShort.o \
StLambda.o \
StLambdacPlus.o \
StLorentzVectorD.o \
StLorentzVectorF.o \
StMatrixD.o \
StAlpha.o \
StAntiBMesonZero.o \
StAntiBsMesonZero.o \
StAntiDMesonZero.o \
StAntiKaonZero.o \
StAntiLambda.o \
StAntiLambdacPlus.o \
StAntiNeutrinoE.o \
StAntiNeutrinoMu.o \
StAntiNeutrinoTau.o \
StAntiNeutron.o \
StAntiOmegaMinus.o \
StAntiOmegacZero.o \
StAntiProton.o \
StAntiSigmaMinus.o \
StAntiSigmaPlus.o \
StAntiXicPlus.o \
StAntiXicZero.o \
StDeuteron.o \
StAntiSigmaZero.o \
StAntiSigmacPlus.o \
StAntiSigmacPlusPlus.o \
StAntiSigmacZero.o \
StAntiXiMinus.o \
StAntiXiZero.o 


CPPFLAGS      := -I. -I.. $(HBTSUBS) -DST_NO_NAMESPACES
CXXFLAGS      := -g -Wall 
LDFLAGS       := -L$(CERN)/pro/lib -lpacklib -lhbtlib
#LDFLAGS       := -lhbtlib

#
#   Architecture dependent variables...
#


# -------- Linux ---------

ifeq ($(findstring i386, $(STAR_SYS)),i386)
	CXX          := g++ 
	CPPFLAGS     += -DGNU_GCC -DST_NO_NUMERIC_LIMITS
	CXXFLAGS     += -fno-for-scope
	LD           := g++ 
	LDFLAGS      += -lcrypt -ldl -lg2c
endif

# -------- sun4x_54, sun4x_55, sun4x_56 ---------

ifeq ($(findstring sun4x, $(STAR_SYS)),sun4x)
	CXX          := CC
	STDHOME      := /afs/rhic/star/packages/ObjectSpace/2.0m
	CPPFLAGS     += -I$(STDHOME) -I$(STDHOME)/ospace/std 
	CPPFLAGS     += -DST_NO_MEMBER_TEMPLATES -DST_NO_NUMERIC_LIMITS
	CPPFLAGS     += -DST_NO_EXCEPTIONS -DST_NO_TEMPLATE_DEF_ARGS
	CXXFLAGS     += -Qoption ld -t
	LD	     := $(CXX)
	LDFLAGS      += -L$(STDHOME)/lib -lospace -lF77 -lM77 -lsocket -lnsl -lsunmath
endif

#
#   Rules and dependencies
#

.PRECIOUS: %.o %.cxx
.SUFFIXES: .h .o .cc .cxx

#%:	%.cc
#	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< -o $@ $(LDFLAGS)

#%:	%.cxx
#	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< -o $@ $(LDFLAGS)

#%.o:	%.cc %.cxx
#	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) $<

%.o:	%.cxx
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) $<

#%.o.cc:
#	@echo "*** cc ***"
#	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) $<


   
##############################################################
hbt:	hbt.o $(HBTOBJ) $(STARCLASSOBJ)
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< $(HBTOBJ) $(STARCLASSOBJ) -o hbt
	echo $(LDFLAGS)

StarClass: $(STARCLASSOBJ)


obj:	hbt.o $(HBTOBJ) $(STARCLASSOBJ)

clean:
	rm *.o 
	rm hbt
