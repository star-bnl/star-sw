STROOT        := ../StRoot

STARCLASSPATH := $(STROOT)/StarClassLibrary
HBTPATH       := $(STROOT)/StHbtMaker
ROOTPATH      := /afs/rhic.bnl.gov/star/ROOT/2.25.00/include
HBTSUBS       := -I$(ROOTPATH) \
                 -I$(STROOT) -I$(HBTPATH) -I$(HBTPATH)/Base -I$(HBTPATH)/Cut \
                 -I$(HBTPATH)/Infrastructure/ -I$(HBTPATH)/CorrFctn \
		 -I$(HBTPATH)/Fit -I$(HBTPATH)/Reader -I$(HBTPATH)/doc/Make -I$(STARCLASSPATH)

VPATH         := $(ROOTPATH) \
                 $(STROOT) $(HBTPATH) $(HBTPATH)/Base $(HBTPATH)/Cut \
                 $(HBTPATH)/Infrastructure/ $(HBTPATH)/CorrFctn \
		 $(HBTPATH)/Fit $(HBTPATH)/Reader $(HBTPATH)/doc/Make $(STARCLASSPATH)

HBTOBJ        := \
StHbtMaker.o \
QvecCorrFctn.o \
MinvCorrFctnM_vs_Phi.o \
MinvCorrFctn.o \
MinvCorrFctnArmenteros.o \
MinvCorrFctnM_vs_Pt.o \
Q3invCorrFctn.o \
ParityCorrFctn.o \
MinvCorrFctnY_vs_Pt.o \
QinvCorrFctn.o \
OpeningAngleCorrFctn.o \
franksPairCut.o \
trackCutMonitor_P.o \
franksV0PairCut.o \
mikesEventCut.o \
dummyV0Cut.o \
GenericTripletCut.o \
eventCutMonitor_Mult.o \
mikesPairCut.o \
trackCutMonitor_P_vs_Dedx.o \
trackCutMonitor_Pt.o \
mikesTrackCut.o \
v0CutMonitor_Minv.o \
trackCutMonitor_DCA.o \
franksTrackCut.o \
trackCutMonitor_Y_vs_Pt.o \
helensV0Cut.o \
StHbtAnalysis.o \
StHbtCheckPdgIdList.o \
StHbtSectoredAnalysis.o \
StHbtCutMonitorHandler.o \
StHbtThreeParticleAnalysis.o \
StHbtCoulomb.o \
StHbtMultiTrackCut.o \
StHbtManager.o \
StHbtVertexAnalysis.o \
StHbtAsciiReader.o \
StHbtBinaryReader.o \
StHbtClassImp.o \
StHbtEvent.o \
StHbtPair.o \
StHbtIOBinary.o \
franks1HistoD.o \
StHbtSectoredPicoEvent.o \
franks2HistoD.o \
gamovCorrect.o \
StHbtIO.o \
StHbtTriplet.o \
StHbtV0.o \
StHbtPicoEvent.o \
franks2Histo.o \
StHbtTrack.o \
StHbtParticle.o \
StHbtPicoEventCollectionVectorHideAway.o
#rotationEventCut.o \
#randomRotateEventCut.o \
#rotateToReactionPlaneEventCut.o \


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


CPPFLAGS      := -I. -I.. $(HBTSUBS) -DST_NO_NAMESPACES -DST_NO_NUMERIC_LIMITS
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
	STDHOME      := /afs/rhic.bnl.gov/star/packages/ObjectSpace/2.0m
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

.PRECIOUS: %.o %.cxx %.cc
.SUFFIXES: .hh .h .o .cc .cxx

#%:	%.cc
#	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< -o $@ $(LDFLAGS)

#%:	%.cxx
#	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< -o $@ $(LDFLAGS)

#%.o:	%.cc %.cxx
#	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) $<

%.o:	%.cxx
	@echo "*** cxx ***"
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) $<

%.o:    %.cc
	@echo "*** cc ***"
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) $<


   
##############################################################
hbt:	hbt.o $(HBTOBJ) $(STARCLASSOBJ)
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< $(HBTOBJ) $(STARCLASSOBJ) -o hbt
	echo $(LDFLAGS)

hbtQinv: hbtQinv.o $(HBTOBJ) $(STARCLASSOBJ)
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $< $(HBTOBJ) $(STARCLASSOBJ) -o hbtQinv
	echo $(LDFLAGS)

StarClass: $(STARCLASSOBJ)


obj:	hbt.o $(HBTOBJ) $(STARCLASSOBJ)

clean:
	rm *.o 
	rm hbt
