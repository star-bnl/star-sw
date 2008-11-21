#ifdef DEFINE_CUT_PARAMETERS

DEFINE_CUT_PARAMETER(Bool_t, simulation)
DEFINE_CUT_PARAMETER(Int_t, jetRotate)
DEFINE_CUT_PARAMETER(Int_t, isBadRun)
DEFINE_CUT_PARAMETER(Float_t, zCutLow)
DEFINE_CUT_PARAMETER(Float_t, zCutHigh)
DEFINE_CUT_PARAMETER(Int_t, neutralPointsCutLow)
DEFINE_CUT_PARAMETER(Int_t, neutralPointsCutHigh)
DEFINE_CUT_PARAMETER(Int_t, HT1AdcThreshold)
DEFINE_CUT_PARAMETER(Int_t, HT2AdcThreshold)
DEFINE_CUT_PARAMETER(Float_t, HT1EtThreshold)
DEFINE_CUT_PARAMETER(Float_t, HT2EtThreshold)
DEFINE_CUT_PARAMETER(ULong_t, triggersMB)
DEFINE_CUT_PARAMETER(ULong_t, triggersHT1)
DEFINE_CUT_PARAMETER(ULong_t, triggersHT2)
DEFINE_CUT_PARAMETER(Float_t, trackDistCutLow)
DEFINE_CUT_PARAMETER(Float_t, trackDistCutHigh)
DEFINE_CUT_PARAMETER(Float_t, gammaDistCutLow)
DEFINE_CUT_PARAMETER(Float_t, gammaDistCutHigh)
DEFINE_CUT_PARAMETER(Float_t, asymCutLow)
DEFINE_CUT_PARAMETER(Float_t, asymCutHigh)
DEFINE_CUT_PARAMETER(Float_t, ptLow)
DEFINE_CUT_PARAMETER(Float_t, ptHigh)
DEFINE_CUT_PARAMETER(Float_t, ptBinStart)
DEFINE_CUT_PARAMETER(Float_t, ptBinStep)
DEFINE_CUT_PARAMETER(Float_t, gammaConversionRadiusLow)
DEFINE_CUT_PARAMETER(Float_t, gammaConversionRadiusHigh)
DEFINE_CUT_PARAMETER(Float_t, massRegionLeft)
DEFINE_CUT_PARAMETER(Float_t, massRegionLeftPt)
DEFINE_CUT_PARAMETER(Float_t, massRegionRight)
DEFINE_CUT_PARAMETER(Float_t, massRegionRightPt)
DEFINE_CUT_PARAMETER(Float_t, pointEnergyLow)
DEFINE_CUT_PARAMETER(Float_t, pointEnergyHigh)
DEFINE_CUT_PARAMETER(Float_t, smdEnergyLow)
DEFINE_CUT_PARAMETER(Float_t, smdEnergyHigh)
DEFINE_CUT_PARAMETER(Int_t, smdSizeLow)
DEFINE_CUT_PARAMETER(Int_t, smdSizeHigh)
DEFINE_CUT_PARAMETER(Float_t, etaCoordLow)
DEFINE_CUT_PARAMETER(Float_t, etaCoordHigh)
DEFINE_CUT_PARAMETER(Float_t, etaLow)
DEFINE_CUT_PARAMETER(Float_t, etaHigh)
DEFINE_CUT_PARAMETER(Float_t, phiCoordLow)
DEFINE_CUT_PARAMETER(Float_t, phiCoordHigh)
DEFINE_CUT_PARAMETER(Float_t, openAngleMinFraction)
DEFINE_CUT_PARAMETER(Float_t, openAngleMinOffset)
DEFINE_CUT_PARAMETER(Int_t, ftpcRefMultLow)
DEFINE_CUT_PARAMETER(Int_t, ftpcRefMultHigh)
DEFINE_CUT_PARAMETER(Int_t, tpcRefMultLow)
DEFINE_CUT_PARAMETER(Int_t, tpcRefMultHigh)
DEFINE_CUT_PARAMETER(Float_t, clustersPerTrackLow)
DEFINE_CUT_PARAMETER(Float_t, clustersPerTrackHigh)
DEFINE_CUT_PARAMETER(Float_t, EMCEtNeutralToTotalLow)
DEFINE_CUT_PARAMETER(Float_t, EMCEtNeutralToTotalHigh)
DEFINE_CUT_PARAMETER(Float_t, EMCENeutralToTotalLow)
DEFINE_CUT_PARAMETER(Float_t, EMCENeutralToTotalHigh)
DEFINE_CUT_PARAMETER(Float_t, TPCPtToEMCEtLow)
DEFINE_CUT_PARAMETER(Float_t, TPCPtToEMCEtHigh)
DEFINE_CUT_PARAMETER(Float_t, TPCPt0VsEMCEt0)
DEFINE_CUT_PARAMETER(Float_t, TPCPt1VsEMCEt0)
DEFINE_CUT_PARAMETER(Float_t, TPCPt0VsEMCEt1)
DEFINE_CUT_PARAMETER(Float_t, TPCPt1VsEMCEt1)
DEFINE_CUT_PARAMETER(Float_t, TPCPt0VsEMCEt2)
DEFINE_CUT_PARAMETER(Float_t, TPCPt2VsEMCEt0)
DEFINE_CUT_PARAMETER(Float_t, highestHitEnergyFractionSMDE)
DEFINE_CUT_PARAMETER(Float_t, highestHitEnergyFractionSMDP)
DEFINE_CUT_PARAMETER(Float_t, highestHitEnergyFractionBTOW)
DEFINE_CUT_PARAMETER(Float_t, jetEtLow)
DEFINE_CUT_PARAMETER(Float_t, jetDistCutLow)
DEFINE_CUT_PARAMETER(Float_t, jetDistCutHigh)
DEFINE_CUT_PARAMETER(TString, bunchCrossingIdOffsetsFilename)
DEFINE_CUT_PARAMETER(Float_t, zBBCcoeff0)
DEFINE_CUT_PARAMETER(Float_t, zBBCcoeff1)
DEFINE_CUT_PARAMETER(Bool_t, useZTPC)
DEFINE_CUT_PARAMETER(Bool_t, useZBBC)
DEFINE_CUT_PARAMETER(Bool_t, zSimuSmearing)
DEFINE_CUT_PARAMETER(Float_t, zSimuMeanCoeff0)
DEFINE_CUT_PARAMETER(Float_t, zSimuMeanCoeff1)
DEFINE_CUT_PARAMETER(Float_t, zSimuSpreadCoeff0)
DEFINE_CUT_PARAMETER(Float_t, zSimuSpreadCoeff1)
DEFINE_CUT_PARAMETER(TWeightCalculator*, zSimuSmearingPt)
DEFINE_CUT_PARAMETER(Bool_t, zSimuSmearingZero)
DEFINE_CUT_PARAMETER(TWeightCalculator*, zSimuSmearingZeroPt)
DEFINE_CUT_PARAMETER(Int_t, highestAdcLow)
DEFINE_CUT_PARAMETER(Int_t, highestAdcHigh)
DEFINE_CUT_PARAMETER(Float_t, highestEtLow)
DEFINE_CUT_PARAMETER(Float_t, highestEtHigh)
DEFINE_CUT_PARAMETER(TString, badEventsListFilename)
DEFINE_CUT_PARAMETER(Float_t, pythiaPi0PtCutoff)
DEFINE_CUT_PARAMETER(Float_t, calibrationSlope)

#else

#ifndef StPi0Analysis_TCutParameters_H
#define StPi0Analysis_TCutParameters_H

#include <list>
#include <map>
using namespace std;

#include <TNamed.h>

#include "StPi0AnalysisVersion.h"

class TWeightCalculator;

class TCutParameters : public TNamed {
	public:
		typedef TCutParameters this_type;
		typedef TNamed inherited;

		TCutParameters(const Char_t *name = 0, const Char_t *title = 0);
		TCutParameters(const this_type &parameters);
		virtual ~TCutParameters();
		
		this_type &operator=(const this_type &parameters);
		Bool_t operator==(const this_type &parameters) const;
		Bool_t operator!=(const this_type &parameters) const;

		virtual void Print(Option_t* option) const;

#define DEFINE_CUT_PARAMETERS
#define DEFINE_CUT_PARAMETER(TYPE, NAME) TYPE NAME;
#include "TCutParameters.h"
#undef DEFINE_CUT_PARAMETER

		ClassDef(TCutParameters, STPI0ANALYSIS_VERSION);
};

#endif

#endif
#undef DEFINE_CUT_PARAMETERS
