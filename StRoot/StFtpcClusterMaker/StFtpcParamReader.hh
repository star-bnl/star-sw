// $Id: StFtpcParamReader.hh,v 1.6 2000/11/06 13:42:57 hummler Exp $
//
// $Log: StFtpcParamReader.hh,v $
// Revision 1.6  2000/11/06 13:42:57  hummler
// include latest changes in second constructor as well
//
// Revision 1.3  2000/10/31 09:52:19  hummler
// add parameters for slow simulator
//
// Revision 1.2  2000/09/18 14:26:51  hummler
// expand StFtpcParamReader to supply data for slow simulator as well
// introduce StFtpcGeantReader to separate g2t tables from simulator code
// implement StFtpcGeantReader in StFtpcFastSimu
//
// Revision 1.1  2000/08/03 14:39:01  hummler
// Create param reader to keep parameter tables away from cluster finder and
// fast simulator. StFtpcClusterFinder now knows nothing about tables anymore!
//

#ifndef STAR_StFtpcParamReader
#define STAR_StFtpcParamReader

#include <sys/types.h>
#include "TObject.h"

#include "tables/St_fcl_padtrans_Table.h"
#include "tables/St_fcl_ampslope_Table.h"
#include "tables/St_fcl_ampoff_Table.h"
#include "tables/St_fcl_timeoff_Table.h"
#include "tables/St_fcl_det_Table.h"
#include "tables/St_fcl_zrow_Table.h"
#include "tables/St_ffs_gaspar_Table.h"
#include "tables/St_fss_gas_Table.h"
#include "tables/St_fss_param_Table.h"

#define TRUE 1
#define FALSE 0

class StFtpcParamReader : public TObject 
{
  
protected:
  //STAF table pointers stored for writing back in destructor
  //set to NULL if not set in constructor
  fcl_padtrans_st *mPadtransTable;

  //ClusterFinder parameters (also used by other classes)
  Float_t *mAmplitudeOffset;
  Float_t *mAmplitudeSlope;
  Float_t *mTimeOffset;
  Int_t mNumberOfCalibrationValues;
  Int_t mNumberOfPadtransBins;
  Int_t mNumberOfPadrowsPerSide;
  Float_t *mPadtransEField;
  Float_t *mPadtransVDrift;
  Float_t *mPadtransDeflection;
  Float_t *mPadtransdVDriftdP;
  Float_t *mPadtransdDeflectiondP;
  Int_t mFirstPadrowToSearch;
  Int_t mLastPadrowToSearch;
  Int_t mFirstSectorToSearch;
  Int_t mLastSectorToSearch;
  Int_t mNumberOfPadrows;
  Int_t mNumberOfSectors;
  Int_t mNumberOfPads;
  Int_t mNumberOfTimebins;
  Int_t mGaussFittingFlags;
  Int_t mMinimumClusterMaxADC;
  Int_t mNumberOfDriftSteps;
  Int_t mDirectionOfMagnetField;
  Float_t mSensitiveVolumeInnerRadius;
  Float_t mSensitiveVolumeOuterRadius;
  Float_t mRadiusTimesField;
  Float_t mRadiansPerDegree;
  Float_t mMicrosecondsPerTimebin;
  Float_t mRadiansPerPad;
  Float_t mRadiansPerBoundary;
  Float_t mStandardPressure;
  Float_t mNormalizedNowPressure;
  Int_t mOrderOfDiffusionErrors;
  Float_t *mPadDiffusionErrors;
  Float_t *mTimeDiffusionErrors;
  Float_t mPadBadFitError;
  Float_t mTimeBadFitError;
  Float_t mPadUnfoldError;
  Float_t mTimeUnfoldError;
  Float_t mPadFailedFitError;
  Float_t mTimeFailedFitError;
  Float_t mPadCutoffClusterError;
  Float_t mTimeCutoffClusterError;
  Float_t mPadSaturatedClusterError;
  Float_t mTimeSaturatedClusterError;
  Float_t m2PadWeightedError;
  Float_t m2PadGaussError;
  Float_t m3PadWeightedError;
  Float_t m3PadGaussError;
  Float_t mZDirectionError;
  Float_t mMinimumDriftField;
  Float_t mStepSizeDriftField;
  Float_t mDvdpCalcOffset;
  Float_t mBaseTemperature;
  Float_t mPercentAr;
  Float_t mPercentCO2;
  Float_t mPercentNe;
  Float_t mPercentHe;
  Float_t *mPadrowZPosition;
  //FastSimulator parameters
  Int_t mOrderOfFastEstimates;
  Float_t *mVDriftEstimates;
  Float_t *mTDriftEstimates;
  Float_t *mSigmaRadialEstimates;
  Float_t *mSigmaAzimuthalEstimates;
  Float_t *mErrorRadialEstimates;
  Float_t *mErrorAzimuthalEstimates;
  //SlowSimulator parameters
  Int_t mNumberOfFssGasValues;
  Float_t *mFssGasEField;
  Float_t *mFssGasVDrift;
  Float_t *mFssGasDiffusionX;
  Float_t *mFssGasDiffusionY;
  Float_t *mFssGasDiffusionZ;
  Float_t *mFssGasLorentzAngle;
  Int_t mRandomNumberGenerator;
  Int_t mZeroSuppressThreshold;
  Int_t mNumSlowSimGridPoints;
  Int_t mMaxAdc;
  Int_t mGaussIntegrationSteps;
  Int_t mDiffusionCoarseness;
  Float_t mAdcConversion;
  Float_t mSimulationPhiStart;
  Float_t mSimulationPhiEnd;
  Float_t mChamberCathodeVoltage;
  Float_t mGasAttenuation;
  Float_t mGasGain;
  Float_t mGasIonizationPotential;
  Float_t mPadLength;
  Float_t mSigmaPadResponseFuntion;
  Float_t mReadoutShaperTime;
  
public:
  // constructor used by StFtpcClusterMaker:
  StFtpcParamReader(St_fcl_ampoff *ampoff,
		    St_fcl_ampslope *ampslope,
		    St_fcl_timeoff *timeoff,
		    St_fcl_padtrans *padtrans,
		    St_fcl_det *det,
		    St_fcl_zrow *zrow,
		    St_ffs_gaspar *gaspar);
  // constructor used by StFtpcSlowSimMaker:
  StFtpcParamReader(St_fss_gas *gas,
		    St_fss_param *param,
		    St_fcl_padtrans *padtrans,
		    St_fcl_det *det,
		    St_fcl_zrow *zrow);
  ~StFtpcParamReader();
  Float_t amplitudeOffset(Int_t i);
  Float_t amplitudeSlope(Int_t i);
  Float_t timeOffset(Int_t i);
  Float_t padtransEField(Int_t i); 
  Float_t padtransVDrift(Int_t i, Int_t padrow); 
  Float_t padtransDeflection(Int_t i, Int_t padrow); 
  Float_t padtransdVDriftdP(Int_t i, Int_t padrow); 
  Float_t padtransdDeflectiondP(Int_t i, Int_t padrow); 
  Float_t padDiffusionErrors(Int_t i); 
  Float_t timeDiffusionErrors(Int_t i); 
  Float_t padrowZPosition(Int_t i); 
  Float_t vDriftEstimates(Int_t i); 
  Float_t tDriftEstimates(Int_t i); 
  Float_t sigmaRadialEstimates(Int_t i); 
  Float_t sigmaAzimuthalEstimates(Int_t i); 
  Float_t errorRadialEstimates(Int_t i); 
  Float_t errorAzimuthalEstimates(Int_t i); 
  Float_t fssGasEField(Int_t i);
  Float_t fssGasVDrift(Int_t i);
  Float_t fssGasDiffusionX(Int_t i);
  Float_t fssGasDiffusionY(Int_t i);
  Float_t fssGasDiffusionZ(Int_t i);
  Float_t fssGasLorentzAngle(Int_t i);
  // parameter set functions
  Int_t setPadtransEField(Int_t i, Float_t newvalue); 
  Int_t setPadtransVDrift(Int_t i, Int_t padrow, Float_t  newvalue); 
  Int_t setPadtransDeflection(Int_t i, Int_t padrow, Float_t  newvalue); 
  Int_t setPadtransdVDriftdP(Int_t i, Int_t padrow, Float_t  newvalue); 
  Int_t setPadtransdDeflectiondP(Int_t i, Int_t padrow, Float_t  newvalue); 

  // inline get functions
  Int_t numberOfCalibrationValues() {return mNumberOfCalibrationValues;}
  Int_t numberOfPadtransBins() {return mNumberOfPadtransBins;}
  Int_t numberOfPadrowsPerSide() {return mNumberOfPadrowsPerSide;}
  Int_t firstPadrowToSearch() {return mFirstPadrowToSearch;}
  Int_t lastPadrowToSearch() {return mLastPadrowToSearch;}
  Int_t firstSectorToSearch() {return mFirstSectorToSearch;}
  Int_t lastSectorToSearch() {return mLastSectorToSearch;}
  Int_t numberOfPadrows() {return mNumberOfPadrows;}
  Int_t numberOfSectors() {return mNumberOfSectors;}
  Int_t numberOfPads() {return mNumberOfPads;}
  Int_t numberOfTimebins() {return mNumberOfTimebins;}
  Int_t gaussFittingFlags() {return mGaussFittingFlags;}
  Int_t minimumClusterMaxADC() {return mMinimumClusterMaxADC;}
  Int_t numberOfDriftSteps() {return mNumberOfDriftSteps;}
  Int_t directionOfMagnetField() {return mDirectionOfMagnetField;}
  Float_t sensitiveVolumeInnerRadius() {return mSensitiveVolumeInnerRadius;}
  Float_t sensitiveVolumeOuterRadius() {return mSensitiveVolumeOuterRadius;}
  Float_t radiusTimesField() {return mRadiusTimesField;}
  Float_t radiansPerDegree() {return mRadiansPerDegree;}
  Float_t microsecondsPerTimebin() {return mMicrosecondsPerTimebin;}
  Float_t radiansPerPad() {return mRadiansPerPad;}
  Float_t radiansPerBoundary() {return mRadiansPerBoundary;}
  Float_t standardPressure() {return mStandardPressure;}
  Float_t normalizedNowPressure() {return mNormalizedNowPressure;}
  Float_t padBadFitError() {return mPadBadFitError;}
  Float_t timeBadFitError() {return mTimeBadFitError;}
  Float_t padUnfoldError() {return mPadUnfoldError;}
  Float_t timeUnfoldError() {return mTimeUnfoldError;}
  Float_t padFailedFitError() {return mPadFailedFitError;}
  Float_t timeFailedFitError() {return mTimeFailedFitError;}
  Float_t padCutoffClusterError() {return mPadCutoffClusterError;}
  Float_t timeCutoffClusterError() {return mTimeCutoffClusterError;}
  Float_t padSaturatedClusterError() {return mPadSaturatedClusterError;}
  Float_t timeSaturatedClusterError() {return mTimeSaturatedClusterError;}
  Float_t twoPadWeightedError() {return m2PadWeightedError;}
  Float_t twoPadGaussError() {return m2PadGaussError;}
  Float_t threePadWeightedError() {return m3PadWeightedError;}
  Float_t threePadGaussError() {return m3PadGaussError;}
  Float_t zDirectionError() {return mZDirectionError;}
  Float_t minimumDriftField() {return mMinimumDriftField;}
  Float_t stepSizeDriftField() {return mStepSizeDriftField;}
  Float_t dvdpCalcOffset() {return mDvdpCalcOffset;}
  Float_t baseTemperature() {return mBaseTemperature;}
  Float_t percentAr() {return mPercentAr;}
  Float_t percentCO2() {return mPercentCO2;}
  Float_t percentNe() {return mPercentNe;}
  Float_t percentHe() {return mPercentHe;}
  Int_t numberOfFssGasValues() {return mNumberOfFssGasValues;}
  Int_t randomNumberGenerator() {return mRandomNumberGenerator;}
  Int_t zeroSuppressThreshold() {return mZeroSuppressThreshold;}
  Int_t numSlowSimGridPoints() {return mNumSlowSimGridPoints;}
  Int_t maxAdc() {return mMaxAdc;}
  Int_t gaussIntegrationSteps() {return mGaussIntegrationSteps;}
  Int_t diffusionCoarseness() {return mDiffusionCoarseness;}
  Float_t adcConversion() {return mAdcConversion;}
  Float_t simulationPhiStart() {return mSimulationPhiStart;}
  Float_t simulationPhiEnd() {return mSimulationPhiEnd;}
  Float_t chamberCathodeVoltage() {return mChamberCathodeVoltage;}
  Float_t gasAttenuation() {return mGasAttenuation;}
  Float_t gasGain() {return mGasGain;}
  Float_t gasIonizationPotential() {return mGasIonizationPotential;}
  Float_t padLength() {return mPadLength;}
  Float_t sigmaPadResponseFuntion() {return mSigmaPadResponseFuntion;}
  Float_t readoutShaperTime() {return mReadoutShaperTime;}
  Float_t padPitch() {return mRadiansPerPad*mSensitiveVolumeOuterRadius;}
};

#endif








