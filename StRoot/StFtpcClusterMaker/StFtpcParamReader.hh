// $Id: StFtpcParamReader.hh,v 1.18 2002/01/21 22:14:56 jcs Exp $
//
// $Log: StFtpcParamReader.hh,v $
// Revision 1.18  2002/01/21 22:14:56  jcs
// added values for temperature/pressure calculations to ftpcClusterPars
//
// Revision 1.17  2001/07/11 21:19:33  jcs
// remove obsolete entries in tables
//
// Revision 1.16  2001/04/24 07:11:45  oldi
// Float_t mSlowSimPressure introduced to replace mNormalizedNowPressure in
// StFtpcSlowSimMaker.
//
// Revision 1.15  2001/04/02 12:10:28  jcs
// get FTPC calibrations,geometry from MySQL database and code parameters
// from StarDb/ftpc
//
// Revision 1.14  2001/03/19 15:52:48  jcs
// use ftpcDimensions from database
//
// Revision 1.13  2001/03/06 23:34:16  jcs
// use database instead of params
//
// Revision 1.12  2001/01/27 20:07:47  jcs
// change name of parameter
//
// Revision 1.11  2001/01/15 16:08:52  jcs
// get phiOrigin and phiPerSector fro ftpcDimensions
//
// Revision 1.10  2001/01/08 17:07:17  jcs
// move remaining constants from code to database
//
// Revision 1.9  2000/12/11 16:39:11  jcs
// move FTPC geant volume id and cluster flags from code to parameter reader
//
// Revision 1.8  2000/11/27 14:09:26  hummler
// implement tzero and lorentz angle correction factor
//
// Revision 1.7  2000/11/14 13:08:30  hummler
// add charge step calculation, minor cleanup
//
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

#include "tables/St_ftpcClusterPars_Table.h"
#include "tables/St_ftpcSlowSimGas_Table.h"
#include "tables/St_ftpcSlowSimPars_Table.h"
#include "tables/St_ftpcFastSimGas_Table.h"
#include "tables/St_ftpcFastSimPars_Table.h"

#define TRUE 1
#define FALSE 0

class StFtpcParamReader : public TObject 
{
  
protected:
  //STAF table pointers stored for writing back in destructor
  //set to NULL if not set in constructor
  ftpcClusterPars_st *mClusterParsTable;

  //ClusterFinder parameters (also used by other classes)
  Int_t mGaussFittingFlags;
  Int_t mMinimumClusterMaxADC;
  Int_t mNumberOfDriftSteps;
  Float_t mStandardPressure;
  Float_t mNormalizedNowPressure;
  Float_t mAdjustedAirPressureWest;
  Float_t mAdjustedAirPressureEast;
  Float_t mMinPressure;
  Float_t mMaxPressure;
  Float_t mGasTemperatureWest;
  Float_t mGasTemperatureEast;
  Float_t mMinGasTemperature; 
  Float_t mMaxGasTemperature;
  Float_t mLorentzAngleFactor;
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
  //FastSimulator parameters
  Int_t mOrderOfFastEstimates;
  Float_t *mVDriftEstimates;
  Float_t *mTDriftEstimates;
  Float_t *mSigmaRadialEstimates;
  Float_t *mSigmaAzimuthalEstimates;
  Float_t *mErrorRadialEstimates;
  Float_t *mErrorAzimuthalEstimates;
  Int_t mFtpcWestGeantVolumeId;
  Int_t mFtpcEastGeantVolumeId;
  Int_t mUnfoldedClusterFlag;
  Int_t mBadShapeClusterFlag;
  Int_t mMergedClusterFlag;
  Int_t mNumberOfPadsDedxSmearing;
  Int_t mNumberOfBinsDedxSmearing;
  Float_t mRadiusTolerance;
  Float_t mSigmaSpacingFactor;
  Float_t mAdcConversionFactor;
  Float_t mClusterChargeConversionFactor;
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
  Float_t mChamberCathodeVoltage;
  Float_t mSigmaPadResponseFuntion;
  Float_t mReadoutShaperTime;
  Float_t mSlowSimPressure;
  
public:
  // constructor used by StFtpcClusterMaker:
  StFtpcParamReader(St_ftpcClusterPars *det,
		    St_ftpcFastSimGas *gaspar,
                    St_ftpcFastSimPars *param);
  // constructor used by StFtpcSlowSimMaker:
  StFtpcParamReader(St_ftpcClusterPars *det,
                    St_ftpcSlowSimGas  *gas,
                    St_ftpcSlowSimPars *param);
  ~StFtpcParamReader();
  Float_t padDiffusionErrors(Int_t i); 
  Float_t timeDiffusionErrors(Int_t i); 
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
  Int_t setNormalizedNowPressure(Float_t f) {mNormalizedNowPressure=f; return 1;}
  Int_t setAdjustedAirPressureWest(Float_t f) {mAdjustedAirPressureWest=f;return 0;}
  Int_t setAdjustedAirPressureEast(Float_t f) {mAdjustedAirPressureEast=f;return 0;}
  Int_t setGasTemperatureWest(Float_t f) {mGasTemperatureWest = f;return 0;}
  Int_t setGasTemperatureEast(Float_t f) {mGasTemperatureEast = f;return 0;}

  // inline get functions
  Int_t gaussFittingFlags() {return mGaussFittingFlags;}
  Int_t minimumClusterMaxADC() {return mMinimumClusterMaxADC;}
  Int_t numberOfDriftSteps() {return mNumberOfDriftSteps;}
  Float_t standardPressure() {return mStandardPressure;}
  Float_t normalizedNowPressure() {return mNormalizedNowPressure;}
  Float_t adjustedAirPressureWest() {return mAdjustedAirPressureWest;}
  Float_t adjustedAirPressureEast() {return mAdjustedAirPressureEast;}
  Float_t minPressure() {return mMinPressure;}
  Float_t maxPressure() {return mMaxPressure;}
  Float_t gasTemperatureEast() {return mGasTemperatureEast;}
  Float_t gasTemperatureWest() {return mGasTemperatureWest;}
  Float_t minGasTemperature() {return mMinGasTemperature;}
  Float_t maxGasTemperature() {return mMaxGasTemperature;}
  Float_t lorentzAngleFactor() {return mLorentzAngleFactor;}
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
  Int_t ftpcWestGeantVolumeId() {return mFtpcWestGeantVolumeId;}
  Int_t ftpcEastGeantVolumeId() {return mFtpcEastGeantVolumeId;}
  Int_t unfoldedClusterFlag() {return mUnfoldedClusterFlag;}
  Int_t badShapeClusterFlag() {return mBadShapeClusterFlag;}
  Int_t mergedClusterFlag() {return mMergedClusterFlag;}
  Int_t numberOfPadsDedxSmearing() {return mNumberOfPadsDedxSmearing;}
  Int_t numberOfBinsDedxSmearing() {return mNumberOfBinsDedxSmearing;}
  Float_t radiusTolerance() {return mRadiusTolerance;}
  Float_t sigmaSpacingFactor() {return mSigmaSpacingFactor;}
  Float_t adcConversionFactor() {return mAdcConversionFactor;}
  Float_t clusterChargeConversionFactor() {return mClusterChargeConversionFactor;}
  Int_t numberOfFssGasValues() {return mNumberOfFssGasValues;}
  Int_t randomNumberGenerator() {return mRandomNumberGenerator;}
  Int_t zeroSuppressThreshold() {return mZeroSuppressThreshold;}
  Int_t numSlowSimGridPoints() {return mNumSlowSimGridPoints;}
  Int_t maxAdc() {return mMaxAdc;}
  Int_t gaussIntegrationSteps() {return mGaussIntegrationSteps;}
  Int_t diffusionCoarseness() {return mDiffusionCoarseness;}
  Float_t adcConversion() {return mAdcConversion;}
  Float_t chamberCathodeVoltage() {return mChamberCathodeVoltage;}
  Float_t sigmaPadResponseFuntion() {return mSigmaPadResponseFuntion;}
  Float_t readoutShaperTime() {return mReadoutShaperTime;}
  Float_t slowSimPressure() {return mSlowSimPressure;}

};

#endif

