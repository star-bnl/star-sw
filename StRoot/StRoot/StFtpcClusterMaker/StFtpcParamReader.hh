// $Id: StFtpcParamReader.hh,v 1.28 2007/01/15 07:49:22 jcs Exp $
//
// $Log: StFtpcParamReader.hh,v $
// Revision 1.28  2007/01/15 07:49:22  jcs
// replace printf, cout and gMesMgr with Logger
//
// Revision 1.27  2006/03/13 19:26:59  jcs
// add constructor StFtpcCalibMaker
//
// Revision 1.26  2003/10/10 12:37:14  jcs
// remove parameters used to calculate the FTPC geant volume id using the obsolete method
//
// Revision 1.25  2003/06/11 11:10:15  jcs
// remove inner cathode and cluster geometry parameters from ftpcClusterPars
//
// Revision 1.24  2003/06/10 13:11:51  jcs
// min,max gas temperature and pressure limits removed from ftpcClusterPars
//
// Revision 1.23  2003/05/07 15:10:46  putschke
// improvements for cathode offset corretions
//
// Revision 1.22  2003/04/15 11:36:22  putschke
// Include corrections for inner cathode offset and move some parameter to database
//
// Revision 1.21  2002/04/05 16:46:02  oldi
// Small code clean ups, to be sure that this part is recompiled. It relies
// on StFtpcTracker/StFtpcPoint.* which were changed.
//
// Revision 1.20  2002/03/01 14:22:21  jcs
// add additional histograms to monitor cluster finding
//
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

#include "StMaker.h"

#include "TObject.h"

#include "tables/St_ftpcClusterPars_Table.h"
#include "tables/St_ftpcSlowSimGas_Table.h"
#include "tables/St_ftpcSlowSimPars_Table.h"
#include "tables/St_ftpcFastSimGas_Table.h"
#include "tables/St_ftpcFastSimPars_Table.h"

class StFtpcParamReader : public TObject 
{
  
protected:

  // table pointer stored for writing back in destructor
  ftpcClusterPars_st *mClusterParsTable;

  //ClusterFinder parameters (also used by other classes)
  Int_t mGaussFittingFlags;
  Int_t mMinimumClusterMaxADC;
  Int_t mNumberOfDriftSteps;
  Float_t mStandardPressure;
  Float_t mNormalizedNowPressure;
  Float_t mAdjustedAirPressureWest;
  Float_t mAdjustedAirPressureEast;
  Float_t mGasTemperatureWest;
  Float_t mGasTemperatureEast;
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
  Int_t mMaxNumSequences; 
  Int_t mMaxNumSeqPeaks;
  Int_t mMaxNumPeaks;          
  Int_t mMaxNumCUC;           
  Int_t mMaxLoops;         
  Int_t mMaxFastLoops;    
  Float_t mUnfoldLimit;      
  Float_t mUnfoldFailedLimit;
  
private:

Int_t FtpcClusterPars(St_ftpcClusterPars *det);
Int_t FtpcFastSimGas(St_ftpcFastSimGas *gaspar);
Int_t FtpcFastSimPars(St_ftpcFastSimPars *param);
Int_t FtpcSlowSimGas(St_ftpcSlowSimGas  *gas);
Int_t FtpcSlowSimPars(St_ftpcSlowSimPars *param);

public:
  // constructor used by StFtpcClusterMaker:
  StFtpcParamReader(St_ftpcClusterPars *det,
		    St_ftpcFastSimGas *gaspar,
                    St_ftpcFastSimPars *param);
  // constructor used by StFtpcSlowSimMaker:
  StFtpcParamReader(St_ftpcClusterPars *det,
                    St_ftpcSlowSimGas  *gas,
                    St_ftpcSlowSimPars *param);
  // constructor used by StFtpcCalibMaker:
  StFtpcParamReader(St_ftpcClusterPars *det);

  ~StFtpcParamReader();

  Int_t returnCode;

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
  Float_t gasTemperatureEast() {return mGasTemperatureEast;}
  Float_t gasTemperatureWest() {return mGasTemperatureWest;}
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
  Int_t maxNumSequences() {return mMaxNumSequences;}
  Int_t maxNumSeqPeaks() {return mMaxNumSeqPeaks;}
  Int_t maxNumPeaks() {return mMaxNumPeaks;}
  Int_t maxNumCUC() {return mMaxNumCUC;}
  Int_t maxLoops() {return mMaxLoops;}
  Int_t maxFastLoops() {return mMaxFastLoops;}
  Float_t unfoldLimit() {return mUnfoldLimit;}
  Float_t unfoldFailedLimit() {return mUnfoldFailedLimit;} 

};

#endif

