// $Id: StFtpcParamReader.cc,v 1.20 2002/03/01 14:22:20 jcs Exp $
//
// $Log: StFtpcParamReader.cc,v $
// Revision 1.20  2002/03/01 14:22:20  jcs
// add additional histograms to monitor cluster finding
//
// Revision 1.18  2002/01/21 22:14:56  jcs
// added values for temperature/pressure calculations to ftpcClusterPars
//
// Revision 1.17  2001/07/11 21:19:31  jcs
// remove obsolete entries in tables
//
// Revision 1.16  2001/04/24 07:11:39  oldi
// Float_t mSlowSimPressure introduced to replace mNormalizedNowPressure in
// StFtpcSlowSimMaker.
//
// Revision 1.15  2001/04/02 12:10:26  jcs
// get FTPC calibrations,geometry from MySQL database and code parameters
// from StarDb/ftpc
//
// Revision 1.14  2001/03/19 15:52:48  jcs
// use ftpcDimensions from database
//
// Revision 1.13  2001/03/06 23:34:12  jcs
// use database instead of params
//
// Revision 1.12  2001/01/27 20:07:46  jcs
// change name of parameter
//
// Revision 1.11  2001/01/15 16:08:41  jcs
// get phiOrigin and phiPerSector fro ftpcDimensions
//
// Revision 1.10  2001/01/08 17:07:09  jcs
// move remaining constants from code to database
//
// Revision 1.9  2000/12/11 16:39:07  jcs
// move FTPC geant volume id and cluster flags from code to parameter reader
//
// Revision 1.8  2000/11/27 14:09:20  hummler
// implement tzero and lorentz angle correction factor
//
// Revision 1.7  2000/11/14 13:08:26  hummler
// add charge step calculation, minor cleanup
//
// Revision 1.6  2000/11/06 13:42:57  hummler
// include latest changes in second constructor as well
//
// Revision 1.3  2000/10/31 09:52:13  hummler
// add parameters for slow simulator
//
// Revision 1.2  2000/09/18 14:26:50  hummler
// expand StFtpcParamReader to supply data for slow simulator as well
// introduce StFtpcGeantReader to separate g2t tables from simulator code
// implement StFtpcGeantReader in StFtpcFastSimu
//
// Revision 1.1  2000/08/03 14:39:00  hummler
// Create param reader to keep parameter tables away from cluster finder and
// fast simulator. StFtpcClusterFinder now knows nothing about tables anymore!
//

#include "StFtpcParamReader.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"

#include <math.h>
#include "StMessMgr.h"

StFtpcParamReader::StFtpcParamReader(St_ftpcClusterPars *det,
				     St_ftpcFastSimGas *gaspar,
                                     St_ftpcFastSimPars *param)
{

  mStandardPressure = atmosphere/(100*pascal);

  // ftpcClusterPars table exists only once, just copy
  ftpcClusterPars_st *detTable = det->GetTable();
  if(detTable){
    mClusterParsTable = detTable; // copy to data member to write parameters back
    mGaussFittingFlags = detTable->gaussFittingFlags;
    mMinimumClusterMaxADC = detTable->minimumClusterMaxADC;
    mNumberOfDriftSteps = detTable->numberOfDriftSteps;
    mOrderOfDiffusionErrors = detTable->orderOfDiffusionErrors;
    mPadDiffusionErrors = (Float_t *) detTable->padDiffusionErrors;
    mTimeDiffusionErrors = (Float_t *) detTable->timeDiffusionErrors;
    mLorentzAngleFactor = detTable->lorentzAngleFactor;
    mPadBadFitError = detTable->padBadFitError;
    mTimeBadFitError = detTable->timeBadFitError;
    mPadUnfoldError = detTable->padUnfoldError;
    mTimeUnfoldError = detTable->timeUnfoldError;
    mPadFailedFitError = detTable->padFailedFitError;
    mTimeFailedFitError = detTable->timeFailedFitError;
    mPadCutoffClusterError = detTable->padCutoffClusterError;
    mTimeCutoffClusterError = detTable->timeCutoffClusterError;
    mPadSaturatedClusterError = detTable->padSaturatedClusterError;
    mTimeSaturatedClusterError = detTable->timeSaturatedClusterError;
    m2PadWeightedError = detTable->twoPadWeightedError;
    m2PadGaussError = detTable->twoPadGaussError;
    m3PadWeightedError = detTable->threePadWeightedError;
    m3PadGaussError = detTable->threePadGaussError;
    mZDirectionError = detTable->zDirectionError;
    mNormalizedNowPressure = detTable->normalizedNowPressure;
    mAdjustedAirPressureWest = detTable->adjustedAirPressureWest;
    mAdjustedAirPressureEast = detTable->adjustedAirPressureEast;
    mMinPressure = detTable->minPressure;
    mMaxPressure = detTable->maxPressure;
    mGasTemperatureWest = detTable->gasTemperatureWest;
    mGasTemperatureEast = detTable->gasTemperatureEast;
    mMinGasTemperature = detTable->minGasTemperature;
    mMaxGasTemperature = detTable->maxGasTemperature;
    mMaxNumSequences = detTable->maxNumSequences ; 
    mMaxNumSeqPeaks = detTable->maxNumSeqPeaks;
    mMaxNumPeaks = detTable->maxNumPeaks;          
    mMaxNumCUC = detTable->maxNumCUC;           
    mMaxLoops = detTable->maxLoops;         
    mMaxFastLoops = detTable->maxFastLoops;    
    mUnfoldLimit = detTable->unfoldLimit;      
    mUnfoldFailedLimit = detTable->unfoldFailedLimit;
    mMaxTimeLength = detTable->maxTimelength;
    mMaxPadLength = detTable->maxPadlength;
    mMinTimeBin = detTable->minTimebin;

  } else {
    gMessMgr->Message( " No data in table class St_ftpcClusterPars","E");
  }

  // ftpcFastSimPars table exists only once, just copy
  ftpcFastSimPars_st *paramTable = param->GetTable();
  if(paramTable){
     mFtpcWestGeantVolumeId         = paramTable->ftpcWestGeantVolumeId;
     mFtpcEastGeantVolumeId         = paramTable->ftpcEastGeantVolumeId; 
     mUnfoldedClusterFlag           = paramTable->unfoldedClusterFlag;
     mBadShapeClusterFlag           = paramTable->badShapeClusterFlag;
     mMergedClusterFlag             = paramTable->mergedClusterFlag;
     mNumberOfPadsDedxSmearing      = paramTable->numberOfPadsDedxSmearing; 
     mNumberOfBinsDedxSmearing      = paramTable->numberOfBinsDedxSmearing;
     mRadiusTolerance               = paramTable->radiusTolerance; 
     mSigmaSpacingFactor            = paramTable->sigmaSpacingFactor;
     mAdcConversionFactor           = paramTable->adcConversionFactor;
     mClusterChargeConversionFactor = paramTable->clusterChargeConversionFactor;
  } else {
    gMessMgr->Message( " No data in table class St_ftpcFastSimPars","E");
  }

  //  just copy gaspar array pointers  
  ftpcFastSimGas_st *gasTable = gaspar->GetTable();
  if(gasTable){
      mOrderOfFastEstimates =  gasTable->orderOfFastEstimates;
      mVDriftEstimates = (Float_t *) &(gaspar->GetTable()->driftVelocityEstimates);
      mTDriftEstimates = (Float_t *) &(gaspar->GetTable()->driftTimeEstimates);
      mSigmaRadialEstimates = (Float_t *) &(gaspar->GetTable()->sigmaRadialEstimates);
      mSigmaAzimuthalEstimates = (Float_t *) &(gaspar->GetTable()->sigmaAzimuthalEstimates);
      mErrorRadialEstimates = (Float_t *) &(gaspar->GetTable()->errorRadialEstimates);
      mErrorAzimuthalEstimates = (Float_t *) &(gaspar->GetTable()->errorAzimuthalEstimates);
  } else {
    gMessMgr->Message( " No data in table class St_ftpcFastSimGas","E");
  }

  // create empty dummy of fss gas table, to keep destructor uniform
  mNumberOfFssGasValues = 0;
  mFssGasEField = new Float_t[1];
  mFssGasVDrift = new Float_t[1];
  mFssGasDiffusionX = new Float_t[1];
  mFssGasDiffusionY = new Float_t[1];
  mFssGasDiffusionZ = new Float_t[1];
  mFssGasLorentzAngle = new Float_t[1];

//   cout << "StFtpcParamReader constructed from StFtpcClusterMaker tables" << endl;  
}

StFtpcParamReader::StFtpcParamReader(St_ftpcClusterPars *det,
                                     St_ftpcSlowSimGas  *gas, 
                                     St_ftpcSlowSimPars *param)
{

  mStandardPressure = atmosphere/(100*pascal);

  // slow simulator gas table has to be copied to be accessible as separate arrays
  mNumberOfFssGasValues = gas->GetNRows();
  mFssGasEField = new Float_t[mNumberOfFssGasValues];
  mFssGasVDrift = new Float_t[mNumberOfFssGasValues];
  mFssGasDiffusionX = new Float_t[mNumberOfFssGasValues];
  mFssGasDiffusionY = new Float_t[mNumberOfFssGasValues];
  mFssGasDiffusionZ = new Float_t[mNumberOfFssGasValues];
  mFssGasLorentzAngle = new Float_t[mNumberOfFssGasValues];
  Int_t i;
  ftpcSlowSimGas_st *gasTable = gas->GetTable();
  for(i=0; i<mNumberOfFssGasValues; i++)
    {
      mFssGasEField[i] = gasTable[i].electricField;
      mFssGasVDrift[i] = gasTable[i].driftVelocity;
      mFssGasDiffusionX[i] = gasTable[i].diffusionX;
      mFssGasDiffusionY[i] = gasTable[i].diffusionY;
      mFssGasDiffusionZ[i] = gasTable[i].diffusionZ;
      mFssGasLorentzAngle[i] = gasTable[i].lorentzAngle;
    }

  // ftpcSlowSimPars table exists only once, just copy
  ftpcSlowSimPars_st *paramTable = param->GetTable();
  if(paramTable){
     mRandomNumberGenerator = paramTable->randomNumberGenerator;
     mZeroSuppressThreshold = paramTable->zeroSuppressThreshold;
     mNumSlowSimGridPoints = paramTable->numSlowSimGridPoints;
     mMaxAdc = paramTable->maxAdc;
     mGaussIntegrationSteps = paramTable->numGaussIntSteps;
     mDiffusionCoarseness = paramTable->diffusionCoarseness;
     mAdcConversion = paramTable->adcConversion;
     mChamberCathodeVoltage = paramTable->chamberCathodeVoltage;
     mSigmaPadResponseFuntion = paramTable->sigmaPadResponseFuntion;
     mReadoutShaperTime = paramTable->shaperTime;
     mSlowSimPressure = paramTable->slowSimPressure;
  } else {
    gMessMgr->Message( " No data in table class St_ftpcSlowSimPars","E");
  }
 
  mStandardPressure = atmosphere/(100*pascal);

  // ftpcClusterPars table exists only once, just copy
  ftpcClusterPars_st *detTable = det->GetTable();
  if(detTable){
    mClusterParsTable = detTable; // copy to data member to write parameters back
    mGaussFittingFlags = detTable->gaussFittingFlags;
    mMinimumClusterMaxADC = detTable->minimumClusterMaxADC;
    mNumberOfDriftSteps = detTable->numberOfDriftSteps;
    mOrderOfDiffusionErrors = detTable->orderOfDiffusionErrors;
    mPadDiffusionErrors = (Float_t *) detTable->padDiffusionErrors;
    mTimeDiffusionErrors = (Float_t *) detTable->timeDiffusionErrors;
    mLorentzAngleFactor = detTable->lorentzAngleFactor;
    mPadBadFitError = detTable->padBadFitError;
    mTimeBadFitError = detTable->timeBadFitError;
    mPadUnfoldError = detTable->padUnfoldError;
    mTimeUnfoldError = detTable->timeUnfoldError;
    mPadFailedFitError = detTable->padFailedFitError;
    mTimeFailedFitError = detTable->timeFailedFitError;
    mPadCutoffClusterError = detTable->padCutoffClusterError;
    mTimeCutoffClusterError = detTable->timeCutoffClusterError;
    mPadSaturatedClusterError = detTable->padSaturatedClusterError;
    mTimeSaturatedClusterError = detTable->timeSaturatedClusterError;
    m2PadWeightedError = detTable->twoPadWeightedError;
    m2PadGaussError = detTable->twoPadGaussError;
    m3PadWeightedError = detTable->threePadWeightedError;
    m3PadGaussError = detTable->threePadGaussError;
    mZDirectionError = detTable->zDirectionError;
    mNormalizedNowPressure = detTable->normalizedNowPressure;
    mAdjustedAirPressureWest = detTable->adjustedAirPressureWest;
    mAdjustedAirPressureEast = detTable->adjustedAirPressureEast;
    mMaxNumSequences = detTable->maxNumSequences ; 
    mMaxNumSeqPeaks = detTable->maxNumSeqPeaks;
    mMaxNumPeaks = detTable->maxNumPeaks;          
    mMaxNumCUC = detTable->maxNumCUC;           
    mMaxLoops = detTable->maxLoops;         
    mMaxFastLoops = detTable->maxFastLoops;    
    mUnfoldLimit = detTable->unfoldLimit;      
    mUnfoldFailedLimit = detTable->unfoldFailedLimit;
    mMaxTimeLength = detTable->maxTimelength;
    mMaxPadLength = detTable->maxPadlength;
    mMinTimeBin = detTable->minTimebin;

  } else {
    gMessMgr->Message( " No data in table class St_ftpcClusterPars","E");
  }

//   cout << "StFtpcParamReader constructed from StFtpcSlowSimMaker tables" << endl;  
}



StFtpcParamReader::~StFtpcParamReader()
{

  // write back clusterpars table entries that have set functions:
  mClusterParsTable->normalizedNowPressure = mNormalizedNowPressure;
  mClusterParsTable->adjustedAirPressureWest = mAdjustedAirPressureWest;
  mClusterParsTable->adjustedAirPressureEast = mAdjustedAirPressureEast;
  mClusterParsTable->gasTemperatureWest = mGasTemperatureWest;
  mClusterParsTable->gasTemperatureEast = mGasTemperatureEast;

  // delete allocated memory
  delete[] mFssGasEField;
  delete[] mFssGasVDrift;
  delete[] mFssGasDiffusionX;
  delete[] mFssGasDiffusionY;
  delete[] mFssGasDiffusionZ;
  delete[] mFssGasLorentzAngle;

//   cout << "StFtpcParamReader destructed" << endl;
}

Float_t StFtpcParamReader::padDiffusionErrors(Int_t i) 
{
  if(i>=0 && i<mOrderOfDiffusionErrors)
    {
      return mPadDiffusionErrors[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: padDiffusionErrors index out of range, using 0", "W", "OST");
      return mPadDiffusionErrors[0];
    }
}

Float_t StFtpcParamReader::timeDiffusionErrors(Int_t i) 
{
  if(i>=0 && i<mOrderOfDiffusionErrors)
    {
      return mTimeDiffusionErrors[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: timeDiffusionErrors index out of range, using 0", "W", "OST");
      return mTimeDiffusionErrors[0];
    }
}

Float_t StFtpcParamReader::vDriftEstimates(Int_t i) 
{
  if(i>=0 && i<mOrderOfFastEstimates)
    {
      return mVDriftEstimates[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: vDriftEstimates index out of range, using 0", "W", "OST");
      return mVDriftEstimates[0];
    }
}

Float_t StFtpcParamReader::tDriftEstimates(Int_t i) 
{
  if(i>=0 && i<mOrderOfFastEstimates)
    {
      return mTDriftEstimates[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: tDriftEstimates index out of range, using 0", "W", "OST");
      return mTDriftEstimates[0];
    }
}

Float_t StFtpcParamReader::sigmaRadialEstimates(Int_t i) 
{
  if(i>=0 && i<mOrderOfFastEstimates)
    {
      return mSigmaRadialEstimates[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: sigmaRadialEstimates index out of range, using 0", "W", "OST");
      return mSigmaRadialEstimates[0];
    }
}

Float_t StFtpcParamReader::sigmaAzimuthalEstimates(Int_t i) 
{
  if(i>=0 && i<mOrderOfFastEstimates)
    {
      return mSigmaAzimuthalEstimates[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: sigmaAzimuthalEstimates index out of range, using 0", "W", "OST");
      return mSigmaAzimuthalEstimates[0];
    }
}

Float_t StFtpcParamReader::errorRadialEstimates(Int_t i) 
{
  if(i>=0 && i<mOrderOfFastEstimates)
    {
      return mErrorRadialEstimates[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: errorRadialEstimates index out of range, using 0", "W", "OST");
      return mErrorRadialEstimates[0];
    }
}

Float_t StFtpcParamReader::errorAzimuthalEstimates(Int_t i) 
{
  if(i>=0 && i<mOrderOfFastEstimates)
    {
      return mErrorAzimuthalEstimates[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: errorAzimuthalEstimates index out of range, using 0", "W", "OST");
      return mErrorAzimuthalEstimates[0];
    }
}

Float_t StFtpcParamReader::fssGasEField(Int_t i)
{
  if(i>=0 && i<mNumberOfFssGasValues)
    {
      return mFssGasEField[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: fssGasEField index out of range, using 0", "W", "OST");
      return mFssGasEField[0];
    }
}

Float_t StFtpcParamReader::fssGasVDrift(Int_t i)
{
  if(i>=0 && i<mNumberOfFssGasValues)
    {
      return mFssGasVDrift[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: fssGasVDrift index out of range, using 0", "W", "OST");
      return mFssGasVDrift[0];
    }
}

Float_t StFtpcParamReader::fssGasDiffusionX(Int_t i)
{
  if(i>=0 && i<mNumberOfFssGasValues)
    {
      return mFssGasDiffusionX[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: fssGasDiffusionX index out of range, using 0", "W", "OST");
      return mFssGasDiffusionX[0];
    }
}

Float_t StFtpcParamReader::fssGasDiffusionY(Int_t i)
{
  if(i>=0 && i<mNumberOfFssGasValues)
    {
      return mFssGasDiffusionY[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: fssGasDiffusionY index out of range, using 0", "W", "OST");
      return mFssGasDiffusionY[0];
    }
}

  Float_t StFtpcParamReader::fssGasDiffusionZ(Int_t i)
{
  if(i>=0 && i<mNumberOfFssGasValues)
    {
      return mFssGasDiffusionZ[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: fssGasDiffusionZ index out of range, using 0", "W", "OST");
      return mFssGasDiffusionZ[0];
    }
}

  Float_t StFtpcParamReader::fssGasLorentzAngle(Int_t i)
{
  if(i>=0 && i<mNumberOfFssGasValues)
    {
      return mFssGasLorentzAngle[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: fssGasLorentzAngle index out of range, using 0", "W", "OST");
      return mFssGasLorentzAngle[0];
    }
}

