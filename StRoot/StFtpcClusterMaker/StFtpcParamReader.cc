// $Id: StFtpcParamReader.cc,v 1.30 2007/01/26 12:46:06 jcs Exp $
//
// $Log: StFtpcParamReader.cc,v $
// Revision 1.30  2007/01/26 12:46:06  jcs
// replace //LOG_INFO with LOG_DEBUG
//
// Revision 1.29  2007/01/15 07:49:22  jcs
// replace printf, cout and gMesMgr with Logger
//
// Revision 1.28  2006/03/13 19:26:59  jcs
// add constructor StFtpcCalibMaker
//
// Revision 1.27  2004/01/28 01:41:15  jeromel
// Change OST to OS everywhere since defaultoption is now not to print
// the date.
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
// Revision 1.23  2003/05/07 15:10:26  putschke
// improvements for cathode offset corretions
//
// Revision 1.22  2003/04/15 11:36:11  putschke
// Include corrections for inner cathode offset and move some parameter to database
//
// Revision 1.21  2003/02/27 22:47:40  jcs
// make the temperature and pressure parameters available to the Ftpc slow simulator
// (needed for embedding)
//
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

// For StFtpcClusterMaker
StFtpcParamReader::StFtpcParamReader(St_ftpcClusterPars *det,
				     St_ftpcFastSimGas *gaspar,
                                     St_ftpcFastSimPars *param)
{

  mStandardPressure = atmosphere/(100*pascal);

  returnCode = kStOK;
  returnCode += FtpcClusterPars(det);
  returnCode += FtpcFastSimGas(gaspar);
  returnCode += FtpcFastSimPars(param);

  // create empty dummy of fss gas table, to keep destructor uniform
  mNumberOfFssGasValues = 0;
  mFssGasEField = new Float_t[1];
  mFssGasVDrift = new Float_t[1];
  mFssGasDiffusionX = new Float_t[1];
  mFssGasDiffusionY = new Float_t[1];
  mFssGasDiffusionZ = new Float_t[1];
  mFssGasLorentzAngle = new Float_t[1];

  LOG_DEBUG << "StFtpcParamReader constructed from StFtpcClusterMaker tables" << endm;  
}

// For StFtpcSlowSimMaker
StFtpcParamReader::StFtpcParamReader(St_ftpcClusterPars *det,
                                     St_ftpcSlowSimGas  *gas, 
                                     St_ftpcSlowSimPars *param)
{

  mStandardPressure = atmosphere/(100*pascal);

  returnCode = kStOK;
  returnCode += FtpcClusterPars(det);
  returnCode += FtpcSlowSimGas(gas);
  returnCode += FtpcSlowSimPars(param);

  LOG_DEBUG << "StFtpcParamReader constructed from StFtpcSlowSimMaker tables" << endm;  
}

// For StFtpcCalibMaker
StFtpcParamReader::StFtpcParamReader(St_ftpcClusterPars *det)
{

  returnCode = kStOK;
  returnCode += FtpcClusterPars(det);

  // create empty dummy of fss gas table, to keep destructor uniform
  mNumberOfFssGasValues = 0;
  mFssGasEField = new Float_t[1];
  mFssGasVDrift = new Float_t[1];
  mFssGasDiffusionX = new Float_t[1];
  mFssGasDiffusionY = new Float_t[1];
  mFssGasDiffusionZ = new Float_t[1];
  mFssGasLorentzAngle = new Float_t[1];
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

  LOG_DEBUG << "StFtpcParamReader destructed" << endm;
}

//===============================================================

Int_t StFtpcParamReader::FtpcClusterPars(St_ftpcClusterPars *det)
{

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
    mGasTemperatureWest = detTable->gasTemperatureWest;
    mGasTemperatureEast = detTable->gasTemperatureEast;
    mMaxNumSequences = detTable->maxNumSequences ; 
    mMaxNumSeqPeaks = detTable->maxNumSeqPeaks;
    mMaxNumPeaks = detTable->maxNumPeaks;          
    mMaxNumCUC = detTable->maxNumCUC;           
    mMaxLoops = detTable->maxLoops;         
    mMaxFastLoops = detTable->maxFastLoops;    
    mUnfoldLimit = detTable->unfoldLimit;      
    mUnfoldFailedLimit = detTable->unfoldFailedLimit;
    return kStOK;
  } else {
    LOG_ERROR << "No data in table class St_ftpcClusterPars" << endm;
    return kStERR;
  }
}

//----------------------------------------------------------------------------

Int_t StFtpcParamReader::FtpcFastSimGas(St_ftpcFastSimGas *gaspar)
{
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
      return kStOK;
  } else {
    LOG_ERROR << "No data in table class St_ftpcFastSimGas" << endm;
    return kStERR;
  }
}

//----------------------------------------------------------------------------

Int_t StFtpcParamReader::FtpcFastSimPars(St_ftpcFastSimPars *param)
{
  // ftpcFastSimPars table exists only once, just copy
  ftpcFastSimPars_st *paramTable = param->GetTable();
  if(paramTable){
     mUnfoldedClusterFlag           = paramTable->unfoldedClusterFlag;
     mBadShapeClusterFlag           = paramTable->badShapeClusterFlag;
     mMergedClusterFlag             = paramTable->mergedClusterFlag;
     mNumberOfPadsDedxSmearing      = paramTable->numberOfPadsDedxSmearing; 
     mNumberOfBinsDedxSmearing      = paramTable->numberOfBinsDedxSmearing;
     mRadiusTolerance               = paramTable->radiusTolerance; 
     mSigmaSpacingFactor            = paramTable->sigmaSpacingFactor;
     mAdcConversionFactor           = paramTable->adcConversionFactor;
     mClusterChargeConversionFactor = paramTable->clusterChargeConversionFactor;
     return kStOK;
  } else {
    LOG_ERROR << "No data in table class St_ftpcFastSimPars" << endm;
    return kStERR;
  }
}

//----------------------------------------------------------------------------

Int_t StFtpcParamReader::FtpcSlowSimGas(St_ftpcSlowSimGas  *gas)
{
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
  if (gasTable) {
    for(i=0; i<mNumberOfFssGasValues; i++)
      {
        mFssGasEField[i] = gasTable[i].electricField;
        mFssGasVDrift[i] = gasTable[i].driftVelocity;
        mFssGasDiffusionX[i] = gasTable[i].diffusionX;
        mFssGasDiffusionY[i] = gasTable[i].diffusionY;
        mFssGasDiffusionZ[i] = gasTable[i].diffusionZ;
        mFssGasLorentzAngle[i] = gasTable[i].lorentzAngle;
      }
     return kStOK;
   }
   else {
     LOG_ERROR << "No data in table class St_ftpcSlowSimGas" << endm;
     return kStERR; 
   }
}

//----------------------------------------------------------------------------

Int_t StFtpcParamReader::FtpcSlowSimPars(St_ftpcSlowSimPars *param)
{
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
     return kStOK;
  } else {
    LOG_ERROR << "No data in table class St_ftpcSlowSimPars" << endm;
    return kStERR;
  }
}

//===============================================================


Float_t StFtpcParamReader::padDiffusionErrors(Int_t i) 
{
  if(i>=0 && i<mOrderOfDiffusionErrors)
    {
      return mPadDiffusionErrors[i];
    }
  else
    {
      LOG_WARN << "StFtpcParamReader: padDiffusionErrors index out of range, using 0" << endm;
      return mPadDiffusionErrors[0];
    }
}

//----------------------------------------------------------------------------

Float_t StFtpcParamReader::timeDiffusionErrors(Int_t i) 
{
  if(i>=0 && i<mOrderOfDiffusionErrors)
    {
      return mTimeDiffusionErrors[i];
    }
  else
    {
      LOG_WARN << "StFtpcParamReader: timeDiffusionErrors index out of range, using 0" << endm;
      return mTimeDiffusionErrors[0];
    }
}

//----------------------------------------------------------------------------

Float_t StFtpcParamReader::vDriftEstimates(Int_t i) 
{
  if(i>=0 && i<mOrderOfFastEstimates)
    {
      return mVDriftEstimates[i];
    }
  else
    {
      LOG_WARN << "StFtpcParamReader: vDriftEstimates index out of range, using 0" << endm;
      return mVDriftEstimates[0];
    }
}

//----------------------------------------------------------------------------

Float_t StFtpcParamReader::tDriftEstimates(Int_t i) 
{
  if(i>=0 && i<mOrderOfFastEstimates)
    {
      return mTDriftEstimates[i];
    }
  else
    {
      LOG_WARN << "StFtpcParamReader: tDriftEstimates index out of range, using 0" << endm;
      return mTDriftEstimates[0];
    }
}

//----------------------------------------------------------------------------

Float_t StFtpcParamReader::sigmaRadialEstimates(Int_t i) 
{
  if(i>=0 && i<mOrderOfFastEstimates)
    {
      return mSigmaRadialEstimates[i];
    }
  else
    {
      LOG_WARN << "StFtpcParamReader: sigmaRadialEstimates index out of range, using 0" << endm;
      return mSigmaRadialEstimates[0];
    }
}

//----------------------------------------------------------------------------

Float_t StFtpcParamReader::sigmaAzimuthalEstimates(Int_t i) 
{
  if(i>=0 && i<mOrderOfFastEstimates)
    {
      return mSigmaAzimuthalEstimates[i];
    }
  else
    {
      LOG_WARN << "StFtpcParamReader: sigmaAzimuthalEstimates index out of range, using 0" << endm;
      return mSigmaAzimuthalEstimates[0];
    }
}

//----------------------------------------------------------------------------

Float_t StFtpcParamReader::errorRadialEstimates(Int_t i) 
{
  if(i>=0 && i<mOrderOfFastEstimates)
    {
      return mErrorRadialEstimates[i];
    }
  else
    {
      LOG_WARN << "StFtpcParamReader: errorRadialEstimates index out of range, using 0" << endm;
      return mErrorRadialEstimates[0];
    }
}

//----------------------------------------------------------------------------

Float_t StFtpcParamReader::errorAzimuthalEstimates(Int_t i) 
{
  if(i>=0 && i<mOrderOfFastEstimates)
    {
      return mErrorAzimuthalEstimates[i];
    }
  else
    {
      LOG_WARN << "StFtpcParamReader: errorAzimuthalEstimates index out of range, using 0" << endm;
      return mErrorAzimuthalEstimates[0];
    }
}

//----------------------------------------------------------------------------

Float_t StFtpcParamReader::fssGasEField(Int_t i)
{
  if(i>=0 && i<mNumberOfFssGasValues)
    {
      return mFssGasEField[i];
    }
  else
    {
      LOG_WARN << "StFtpcParamReader: fssGasEField index out of range, using 0" << endm;
      return mFssGasEField[0];
    }
}

//----------------------------------------------------------------------------

Float_t StFtpcParamReader::fssGasVDrift(Int_t i)
{
  if(i>=0 && i<mNumberOfFssGasValues)
    {
      return mFssGasVDrift[i];
    }
  else
    {
      LOG_WARN << "StFtpcParamReader: fssGasVDrift index out of range, using 0" << endm;
      return mFssGasVDrift[0];
    }
}

//----------------------------------------------------------------------------

Float_t StFtpcParamReader::fssGasDiffusionX(Int_t i)
{
  if(i>=0 && i<mNumberOfFssGasValues)
    {
      return mFssGasDiffusionX[i];
    }
  else
    {
      LOG_WARN << "StFtpcParamReader: fssGasDiffusionX index out of range, using 0" << endm;
      return mFssGasDiffusionX[0];
    }
}

//----------------------------------------------------------------------------

Float_t StFtpcParamReader::fssGasDiffusionY(Int_t i)
{
  if(i>=0 && i<mNumberOfFssGasValues)
    {
      return mFssGasDiffusionY[i];
    }
  else
    {
      LOG_WARN << "StFtpcParamReader: fssGasDiffusionY index out of range, using 0" << endm;
      return mFssGasDiffusionY[0];
    }
}

//----------------------------------------------------------------------------

  Float_t StFtpcParamReader::fssGasDiffusionZ(Int_t i)
{
  if(i>=0 && i<mNumberOfFssGasValues)
    {
      return mFssGasDiffusionZ[i];
    }
  else
    {
      LOG_WARN << "StFtpcParamReader: fssGasDiffusionZ index out of range, using 0" << endm;
      return mFssGasDiffusionZ[0];
    }
}

//----------------------------------------------------------------------------

  Float_t StFtpcParamReader::fssGasLorentzAngle(Int_t i)
{
  if(i>=0 && i<mNumberOfFssGasValues)
    {
      return mFssGasLorentzAngle[i];
    }
  else
    {
      LOG_WARN << "StFtpcParamReader: fssGasLorentzAngle index out of range, using 0" << endm;
      return mFssGasLorentzAngle[0];
    }
}


