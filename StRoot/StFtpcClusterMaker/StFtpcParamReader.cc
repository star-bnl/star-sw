// $Id: StFtpcParamReader.cc,v 1.1 2000/08/03 14:39:00 hummler Exp $
//
// $Log: StFtpcParamReader.cc,v $
// Revision 1.1  2000/08/03 14:39:00  hummler
// Create param reader to keep parameter tables away from cluster finder and
// fast simulator. StFtpcClusterFinder now knows nothing about tables anymore!
//

#include "StFtpcParamReader.hh"

#include <math.h>
#include "StMessMgr.h"

StFtpcParamReader::StFtpcParamReader(St_fcl_ampoff *ampoff,
				     St_fcl_ampslope *ampslope,
				     St_fcl_timeoff *timeoff,
				     St_fcl_padtrans *padtrans,
				     St_fcl_det *det,
				     St_fcl_zrow *zrow,
				     St_ffs_gaspar *gaspar)
{
  // just copy calibration table start to pointers
  mAmplitudeOffset = (Float_t *) &(ampoff->GetTable()->offset);
  mAmplitudeSlope = (Float_t *) &(ampslope->GetTable()->slope);
  mTimeOffset = (Float_t *) &(timeoff->GetTable()->offset);
  mNumberOfCalibrationValues = ampoff->GetNRows();
  if(mNumberOfCalibrationValues == ampslope->GetNRows())
    mNumberOfCalibrationValues = ampslope->GetNRows();
  if(mNumberOfCalibrationValues == timeoff->GetNRows())
    mNumberOfCalibrationValues = timeoff->GetNRows();

  // padtrans table has to be copied to be accessible as separate arrays
  mNumberOfPadtransBins = padtrans->GetNRows();
  mNumberOfPadrowsPerSide = 10;
  mPadtransEField = new Float_t[mNumberOfPadtransBins];
  mPadtransVDrift = 
    new Float_t[mNumberOfPadrowsPerSide*mNumberOfPadtransBins];
  mPadtransDeflection = 
    new Float_t[mNumberOfPadrowsPerSide*mNumberOfPadtransBins];
  mPadtransdVDriftdP = 
    new Float_t[mNumberOfPadrowsPerSide*mNumberOfPadtransBins];
  mPadtransdDeflectiondP = 
    new Float_t[mNumberOfPadrowsPerSide*mNumberOfPadtransBins];
  Int_t i, j;
  fcl_padtrans_st *padtransTable = padtrans->GetTable();
  for(i=0; i<mNumberOfPadtransBins; i++)
    {
      mPadtransEField[i] = padtransTable[i].e;
      for(j=0; j<mNumberOfPadrowsPerSide; j++)
	{
	  mPadtransVDrift[j+mNumberOfPadrowsPerSide*i] = 
	    padtransTable[i].v[j];
	  mPadtransDeflection[j+mNumberOfPadrowsPerSide*i] = 
	    padtransTable[i].psi[j];
	  mPadtransdVDriftdP[j+mNumberOfPadrowsPerSide*i] = 
	    padtransTable[i].dv_dp[j];
	  mPadtransdDeflectiondP[j+mNumberOfPadrowsPerSide*i] = 
	    padtransTable[i].dpsi_dp[j];
	}
    }

  // det table exists only once, just copy
  fcl_det_st *detTable = det->GetTable();
  mFirstPadrowToSearch = detTable->firstrow  ;
  mLastPadrowToSearch = detTable->lastrow;
  mFirstSectorToSearch = detTable->firstsec;
  mLastSectorToSearch = detTable->lastsec;
  mNumberOfPadrows = detTable->n_rows;
  mNumberOfSectors = detTable->n_sectors;
  mNumberOfPads = detTable->n_pads;
  mNumberOfTimebins = detTable->n_bins;
  mGaussFittingFlags = detTable->usegauss;
  mMinimumClusterMaxADC = detTable->min_max_adc;
  mNumberOfDriftSteps = detTable->n_int_steps;
  mDirectionOfMagnetField = detTable->magfld;
  mSensitiveVolumeInnerRadius = detTable->r_in;
  mSensitiveVolumeOuterRadius = detTable->r_out;
  mRadiusTimesField = detTable->rad_times_field;
  mRadiansPerDegree = M_PI / 180;
  mMicrosecondsPerTimebin = detTable->timebin_size;
  mRadiansPerPad = detTable->rad_per_pad;
  mRadiansPerBoundary = detTable->rad_per_gap;
  mStandardPressure = detTable->p_standard;
  mNormalizedNowPressure = detTable->p_normalized;
  mOrderOfDiffusionErrors = 3;
  mPadDiffusionErrors = (Float_t *) detTable->pad_err_diff;
  mTimeDiffusionErrors = (Float_t *) detTable->time_err_diff;
  mPadBadFitError = detTable->pad_err_bad;
  mTimeBadFitError = detTable->time_err_bad;
  mPadUnfoldError = detTable->pad_err_unfold;
  mTimeUnfoldError = detTable->time_err_unfold;
  mPadFailedFitError = detTable->pad_err_failed;
  mTimeFailedFitError = detTable->time_err_failed;
  mPadCutoffClusterError = detTable->pad_err_cutoff;
  mTimeCutoffClusterError = detTable->time_err_cutoff;
  mPadSaturatedClusterError = detTable->pad_err_sat;
  mTimeSaturatedClusterError = detTable->time_err_sat;
  m2PadWeightedError = detTable->pad_err_2mean;
  m2PadGaussError = detTable->pad_err_2gauss;
  m3PadWeightedError = detTable->pad_err_3mean;
  m3PadGaussError = detTable->pad_err_3gauss;
  mZDirectionError = detTable->z_err;

  //  just copy zrow table start to pointer
  mPadrowZPosition = (Float_t *) &(zrow->GetTable()->z);

  //  just copy gaspar array pointers  
  mOrderOfFastEstimates = 4;
  mVDriftEstimates = (Float_t *) &(gaspar->GetTable()->vdrift);
  mTDriftEstimates = (Float_t *) &(gaspar->GetTable()->tdrift);
  mSigmaRadialEstimates = (Float_t *) &(gaspar->GetTable()->sig_rad);
  mSigmaAzimuthalEstimates = (Float_t *) &(gaspar->GetTable()->sig_azi);
  mErrorRadialEstimates = (Float_t *) &(gaspar->GetTable()->err_rad);
  mErrorAzimuthalEstimates = (Float_t *) &(gaspar->GetTable()->err_azi);

//   cout << "StFtpcParamReader constructed" << endl;  
}

StFtpcParamReader::~StFtpcParamReader()
{
  delete[] mPadtransEField;
  delete[] mPadtransVDrift;
  delete[] mPadtransDeflection;
  delete[] mPadtransdVDriftdP;
  delete[] mPadtransdDeflectiondP;

//   cout << "StFtpcParamReader destructed" << endl;
}

Float_t StFtpcParamReader::amplitudeOffset(Int_t i)
{
  if(i>=0 && i<mNumberOfCalibrationValues)
    {
      return mAmplitudeOffset[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: amplitudeOffset index out of range, using 0", "W", "OST");
      return mAmplitudeOffset[0];
    }
}

Float_t StFtpcParamReader::amplitudeSlope(Int_t i)
{
  if(i>=0 && i<mNumberOfCalibrationValues)
    {
      return mAmplitudeSlope[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: amplitudeSlope index out of range, using 0", "W", "OST");
      return mAmplitudeSlope[0];
    }
}

Float_t StFtpcParamReader::timeOffset(Int_t i)
{
  if(i>=0 && i<mNumberOfCalibrationValues)
    {
      return mTimeOffset[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: timeOffset index out of range, using 0", "W", "OST");
      return mTimeOffset[0];
    }
}

Float_t StFtpcParamReader::padtransEField(Int_t i) 
{
  if(i>=0 && i<mNumberOfPadtransBins)
    {
      return mPadtransEField[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: padtransEField index out of range, using 0", "W", "OST");
      return mPadtransEField[0];
    }
}

Float_t StFtpcParamReader::padtransVDrift(Int_t i, Int_t padrow) 
{
  if(i>=0 && i<mNumberOfPadtransBins && padrow>=0 && padrow<mNumberOfPadrowsPerSide)
    {
      return mPadtransVDrift[padrow+mNumberOfPadrowsPerSide*i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: padtransVDrift index out of range, using 0", "W", "OST");
      return mPadtransVDrift[0];
    }
}

Float_t StFtpcParamReader::padtransDeflection(Int_t i, Int_t padrow) 
{
  if(i>=0 && i<mNumberOfPadtransBins && padrow>=0 && padrow<mNumberOfPadrowsPerSide)
    {
      return mPadtransDeflection[padrow+mNumberOfPadrowsPerSide*i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: padtransDeflection index out of range, using 0", "W", "OST");
      return mPadtransDeflection[0];
    }
}

Float_t StFtpcParamReader::padtransdVDriftdP(Int_t i, Int_t padrow) 
{
  if(i>=0 && i<mNumberOfPadtransBins && padrow>=0 && padrow<mNumberOfPadrowsPerSide)
    {
      return mPadtransdVDriftdP[padrow+mNumberOfPadrowsPerSide*i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: padtransdVDriftdP index out of range, using 0", "W", "OST");
      return mPadtransdVDriftdP[0];
    }
}

Float_t StFtpcParamReader::padtransdDeflectiondP(Int_t i, Int_t padrow) 
{
  if(i>=0 && i<mNumberOfPadtransBins && padrow>=0 && padrow<mNumberOfPadrowsPerSide)
    {
      return mPadtransdDeflectiondP[padrow+mNumberOfPadrowsPerSide*i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: padtransdDeflectiondP index out of range, using 0", "W", "OST");
      return mPadtransdDeflectiondP[0];
    }
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

Float_t StFtpcParamReader::padrowZPosition(Int_t i) 
{
  if(i>=0 && i<mNumberOfPadrows)
    {
      return mPadrowZPosition[i];
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: padrowZPosition index out of range, using 0", "W", "OST");
      return mPadrowZPosition[0];
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
