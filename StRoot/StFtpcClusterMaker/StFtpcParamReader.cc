// $Id: StFtpcParamReader.cc,v 1.6 2000/11/06 13:42:57 hummler Exp $
//
// $Log: StFtpcParamReader.cc,v $
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
  // store padtrans table for writing back in destructor
  mPadtransTable=padtransTable;
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
  mMinimumDriftField = detTable->start_field;
  mStepSizeDriftField = detTable->step_field;
  mDvdpCalcOffset = detTable->dvdp_off;
  mBaseTemperature = detTable->temperature;
  mPercentAr = detTable->percent_ar;
  mPercentCO2 = detTable->percent_co2;
  mPercentNe = detTable->percent_ne;
  mPercentHe = detTable->percent_he;

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

StFtpcParamReader::StFtpcParamReader(St_fss_gas *gas,
				     St_fss_param *param,
				     St_fcl_padtrans *padtrans,
				     St_fcl_det *det,
				     St_fcl_zrow *zrow)
{
  // fss gas table has to be copied to be accessible as separate arrays
  mNumberOfFssGasValues = gas->GetNRows();
  mFssGasEField = new Float_t[mNumberOfFssGasValues];
  mFssGasVDrift = new Float_t[mNumberOfFssGasValues];
  mFssGasDiffusionX = new Float_t[mNumberOfFssGasValues];
  mFssGasDiffusionY = new Float_t[mNumberOfFssGasValues];
  mFssGasDiffusionZ = new Float_t[mNumberOfFssGasValues];
  mFssGasLorentzAngle = new Float_t[mNumberOfFssGasValues];
  Int_t i,j;
  fss_gas_st *gasTable = gas->GetTable();
  for(i=0; i<mNumberOfFssGasValues; i++)
    {
      mFssGasEField[i] = gasTable[i].efield;
      mFssGasVDrift[i] = gasTable[i].velocity_z;
      mFssGasDiffusionX[i] = gasTable[i].diffusion_x;
      mFssGasDiffusionY[i] = gasTable[i].diffusion_y;
      mFssGasDiffusionZ[i] = gasTable[i].diffusion_z;
      mFssGasLorentzAngle[i] = gasTable[i].angle_lorentz;
    }

  // param table exists only once, just copy
  fss_param_st *paramTable = param->GetTable();
  mRandomNumberGenerator = paramTable->random_number_gen;
  mZeroSuppressThreshold = paramTable->adc_threshold;
  mNumSlowSimGridPoints = paramTable->n_grid_points;
  mMaxAdc = paramTable->max_adc;
  mGaussIntegrationSteps = paramTable->n_int_steps;
  mDiffusionCoarseness = paramTable->diff_coarse;
  mAdcConversion = paramTable->adc_conversion;
  mSimulationPhiStart = paramTable->chamber_phi_min;
  mSimulationPhiEnd = paramTable->chamber_phi_max;
  mChamberCathodeVoltage = paramTable->chamber_cath_voltage;
  mGasGain = paramTable->gas_gas_gain;
  mGasAttenuation = paramTable->gas_attenuation;
  mGasIonizationPotential = paramTable->gas_avg_ion_pot;
  mPadLength = paramTable->readout_pad_length;
  mSigmaPadResponseFuntion = paramTable->readout_sigma_prf;
  mReadoutShaperTime = paramTable->readout_shaper_time;
 
  
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
  fcl_padtrans_st *padtransTable = padtrans->GetTable();
  // store padtrans table for writing back in destructor
  mPadtransTable=padtransTable;
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
  mMinimumDriftField = detTable->start_field;
  mStepSizeDriftField = detTable->step_field;
  mDvdpCalcOffset = detTable->dvdp_off;
  mBaseTemperature = detTable->temperature;
  mPercentAr = detTable->percent_ar;
  mPercentCO2 = detTable->percent_co2;
  mPercentNe = detTable->percent_ne;
  mPercentHe = detTable->percent_he;

  //  just copy zrow table start to pointer
  mPadrowZPosition = (Float_t *) &(zrow->GetTable()->z);

//   cout << "StFtpcParamReader constructed from StFtpcSlowSimMaker tables" << endl;  
}



StFtpcParamReader::~StFtpcParamReader()
{
  // write padtrans array back to table
  Int_t i,j;
  if(mPadtransTable!=NULL)
    {
      for(i=0; i<mNumberOfPadtransBins; i++)
	{
	  mPadtransTable[i].e = mPadtransEField[i];
	  for(j=0; j<mNumberOfPadrowsPerSide; j++)
	    {
	      mPadtransTable[i].v[j] =
		mPadtransVDrift[j+mNumberOfPadrowsPerSide*i]; 
	      mPadtransTable[i].psi[j] =
		mPadtransDeflection[j+mNumberOfPadrowsPerSide*i]; 
	      mPadtransTable[i].dv_dp[j] =
		mPadtransdVDriftdP[j+mNumberOfPadrowsPerSide*i]; 
	      mPadtransTable[i].dpsi_dp[j] =
		mPadtransdDeflectiondP[j+mNumberOfPadrowsPerSide*i]; 
	    }
	}
    }

  delete[] mPadtransEField;
  delete[] mPadtransVDrift;
  delete[] mPadtransDeflection;
  delete[] mPadtransdVDriftdP;
  delete[] mPadtransdDeflectiondP;
  delete[] mFssGasEField;
  delete[] mFssGasVDrift;
  delete[] mFssGasDiffusionX;
  delete[] mFssGasDiffusionY;
  delete[] mFssGasDiffusionZ;
  delete[] mFssGasLorentzAngle;

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

Int_t StFtpcParamReader::setPadtransEField(Int_t i, Float_t newvalue) 
{
  if(i>=0 && i<mNumberOfPadtransBins)
    {
      mPadtransEField[i]=newvalue;
      return 1;
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: padtransEField index out of range, not changed", "W", "OST");
      return 0;
    }
}

Int_t StFtpcParamReader::setPadtransVDrift(Int_t i, Int_t padrow, Float_t newvalue) 
{
  if(i>=0 && i<mNumberOfPadtransBins && padrow>=0 && padrow<mNumberOfPadrowsPerSide)
    {
      mPadtransVDrift[padrow+mNumberOfPadrowsPerSide*i]=newvalue;
      return 1;
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: padtransVDrift index out of range, not changed", "W", "OST");
      return 0;
    }
}

Int_t StFtpcParamReader::setPadtransDeflection(Int_t i, Int_t padrow, Float_t newvalue) 
{
  if(i>=0 && i<mNumberOfPadtransBins && padrow>=0 && padrow<mNumberOfPadrowsPerSide)
    {
      mPadtransDeflection[padrow+mNumberOfPadrowsPerSide*i]=newvalue;
      return 1;
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: padtransDeflection index out of range, not changed", "W", "OST");
      return 0;
    }
}

Int_t StFtpcParamReader::setPadtransdVDriftdP(Int_t i, Int_t padrow, Float_t newvalue) 
{
  if(i>=0 && i<mNumberOfPadtransBins && padrow>=0 && padrow<mNumberOfPadrowsPerSide)
    {
      mPadtransdVDriftdP[padrow+mNumberOfPadrowsPerSide*i]=newvalue;
      return 1;
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: padtransdVDriftdP index out of range, not changed", "W", "OST");
      return 0;
    }
}

Int_t StFtpcParamReader::setPadtransdDeflectiondP(Int_t i, Int_t padrow, Float_t newvalue) 
{
  if(i>=0 && i<mNumberOfPadtransBins && padrow>=0 && padrow<mNumberOfPadrowsPerSide)
    {
      mPadtransdDeflectiondP[padrow+mNumberOfPadrowsPerSide*i]=newvalue;
      return 1;
    }
  else
    {
      gMessMgr->Message("StFtpcParamReader: padtransdDeflectiondP index out of range, not changed", "W", "OST");
      return 0;
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

