// $Id: StFtpcParamReader.cc,v 1.14 2001/03/19 15:52:48 jcs Exp $
//
// $Log: StFtpcParamReader.cc,v $
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

#include <math.h>
#include "StMessMgr.h"

StFtpcParamReader::StFtpcParamReader(St_fcl_det *det,
				     St_ffs_gaspar *gaspar)
{

  mNumberOfMagboltzBins = 761;

  // det table exists only once, just copy
  fcl_det_st *detTable = det->GetTable();
  mDetTable = detTable; // copy to data member to write parameters back
  mGaussFittingFlags = detTable->usegauss;
  mMinimumClusterMaxADC = detTable->min_max_adc;
  mNumberOfDriftSteps = detTable->n_int_steps;
  mDirectionOfMagnetField = detTable->magfld;
  mRadiusTimesField = detTable->rad_times_field;
  mRadiansPerDegree = degree;
  mStandardPressure = detTable->p_standard;
  mNormalizedNowPressure = detTable->p_normalized;
  mTZero = detTable->t_zero;
  mLorentzAngleFactor = detTable->angle_factor;
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

  //  temporarily set Ftpc fast simulator parameters until in data base
  mFtpcWestGeantVolumeId = 100;
  mFtpcEastGeantVolumeId = 200;
  mUnfoldedClusterFlag = 1;
  mBadShapeClusterFlag = 8;
  mMergedClusterFlag = 1000;
  mNumberOfPadsDedxSmearing = 4;
  mNumberOfBinsDedxSmearing = 3;
  mRadiusTolerance = 0.25;
  mSigmaSpacingFactor = 2.5;
  mAdcConversionFactor = 8000000.0;
  mClusterChargeConversionFactor = 6;
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
				     St_fcl_det *det)
{
  // fss gas table has to be copied to be accessible as separate arrays
  mNumberOfFssGasValues = gas->GetNRows();
  mFssGasEField = new Float_t[mNumberOfFssGasValues];
  mFssGasVDrift = new Float_t[mNumberOfFssGasValues];
  mFssGasDiffusionX = new Float_t[mNumberOfFssGasValues];
  mFssGasDiffusionY = new Float_t[mNumberOfFssGasValues];
  mFssGasDiffusionZ = new Float_t[mNumberOfFssGasValues];
  mFssGasLorentzAngle = new Float_t[mNumberOfFssGasValues];
  Int_t i;
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
  mSigmaPadResponseFuntion = paramTable->readout_sigma_prf;
  mReadoutShaperTime = paramTable->readout_shaper_time;
 
  mNumberOfMagboltzBins = 761;

  // det table exists only once, just copy
  fcl_det_st *detTable = det->GetTable();
  mDetTable = detTable; // copy to data member to write parameters back
  mGaussFittingFlags = detTable->usegauss;
  mMinimumClusterMaxADC = detTable->min_max_adc;
  mNumberOfDriftSteps = detTable->n_int_steps;
  mDirectionOfMagnetField = detTable->magfld;
  mRadiusTimesField = detTable->rad_times_field;
  mRadiansPerDegree = degree;
  mStandardPressure = detTable->p_standard;
  mNormalizedNowPressure = detTable->p_normalized;
  mTZero = detTable->t_zero;
  mLorentzAngleFactor = detTable->angle_factor;
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

//   cout << "StFtpcParamReader constructed from StFtpcSlowSimMaker tables" << endl;  
}



StFtpcParamReader::~StFtpcParamReader()
{

  // write back det table entries that have set functions:
  mDetTable->p_normalized = mNormalizedNowPressure;

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

