// $Id: StFtpcParamReader.hh,v 1.1 2000/08/03 14:39:01 hummler Exp $
//
// $Log: StFtpcParamReader.hh,v $
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

#define TRUE 1
#define FALSE 0

class StFtpcParamReader : public TObject 
{
  
protected:
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
  Float_t *mPadrowZPosition;
  Int_t mOrderOfFastEstimates;
  Float_t *mVDriftEstimates;
  Float_t *mTDriftEstimates;
  Float_t *mSigmaRadialEstimates;
  Float_t *mSigmaAzimuthalEstimates;
  Float_t *mErrorRadialEstimates;
  Float_t *mErrorAzimuthalEstimates;
  
public:
  StFtpcParamReader(St_fcl_ampoff *ampoff,
		    St_fcl_ampslope *ampslope,
		    St_fcl_timeoff *timeoff,
		    St_fcl_padtrans *padtrans,
		    St_fcl_det *det,
		    St_fcl_zrow *zrow,
		    St_ffs_gaspar *gaspar);
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
};

#endif
