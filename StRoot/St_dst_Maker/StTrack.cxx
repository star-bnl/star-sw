#include "TRandom.h"
#include "StTrack.h"
 
ClassImp(StTrack)
 
 
//______________________________________________________________________________
StTrack::StTrack(const Char_t  *name) : TNamed()
{
  // Create a track object.
  if (name && strlen(name)) SetName(name);
  mQualityBitmask = 0;
  mDetectorId     = 0;
  mNofPoints      = 0;
  mMaxNofPoints   = 0;
  mNofFitPoints   = 0;
  mCharge         = 0;
  xStart          = 0;
  yStart          = 0;
  zStart          = 0;
  mPsi            = 0;
  mTanl           = 0;
  mInvpT          = 0;
  Int_t i;
  for (i = 0; i< 15; i++){mCovariantMatrix[i] = 0;}
  for (i = 0; i< 3; i++){
    mFirstPoint[i]= 0;
    mLastPoint[i] = 0;
  }
  mLength         = 0;
  mImpact         = 0;
  mChiSquared[0]  = 0;
  mChiSquared[1]  = 0;

  mDegreesOfFreedom = 0;

}
//______________________________________________________________________________
StTrack &StTrack::Assign(dst_track_st & track)
{
  // Fill an StTrack object.
  mQualityBitmask = track.iflag;  // bitmask of e.g. quality information
  mDetectorId     = track.det_id;
  mNofPoints      = track.n_point;
  mMaxNofPoints   = track.n_max_point;
  mNofFitPoints   = track.n_fit_point;
  mCharge         = track.icharge;
  xStart          = track.x0;     
  yStart          = track.y0;     
  zStart          = track.z0;     
  mPsi            = track.psi;
  mTanl           = track.tanl;
  mInvpT          = track.invpt;
  Int_t i;
  for (i = 0; i< 5; i++){mCovariantMatrix[i*(i+1)/2] = track.covar_diag[i];}
  for (i = 0; i< 3; i++){
    mFirstPoint[i]= track.x_first[i];
    mLastPoint[i] = track.x_last[i];
  }
  mLength         = track.length;
  mImpact         = track.impact;
  mChiSquared[0]  = track.chisq[0];
  mChiSquared[1]  = track.chisq[1];
  mDegreesOfFreedom = track.ndegf;
  return *this;
}
//______________________________________________________________________________
StTrack &StTrack::operator=(dst_track_st & track)
{
   Assign(track);
   return *this;
}
//______________________________________________________________________________
