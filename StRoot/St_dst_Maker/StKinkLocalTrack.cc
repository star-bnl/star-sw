#include "StKinkLocalTrack.hh"

StKinkLocalTrack::StKinkLocalTrack():
  mHelix(0, 0, 0, StThreeVectorD()) 
{
  mId = 0;
  mDetId = 0;
  mNumOfHits = 0;
  mCharge = 0;
  mPt =0.;  
  mStartPoint[0]=0.;
  mStartPoint[1]=0.;
  mStartPoint[2]=0.;
  mLastPoint[0]=0.;
  mLastPoint[1]=0.;
  mLastPoint[2]=0.;
  mEndRadius2D = 0.;
  mStartRadius2D = 0.;
}

StKinkLocalTrack::StKinkLocalTrack(dst_track_st* trk, Float_t curvature, Float_t dip,
	       Float_t phase, StThreeVectorD origin, Int_t h) :
  mHelix(curvature, dip, phase, origin, h)
 {
  mId = trk->id;
  mDetId = trk->det_id;
  mNumOfHits = trk->n_fit_point;
  mCharge = trk->icharge;
  mPt = 1./trk->invpt;  
  mStartPoint[0]=trk->x0;
  mStartPoint[1]=trk->y0;
  mStartPoint[2]=trk->z0;
  mLastPoint[0]=trk->x_last[0];
  mLastPoint[1]=trk->x_last[1];
  mLastPoint[2]=trk->x_last[2];
  mEndRadius2D = sqrt(trk->x_last[0]*trk->x_last[0] + trk->x_last[1]*trk->x_last[1]);
  mStartRadius2D = sqrt(trk->x0*trk->x0 + trk->y0*trk->y0);
 }

StKinkLocalTrack::~StKinkLocalTrack() {
}


Int_t StKinkLocalTrack::Compare(TObject *obj)
{
  if( mStartRadius2D == ((StKinkLocalTrack*)obj)->mStartRadius2D ) return 0;
  if( mStartRadius2D <  ((StKinkLocalTrack*)obj)->mStartRadius2D ) return -1;
  if( mStartRadius2D >  ((StKinkLocalTrack*)obj)->mStartRadius2D ) return 1;
}

Bool_t StKinkLocalTrack::IsEqual(TObject *obj)
{
   if (this == obj) return 1;
   if (StKinkLocalTrack::Class() != obj->IsA()) return 0;
   return mStartRadius2D == ((StKinkLocalTrack*)obj)->mStartRadius2D;
}
