#ifndef StKinkLocalTrack_hh
#define StKinkLocalTrack_hh
// $Id: StKinkLocalTrack.hh,v 1.5 1999/09/24 01:23:35 fisyak Exp $
// $Log: StKinkLocalTrack.hh,v $
// Revision 1.5  1999/09/24 01:23:35  fisyak
// Reduced Include Path
//
// Revision 1.4  1999/08/26 17:30:51  wdeng
// Use shorter names for identifiers
//
// Revision 1.3  1999/07/08 19:09:51  fisyak
// Add tabs, remove St_glb_Maker
//

#include <iostream.h>
#include "TMath.h"
#include "TObject.h"
#include "StPhysicalHelixD.hh"
#include "StThreeVectorD.hh"
#include "dst_track.h"

#if !defined(ST_NO_NAMESPACES) 
using namespace std;
#endif

class StKinkLocalTrack:public TObject {
public:
  StKinkLocalTrack();
  StKinkLocalTrack(dst_track_st* trk, Float_t curvature, Float_t dip, Float_t phase,
		   StThreeVectorD origin, Int_t h);
  ~StKinkLocalTrack();
  // StKinkLocalTrack(const StKinkLocalTrack&);                  use default
  // const StKinkLocalTrack& operator=(const StKinkLocalTrack&); use default
  
  Int_t  Compare(TObject *obj);
  Bool_t IsSortable() const { return 1; }
  Bool_t IsEqual(TObject *obj);
  
  StPhysicalHelixD& helix();
  Int_t   Id() const;
  Int_t   DetId() const;
  Int_t   numOfHits() const;
  Int_t   charge() const;  
  Float_t pt() const;  
  Float_t startPoint(Int_t i) const;
  Float_t lastPoint(Int_t i) const;  
  Float_t endRadius2D() const;
  Float_t startRadius2D() const;
  
protected:
  StPhysicalHelixD mHelix;  
  Int_t mId;
  Int_t mDetId;
  Int_t mNumOfHits;
  Int_t mCharge;
  Float_t mPt;  
  Float_t mStartPoint[3];  
  Float_t mLastPoint[3];  
  Float_t mEndRadius2D;
  Float_t mStartRadius2D;
  
private:
};

inline StPhysicalHelixD& StKinkLocalTrack::helix() { return mHelix; }
inline Int_t StKinkLocalTrack::Id() const { return mId; }
inline Int_t StKinkLocalTrack::DetId() const { return mDetId; }
inline Int_t StKinkLocalTrack::numOfHits() const { return mNumOfHits; }
inline Int_t StKinkLocalTrack::charge() const { return mCharge; }
inline Float_t StKinkLocalTrack::pt() const { return mPt; }
inline Float_t StKinkLocalTrack::startPoint(Int_t i) const { return mStartPoint[i]; }
inline Float_t StKinkLocalTrack::lastPoint(Int_t i) const { return mLastPoint[i]; }
inline Float_t StKinkLocalTrack::endRadius2D() const { return mEndRadius2D; }
inline Float_t StKinkLocalTrack::startRadius2D() const { return mStartRadius2D; }

#endif
