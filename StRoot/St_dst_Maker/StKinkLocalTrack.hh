#ifndef StKinkLocalTrack_hh
#define StKinkLocalTrack_hh
// $Id: StKinkLocalTrack.hh,v 1.10 2004/06/02 15:02:45 cmironov Exp $
// $Log: StKinkLocalTrack.hh,v $
// Revision 1.10  2004/06/02 15:02:45  cmironov
// Naming clash solved by changing names
//
// Revision 1.9  2003/09/07 03:49:07  perev
// gcc 3.2 + WarnOff
//
// Revision 1.8  2003/09/02 17:59:26  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.7  2001/04/09 19:56:55  wdeng
// Added 'const' to Compare function signature
//
// Revision 1.6  2000/02/02 21:37:36  lbarnby
// CC5
//
// Revision 1.5  1999/09/24 01:23:35  fisyak
// Reduced Include Path
//
// Revision 1.4  1999/08/26 17:30:51  wdeng
// Use shorter names for identifiers
//
// Revision 1.3  1999/07/08 19:09:51  fisyak
// Add tabs, remove St_glb_Maker
//

#include <Stiostream.h>
#include "TMath.h"
#include "TObject.h"
#include "StPhysicalHelixD.hh"
#include "StThreeVectorD.hh"
#include "dst_track.h"


class StOldKinkLocalTrack:public TObject {
public:
  StOldKinkLocalTrack();
  StOldKinkLocalTrack(dst_track_st* trk, Float_t curvature, Float_t dip, Float_t phase,
		   StThreeVectorD origin, Int_t h);
  ~StOldKinkLocalTrack();
  // StOldKinkLocalTrack(const StOldKinkLocalTrack&);                  use default
  // const StOldKinkLocalTrack& operator=(const StOldKinkLocalTrack&); use default
  
  Int_t  Compare(const TObject *obj) const;
  Bool_t IsSortable() const { return 1; }
  Bool_t IsEqual(const TObject *obj) const;
  
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

inline StPhysicalHelixD& StOldKinkLocalTrack::helix() { return mHelix; }
inline Int_t StOldKinkLocalTrack::Id() const { return mId; }
inline Int_t StOldKinkLocalTrack::DetId() const { return mDetId; }
inline Int_t StOldKinkLocalTrack::numOfHits() const { return mNumOfHits; }
inline Int_t StOldKinkLocalTrack::charge() const { return mCharge; }
inline Float_t StOldKinkLocalTrack::pt() const { return mPt; }
inline Float_t StOldKinkLocalTrack::startPoint(Int_t i) const { return mStartPoint[i]; }
inline Float_t StOldKinkLocalTrack::lastPoint(Int_t i) const { return mLastPoint[i]; }
inline Float_t StOldKinkLocalTrack::endRadius2D() const { return mEndRadius2D; }
inline Float_t StOldKinkLocalTrack::startRadius2D() const { return mStartRadius2D; }

#endif
