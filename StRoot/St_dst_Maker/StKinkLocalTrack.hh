#ifndef StKinkLocalTrack_hh
#define StKinkLocalTrack_hh

#include <iostream.h>
#include "TMath.h"
#include "TObject.h"
#include "StPhysicalHelixD.hh"
#include "StThreeVectorD.hh"
#include "tables/dst_track.h"

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
  Int_t getId() const;
  Int_t getDetId() const;
  Int_t getNumOfHits() const;
  Int_t getCharge() const;  
  Float_t getPt() const;  
  Float_t getStartPoint(Int_t i) const;
  Float_t getLastPoint(Int_t i) const;  
  Float_t getEndRadius2D() const;
  Float_t getStartRadius2D() const;

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
inline Int_t StKinkLocalTrack::getId() const { return mId; }
inline Int_t StKinkLocalTrack::getDetId() const { return mDetId; }
inline Int_t StKinkLocalTrack::getNumOfHits() const { return mNumOfHits; }
inline Int_t StKinkLocalTrack::getCharge() const { return mCharge; }
inline Float_t StKinkLocalTrack::getPt() const { return mPt; }
inline Float_t StKinkLocalTrack::getStartPoint(Int_t i) const { return mStartPoint[i]; }
inline Float_t StKinkLocalTrack::getLastPoint(Int_t i) const { return mLastPoint[i]; }
inline Float_t StKinkLocalTrack::getEndRadius2D() const { return mEndRadius2D; }
inline Float_t StKinkLocalTrack::getStartRadius2D() const { return mStartRadius2D; }

#endif
