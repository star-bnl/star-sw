#ifndef STAR_StTrack
#define STAR_StTrack
#include "TNamed.h"
#include "St_dst_track_Table.h"
class StTrack : public TNamed {

private:
  Short_t mQualityBitmask; 
  Short_t mDetectorId;
  Short_t mNofPoints;
  Short_t mMaxNofPoints;
  Short_t mNofFitPoints;
  Short_t mCharge;
  Float_t xStart;
  Float_t yStart;
  Float_t zStart;
  Float_t mPsi;                  // azimuthal angle for pt at first point on track (degree)
  Float_t mTanl;
  Float_t mInvpT;
  Float_t mCovariantMatrix[15];
  Float_t mFirstPoint[3];
  Float_t mLastPoint[3];
  Float_t mLength;
  Float_t mImpact;
  Float_t mChiSquared[2];
  Short_t mDegreesOfFreedom;

public:
  StTrack(const Char_t  *name="");
  virtual ~StTrack() {}
  StTrack      &Assign(dst_track_st &);
  Float_t       GetPx() const { return mInvpT == 0 ? 0 : TMath::Cos(TMath::Pi()/180.*mPsi)/mInvpT; }
  Float_t       GetPy() const { return mInvpT == 0 ? 0 : TMath::Sin(TMath::Pi()/180.*mPsi)/mInvpT; }
  Float_t       GetPz() const { return mInvpT == 0 ? 0 : mTanl/mInvpT;}
  Float_t       GetpT() const { return mInvpT == 0 ? 0 : 1./mInvpT;}
  Float_t       GetImpact() const { return mImpact; }
  Int_t         GetCharge() const { return mCharge; }
  Int_t         GetNpoint() const { return mNofPoints; }
  StTrack      &operator=(dst_track_st & track);
  ClassDef(StTrack,1)  //A track 
};

#endif
