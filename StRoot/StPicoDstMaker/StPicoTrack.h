#ifndef StPicoTrack_hh
#define StPicoTrack_hh

#include <cmath>

class StMuTrack;
class StPicoDst;
class StDcaGeometry;

#include "TObject.h"
#include "StThreeVectorF.hh"
#include "TVector2.h"
#include <stdio.h>
#include <math.h>
#include "StEvent/StDcaGeometry.h"

// Macro to control EMC variables
#define EMCON 1

class StPicoTrack : public TObject {
 public:
  StPicoTrack();
  ~StPicoTrack();
  StPicoTrack(StMuTrack *, StMuTrack *, float, int, double, StDcaGeometry*);
  virtual void Print(const Char_t *option = "") const;  ///< Print track info
            
  Int_t   id() const             { return (Int_t)mId; }
  Float_t chi2() const           { return (Float_t)mChi2/1000.; }
  Float_t gPt() const;
  StThreeVectorF gMom() const;
  StThreeVectorF const & pMom() const    { return mPMomentum; }
  Short_t charge() const         { return (mNHitsFit>0) ? +1 : -1; }
  Int_t   nHitsFit() const       { return (mNHitsFit>0) ? (Int_t)mNHitsFit : (Int_t)(-1*mNHitsFit); }
  Int_t   nHitsDedx() const      { return (Int_t)mNHitsDedx; }
  Int_t   nHitsMapHFT() const    { return (Int_t)mNHitsMapHFT; }
  Int_t   firstTpcHitRow() const { return (Int_t)mFirstTpcHitRow; }
  Int_t   lastTpcHitRow() const  { return (Int_t)mLastTpcHitRow; }  
  Float_t dEdx() const           { return (Float_t)mDedx/1000.; }
  Float_t nSigmaPion() const     { return (Float_t)mNSigmaPion/100.; }
  Float_t nSigmaKaon() const     { return (Float_t)mNSigmaKaon/100.; }
  Float_t nSigmaProton() const   { return (Float_t)mNSigmaProton/100.; }
  Float_t nSigmaElectron() const { return (Float_t)mNSigmaElectron/100.; }

  const Float_t* params() const     { return mPar; }
  const Float_t* errMatrix() const  { return mErrMatrix; }

  StDcaGeometry dcaGeometry() const;
  StPhysicalHelixD helix() const;
  Bool_t isHFTTrack() const { return (mNHitsMapHFT>>0 & 0x1) && (mNHitsMapHFT>>1 & 0x3) && (mNHitsMapHFT>>3 & 0x3); }
          
  // pid traits
  void    setEmcPidTraitsIndex(Int_t index)  { mEmcPidTraitsIndex = (Short_t)index;  }
  Int_t   emcPidTraitsIndex() const          { return (Int_t)mEmcPidTraitsIndex;     }

  void    setBTofPidTraitsIndex(Int_t index) { mBTofPidTraitsIndex = (Short_t)index; }
  Int_t   bTofPidTraitsIndex() const         { return (Int_t)mBTofPidTraitsIndex;    }

  void    setMtdPidTraitsIndex(Int_t index)  { mMtdPidTraitsIndex = (Short_t)index;  }
  Int_t   mtdPidTraitsIndex() const          { return (Int_t)mMtdPidTraitsIndex;     }

 protected:
  UShort_t mId;               // track Id
  UShort_t mChi2;             // chi2*1000
  StThreeVectorF mPMomentum;  // primary momentum, (0.,0.,0.) if none
  UShort_t mDedx;             // dEdx*1000
  Char_t   mNHitsFit;         // q*nHitsFit
  UChar_t  mNHitsDedx;        // nHitsDedx
  Short_t  mNSigmaPion;       // nsigmaPi * 100
  Short_t  mNSigmaKaon;       // nsigmaK * 100
  Short_t  mNSigmaProton;     // nsigmaP * 100
  Short_t  mNSigmaElectron;   // nsigmaE * 100
  UChar_t  mNHitsMapHFT;      // the hit map in all HFT layers
  UChar_t  mFirstTpcHitRow;   // first tpc hit row number
  UChar_t  mLastTpcHitRow;    // last tpc hit row number
  
  // a copy of the StMuTrack::dcaGeometry() parameters
  Float_t  mPar[6];                                            
  Float_t  mErrMatrix[15];
  // pidTraits
  Short_t  mEmcPidTraitsIndex;  // index of the EMC  pidTratis in the event
  Short_t  mBTofPidTraitsIndex; // index of the BTOF pidTratis in the event
  Short_t  mMtdPidTraitsIndex;  // index of the MTD  pidTratis in the event

  friend class StPicoDst;

  ClassDef(StPicoTrack, 1)
};
inline Float_t StPicoTrack::gPt() const
{
  return 1./fabs(mPar[3]);
}

inline StThreeVectorF StPicoTrack::gMom() const
{
  float ptt = gPt();
  return StThreeVectorF(ptt*std::cos(mPar[2]),ptt*std::sin(mPar[2]),ptt*mPar[4]);
}

inline StDcaGeometry StPicoTrack::dcaGeometry() const
{
  StDcaGeometry a;
  a.set(mPar, mErrMatrix);
  return a;
}
      
inline StPhysicalHelixD StPicoTrack::helix() const
{
  return dcaGeometry().helix();
}        
#endif
