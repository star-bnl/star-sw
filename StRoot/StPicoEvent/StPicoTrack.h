#ifndef StPicoTrack_h
#define StPicoTrack_h

#include <cmath>

#include "TObject.h"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StarClassLibrary/SystemOfUnits.h"
#include "StEvent/StDcaGeometry.h"


class StMuTrack;

class StPicoTrack : public TObject
{
public:

  StPicoTrack();

  /// ctor. Note: primary track should be associated with the StPicoEvent::mPrimaryVertex
  StPicoTrack(StMuTrack const* globalTrack, StMuTrack const* primaryTrack,
     double magField, StThreeVectorD const& pVtx, StDcaGeometry const& dcaG);

  virtual ~StPicoTrack() {}

  virtual void Print(Char_t const* option = "") const;  ///< Print track info

  /// track id, copied from StMuTrack, StTrack
  Int_t   id() const;
  Float_t chi2() const;
  /// primary momentum, only if track is primary with selected vertex StPicoEvent::mPrimaryVertex
  StThreeVectorF pMom() const;
  /// global momentum at point of DCA to StPicoEvent::mPrimaryVertex
  StThreeVectorF gMom() const;
  Float_t gPt() const;
  Float_t gPtot() const;
  /// global momentum at point of DCA to pVtx, B should be in kilogauss
  StThreeVectorF gMom(StThreeVectorF const& pVtx, float B) const;
  /// origin at DCA to StPicoEvent::mPrimaryVertex
  StThreeVectorF origin() const;
  /// dca point to StPicoEvent::mPrimaryVertex in global coordinates
  StThreeVectorF dcaPoint() const;
  Short_t charge() const;
  Int_t   nHitsFit() const;
  Int_t   nHitsMax() const;
  Int_t   nHitsDedx() const;
  UInt_t  hftHitsMap() const;
  Float_t dEdx() const;
  Float_t dNdx() const;
  Float_t dNdxError() const;
  Float_t nSigmaPion() const;
  Float_t nSigmaKaon() const;
  Float_t nSigmaProton() const;
  Float_t nSigmaElectron() const;

  /// track topology map, seet StEvent/StTrackTopologyMap.cxx
  UInt_t  topologyMap(unsigned int) const;

  /// helix at point of DCA to StPicoEvent::mPrimaryVertex
  StPhysicalHelixD helix(float B) const;
  Bool_t hasPxl1Hit() const;
  Bool_t hasPxl2Hit() const;
  Bool_t hasIstHit() const;
  Bool_t hasSstHit() const;
  Bool_t isHft() const;
  Bool_t isHFTTrack() const;
  Bool_t hasHft4Layers() const;

  /** Checks whether this track is associated with a primary vertex. */
  Bool_t isPrimary() const;
  const Float_t* params() const     { return mPar; }
  const Float_t* sigmas() const  { return mSigma; }
  const Float_t* correlations() const  { return mCorr; }

  StDcaGeometry dcaGeometry() const;
  StPhysicalHelixD helix() const;

  // MTD pid traits
  void setBEmcPidTraitsIndex(Int_t index);
  void setBTofPidTraitsIndex(Int_t index);
  void setMtdPidTraitsIndex(Int_t index);

  Int_t bemcPidTraitsIndex() const;
  Int_t bTofPidTraitsIndex() const;
  Int_t mtdPidTraitsIndex() const;

protected:
  UShort_t mId;               // track Id, copied from StMuTrack, StTrack
  Float16_t mChi2;             // [0,1000]|
#if 0
  StThreeVectorF16 mPMomentum;  // primary momentum, (0.,0.,0.) if none
  StThreeVectorF16 mGMomentum;  // global momentum at point of DCA to StPicoEvent::mPrimaryVertex
  StThreeVectorF16 mOrigin;     // origin at dca to primary vertex
#else
  Float16_t mPMomentum[3]; // 
  Float16_t mGMomentum[3]; //
  Float16_t mOrigin[3]; //
#endif
  Float16_t  mDedx;             // [0,1000]| dEdx in KeV/cm.
  Float16_t  mDnDx;             //  [0,1000]|  fitted dN/dx
  Float16_t  mDnDxError;        // fitted dN/dx error
  Char_t   mNHitsFit;         // nHitsFit - TPC
  Char_t   mNHitsMax;         // nHitsMax - TPC
  UChar_t  mNHitsDedx;        // nHitsDedx - TPC
  Float16_t   mCharge;        // [-1,1,2]|
  Float16_t  mNSigmaPion;       //[-1000,1000]| nsigmaPi
  Float16_t  mNSigmaKaon;       //[-1000,1000]| nsigmaK 
  Float16_t  mNSigmaProton;     //[-1000,1000]| nsigmaP 
  Float16_t  mNSigmaElectron;   //[-1000,1000]| nsigmaE 
  UInt_t   mTopologyMap[2];   // Toplogy Map data0 and data1. See StEvent/StTrackTopologyMap.cxx

  // pidTraits
  Short_t  mBEmcPidTraitsIndex; // index of the EMC  pidTratis in the event
  Short_t  mBTofPidTraitsIndex; // index of the BTOF pidTratis in the event
  Short_t  mMtdPidTraitsIndex;  // index of the MTD  pidTratis in the event
  // dcaG
  Float16_t mPar[6];
  Float16_t mSigma[5];
  Float16_t mCorr[10]; //[-1,1,12]!
  ClassDef(StPicoTrack, 2)
};

inline void StPicoTrack::setBEmcPidTraitsIndex(Int_t index) { mBEmcPidTraitsIndex = (Short_t)index; }
inline void StPicoTrack::setBTofPidTraitsIndex(Int_t index) { mBTofPidTraitsIndex = (Short_t)index; }
inline void StPicoTrack::setMtdPidTraitsIndex(Int_t index) { mMtdPidTraitsIndex = (Short_t)index; }
inline Int_t   StPicoTrack::id() const { return mId; }
inline Float_t StPicoTrack::chi2() const { return mChi2; }
inline Float_t StPicoTrack::gPt() const { return gMom().perp(); }
inline Float_t StPicoTrack::gPtot() const { return gMom().mag(); }
inline StThreeVectorF  StPicoTrack::pMom() const { return StThreeVectorF(mPMomentum); }
inline StThreeVectorF  StPicoTrack::gMom() const { return StThreeVectorF( mGMomentum); }
inline StThreeVectorF  StPicoTrack::origin() const { return StThreeVectorF(mOrigin); }
inline StThreeVectorF  StPicoTrack::dcaPoint() const { return StThreeVectorF(mOrigin); }
inline Short_t StPicoTrack::charge() const { return static_cast<Short_t>(mCharge); }
inline Int_t   StPicoTrack::nHitsFit() const { return (mNHitsFit > 0) ? (Int_t)mNHitsFit : (Int_t)(-1 * mNHitsFit); }
inline Int_t   StPicoTrack::nHitsMax() const { return mNHitsMax; }
inline Int_t   StPicoTrack::nHitsDedx() const { return mNHitsDedx; }
inline UInt_t  StPicoTrack::hftHitsMap() const { return topologyMap(0) >> 1 & 0x7F; }
inline Float_t StPicoTrack::dEdx() const { return mDedx;}
inline Float_t StPicoTrack::dNdx() const { return mDnDx;}
inline Float_t StPicoTrack::dNdxError() const { return mDnDxError;}
inline Float_t StPicoTrack::nSigmaPion() const { return mNSigmaPion; }
inline Float_t StPicoTrack::nSigmaKaon() const { return mNSigmaKaon; }
inline Float_t StPicoTrack::nSigmaProton() const { return mNSigmaProton; }
inline Float_t StPicoTrack::nSigmaElectron() const { return mNSigmaElectron; }
inline UInt_t  StPicoTrack::topologyMap(unsigned int idx) const { return mTopologyMap[idx]; }
inline Int_t   StPicoTrack::bemcPidTraitsIndex() const { return mBEmcPidTraitsIndex; }
inline Int_t   StPicoTrack::bTofPidTraitsIndex() const { return mBTofPidTraitsIndex; }
inline Int_t   StPicoTrack::mtdPidTraitsIndex() const { return mMtdPidTraitsIndex; }
inline Bool_t  StPicoTrack::hasPxl1Hit() const { return hftHitsMap() >> 0 & 0x1; }
inline Bool_t  StPicoTrack::hasPxl2Hit() const { return hftHitsMap() >> 1 & 0x3; }
inline Bool_t  StPicoTrack::hasIstHit()  const { return hftHitsMap() >> 3 & 0x3; }
inline Bool_t  StPicoTrack::hasSstHit()  const { return hftHitsMap() >> 5 & 0x3; }
inline Bool_t  StPicoTrack::isHft() const { return hasPxl1Hit() && hasPxl2Hit() && (hasIstHit() || hasSstHit()); }
inline Bool_t  StPicoTrack::isHFTTrack() const { return isHft(); }
inline Bool_t  StPicoTrack::hasHft4Layers() const { return hasPxl1Hit() && hasPxl2Hit() && hasIstHit() && hasSstHit(); }

/**
 * The default "primary" momentum is (0, 0, 0) but it is expected to have
 * a non-zero length when the track is associated with a primary vertex.
 */
inline Bool_t StPicoTrack::isPrimary() const
{
  return pMom().magnitude() > 0;
}

/// Return the global momentum at the dca point to the pVtx (usually it is the primary vertex.   B - magnetic field from PicoEvent::bField()
inline StThreeVectorF StPicoTrack::gMom(StThreeVectorF const& pVtx, float const B) const
{
  StPhysicalHelixD gHelix = helix(B);
  return gHelix.momentumAt(gHelix.pathLength(pVtx), B * kilogauss);
}

inline StPhysicalHelixD StPicoTrack::helix(float const B) const
{
  return StPhysicalHelixD(gMom(), origin(), B * kilogauss, static_cast<float>(charge()));
}

#endif
