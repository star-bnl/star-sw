#ifndef StPicoTrack_h
#define StPicoTrack_h

/// C++ headers
#include <cmath>

/// ROOT headers
#include "TObject.h"
#include "TVector3.h"

/// PicoDst headers
#include "StPicoHelix.h"
#include "StPicoPhysicalHelix.h"

#if defined (_VANILLA_ROOT_)
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#else
#include "StarClassLibrary/SystemOfUnits.h"
#include "StarClassLibrary/PhysicalConstants.h"
#endif

//_________________
class StPicoTrack : public TObject {

 public:
  /// Default constructor
  StPicoTrack();
  /// Copy constructor
  StPicoTrack(const StPicoTrack &track);
  /// Destructor
  virtual ~StPicoTrack();
  /// Print track parameters
  virtual void Print(const Char_t *option = "") const;

  /**
   * Getters
   */

  /// Track id, copied from StMuTrack, StTrack
  Int_t   id() const;
  Float_t chi2() const;
  /// Primary momentum, only if track is primary with selected
  /// vertex StPicoEvent::mPrimaryVertex
  TVector3 pMom() const;
  /// Global momentum at point of DCA to StPicoEvent::mPrimaryVertex
  TVector3 gMom() const;
  /// Origin at DCA to StPicoEvent::mPrimaryVertex
  TVector3 origin() const;
  Float_t gPt() const;
  Float_t gPtot() const;
  Float_t pPt() const;
  Float_t pPtot() const;
  /// Global momentum at point of DCA to pVtx, B should be in kilogauss
  TVector3 gMom(TVector3 pVtx, Float_t B) const;
  /// Helix at point of DCA to StPicoEvent::mPrimaryVertex
  StPicoPhysicalHelix helix(Float_t const B) const;

  /// Next functions return DCA (or its components) of the global track
  /// to the point with coordinates (pVtxX, pVtxY, pVtxZ)
  Float_t gDCAx(Float_t pVtxX) const;
  Float_t gDCAy(Float_t pVtxY) const;
  Float_t gDCAz(Float_t pVtxZ) const;
  Float_t gDCAxy(Float_t pVtxX, Float_t pVtxY) const;
  Float_t gDCA(Float_t pVtxX, Float_t pVtxY, Float_t pVtxZ) const;
  TVector3 gDCA(TVector3 pVtx) const;
  /// Charge of the track is encoded in nHitsFit as: nHitsFit * charge
  Short_t charge() const;
  Int_t   nHits() const;
  Int_t   nHitsFit() const;
  Int_t   nHitsMax() const;
  Int_t   nHitsDedx() const;
  UInt_t  hftHitsMap() const;
  Float_t dEdx() const;
  Float_t dEdxError() const;
  //Float_t dNdx() const;
  //Float_t dNdxError() const;
  Float_t nSigmaPion() const;
  Float_t nSigmaKaon() const;
  Float_t nSigmaProton() const;
  Float_t nSigmaElectron() const;

  /// Track topology map, seet StEvent/StTrackTopologyMap.cxx
  UInt_t  topologyMap(UInt_t) const;

  Bool_t hasPxl1Hit() const;
  Bool_t hasPxl2Hit() const;
  Bool_t hasIstHit() const;
  Bool_t hasSstHit() const;
  Bool_t isHft() const;
  Bool_t isHFTTrack() const;
  Bool_t hasHft4Layers() const;
  Bool_t isTofTrack() const;
  Bool_t isBemcTrack() const;
  Bool_t isMtdTrack() const;

  /// Checks whether this track is associated with a primary vertex
  Bool_t isPrimary() const;

  /// Return index to the corresponding track 
  Int_t bemcPidTraitsIndex() const;
  Int_t bTofPidTraitsIndex() const;
  Int_t mtdPidTraitsIndex() const;

  /**
   * Setters
   */

  void setId(Int_t id);       
  void setChi2(Float_t chi2);
  void setPrimaryMomentum(Double_t px, Double_t py, Double_t pz);
  void setPrimaryMomentum(Float_t px, Float_t py, Float_t pz);
  void setPrimaryMomentum(TVector3 mom);
  void setGlobalMomentum(Double_t px, Double_t py, Double_t pz);
  void setGlobalMomentum(Float_t px, Float_t py, Float_t pz);
  void setGlobalMomentum(TVector3 mom);
  void setOrigin(Double_t x, Double_t y, Double_t z);
  void setOrigin(Float_t x, Float_t y, Float_t z);
  void setOrigin(TVector3 origin);
  
  void setDedx(Float_t dEdx);
  void setDedxError(Float_t dEdxError);
  /// nhits MUST be (charge * nHitsFit) !!!
  void setNHitsFit(Int_t nhits);     
  void setNHitsPossible(Int_t nhits);
  void setNHitsMax(Int_t nhits);
  void setNHitsDedx(Int_t nhits);
  void setNSigmaPion(Float_t ns);
  void setNSigmaKaon(Float_t ns);
  void setNSigmaProton(Float_t ns);
  void setNSigmaElectron(Float_t ns);
  void setTopologyMap(Int_t id, UInt_t word);

  void setBEmcPidTraitsIndex(Int_t index);
  void setBTofPidTraitsIndex(Int_t index);
  void setMtdPidTraitsIndex(Int_t index);

 protected:

  /// Track Id, copied from StMuTrack, StTrack
  UShort_t mId;
  /// chi2*1000
  UShort_t mChi2;
  /// Px, Py and Pz of the primary track (0. if not primary)
  Float_t mPMomentumX;        
  Float_t mPMomentumY;
  Float_t mPMomentumZ;
  /// Px, Py and Pz of the global track at DCA to primary vertex
  Float_t mGMomentumX;
  Float_t mGMomentumY;
  Float_t mGMomentumZ;
  /// x, y and z of the global track origin at DCA to primary vertex
  Float_t mOriginX;
  Float_t mOriginY;
  Float_t mOriginZ;
  
  /// dEdx in KeV/cm (dE/dx * 1e6)
  Float16_t  mDedx;
  Float16_t  mDedxError;
  //Float_t  mDnDx;             // fitted dN/dx
  //Float_t  mDnDxError;        // fitted dN/dx error
  /// Charge * nHitsFit
  Char_t   mNHitsFit;
  /// nHitsMax (in TPC)
  UChar_t  mNHitsMax;
  /// nHitsDedx (in TPC)
  UChar_t  mNHitsDedx;
  /// nsigmaPi * 1000
  Short_t  mNSigmaPion;
  /// nsigmaK * 1000
  Short_t  mNSigmaKaon;
  /// nsigmaP * 1000
  Short_t  mNSigmaProton;
  /// nsigmaE * 1000
  Short_t  mNSigmaElectron;
  /// Toplogy Map data0 and data1. See StEvent/StTrackTopologyMap.cxx
  UInt_t   mTopologyMap[2];

  /// Indices to pidTraits and covariance matrix
  /// Index of the EMC  pidTratis in the event
  Short_t  mBEmcPidTraitsIndex;
  /// Index of the BTOF pidTratis in the event
  Short_t  mBTofPidTraitsIndex;
  /// Index of the MTD  pidTratis in the event
  Short_t  mMtdPidTraitsIndex;

  ClassDef(StPicoTrack, 4)
};

/**
 * Getters
 */
inline void StPicoTrack::setBEmcPidTraitsIndex(Int_t index) {
  mBEmcPidTraitsIndex = (Short_t)index;
}
inline void StPicoTrack::setBTofPidTraitsIndex(Int_t index) {
  mBTofPidTraitsIndex = (Short_t)index;
}
inline void StPicoTrack::setMtdPidTraitsIndex(Int_t index) {
  mMtdPidTraitsIndex = (Short_t)index;
}
inline Int_t   StPicoTrack::id() const { return mId; }
inline Float_t StPicoTrack::chi2() const { return mChi2 / 1000.f; }
inline Float_t StPicoTrack::gPt() const { return gMom().Perp(); }
inline Float_t StPicoTrack::gPtot() const { return gMom().Mag(); }
inline Float_t StPicoTrack::pPt() const { return isPrimary() ? pMom().Perp() : 0.; }
inline Float_t StPicoTrack::pPtot() const { return isPrimary() ? pMom().Mag() : 0.; }
inline TVector3 StPicoTrack::pMom() const {
  return TVector3(mPMomentumX, mPMomentumY, mPMomentumZ);
}
inline TVector3 StPicoTrack::gMom() const {
  return TVector3(mGMomentumX, mGMomentumY, mGMomentumZ);
}
inline TVector3 StPicoTrack::origin() const {
  return TVector3(mOriginX, mOriginY, mOriginZ);
}
inline Float_t StPicoTrack::gDCAx(Float_t x) const { return (mOriginX - x); }
inline Float_t StPicoTrack::gDCAy(Float_t y) const { return (mOriginY - y); }
inline Float_t StPicoTrack::gDCAz(Float_t z) const { return (mOriginZ - z); }
inline Short_t StPicoTrack::charge() const { return (mNHitsFit > 0) ? 1 : -1; }
inline Int_t   StPicoTrack::nHits() const { return (mNHitsFit > 0) ? (Int_t)mNHitsFit : (Int_t)(-1 * mNHitsFit); }
inline Int_t   StPicoTrack::nHitsFit() const { return (mNHitsFit > 0) ? (Int_t)mNHitsFit : (Int_t)(-1 * mNHitsFit); }
inline Int_t   StPicoTrack::nHitsMax() const { return (Int_t)mNHitsMax; }
inline Int_t   StPicoTrack::nHitsDedx() const { return (Int_t)mNHitsDedx; }
inline UInt_t  StPicoTrack::hftHitsMap() const { return topologyMap(0) >> 1 & 0x7F; }
inline Float_t StPicoTrack::dEdx() const { return mDedx; }
inline Float_t StPicoTrack::dEdxError() const { return mDedxError; }
//inline Float_t StPicoTrack::dNdx() const { return mDnDx;}
//inline Float_t StPicoTrack::dNdxError() const { return mDnDxError;}
inline Float_t StPicoTrack::nSigmaPion() const { return (Float_t)mNSigmaPion / 1000.f; }
inline Float_t StPicoTrack::nSigmaKaon() const { return (Float_t)mNSigmaKaon / 1000.f; }
inline Float_t StPicoTrack::nSigmaProton() const { return (Float_t)mNSigmaProton / 1000.f; }
inline Float_t StPicoTrack::nSigmaElectron() const { return (Float_t)mNSigmaElectron / 1000.f; }
inline UInt_t  StPicoTrack::topologyMap(UInt_t idx) const { return mTopologyMap[idx]; }
inline Int_t   StPicoTrack::bemcPidTraitsIndex() const { return mBEmcPidTraitsIndex; }
inline Int_t   StPicoTrack::bTofPidTraitsIndex() const { return mBTofPidTraitsIndex; }
inline Int_t   StPicoTrack::mtdPidTraitsIndex() const { return mMtdPidTraitsIndex; }
inline Bool_t  StPicoTrack::hasPxl1Hit() const { return hftHitsMap() >> 0 & 0x1; }
inline Bool_t  StPicoTrack::hasPxl2Hit() const { return hftHitsMap() >> 1 & 0x3; }
inline Bool_t  StPicoTrack::hasIstHit()  const { return hftHitsMap() >> 3 & 0x3; }
inline Bool_t  StPicoTrack::hasSstHit()  const { return hftHitsMap() >> 5 & 0x3; }
inline Bool_t  StPicoTrack::isHft() const
{ return hasPxl1Hit() && hasPxl2Hit() && (hasIstHit() || hasSstHit()); }
inline Bool_t  StPicoTrack::isHFTTrack() const { return isHft(); }
inline Bool_t  StPicoTrack::hasHft4Layers() const {
  return hasPxl1Hit() && hasPxl2Hit() && hasIstHit() && hasSstHit();
}
inline Bool_t  StPicoTrack::isTofTrack() const {
  return (mBTofPidTraitsIndex<0) ? false : true;
}
inline Bool_t  StPicoTrack::isBemcTrack() const {
  return (mBEmcPidTraitsIndex<0) ? false : true;
}
inline Bool_t  StPicoTrack::isMtdTrack() const {
  return (mMtdPidTraitsIndex<0) ? false : true;
}
inline Bool_t  StPicoTrack::isPrimary() const { return ( pMom().Mag()>0 ); }

/**
 * Setters
 */
inline void StPicoTrack::setId(Int_t id) { mId = (UShort_t)id; }
inline void StPicoTrack::setNHitsFit(Int_t nhits) { mNHitsFit = (Char_t)nhits; }
inline void StPicoTrack::setDedxError(Float_t dEdxError) { mDedxError = dEdxError; }
inline void StPicoTrack::setPrimaryMomentum(Double_t px, Double_t py, Double_t pz) {
  mPMomentumX = (Float_t)px; mPMomentumY = (Float_t)py; mPMomentumZ = (Float_t)pz;
}
inline void StPicoTrack::setPrimaryMomentum(Float_t px, Float_t py, Float_t pz) {
  mPMomentumX = px; mPMomentumY = py; mPMomentumZ = pz;
}
inline void StPicoTrack::setPrimaryMomentum(TVector3 mom) {
  mPMomentumX = (Float_t)mom.X(); mPMomentumY = (Float_t)mom.Y(); mPMomentumZ = (Float_t)mom.Z();
}
inline void StPicoTrack::setGlobalMomentum(Double_t px, Double_t py, Double_t pz) {
  mGMomentumX = (Float_t)px; mGMomentumY = (Float_t)py; mGMomentumZ = (Float_t)pz;
}
inline void StPicoTrack::setGlobalMomentum(Float_t px, Float_t py, Float_t pz) {
  mGMomentumX = px; mGMomentumY = py; mGMomentumZ = pz;
}
inline void StPicoTrack::setGlobalMomentum(TVector3 mom) {
  mGMomentumX = (Float_t)mom.X(); mGMomentumY = (Float_t)mom.Y(); mGMomentumZ = (Float_t)mom.Z();
}
inline void StPicoTrack::setOrigin(Double_t x, Double_t y, Double_t z) {
  mOriginX = (Float_t)x; mOriginY = (Float_t)y; mOriginZ = (Float_t)z;
}
inline void StPicoTrack::setOrigin(Float_t x, Float_t y, Float_t z) {
  mOriginX = x; mOriginY = y; mOriginZ = z;
}
inline void StPicoTrack::setOrigin(TVector3 orig) {
  mOriginX = (Float_t)orig.X(); mOriginY = (Float_t)orig.Y(); mOriginZ = (Float_t)orig.Z();
}

#endif
