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
#include "StPicoTrackCovMatrix.h"
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
  Float_t dEdxError() 	    const { return mDedxError;} 
  //Float_t dNdx() const;
  //Float_t dNdxError() const;
#if ! defined (_VANILLA_ROOT_)
  Float_t dEdxPull(Float_t mass, UChar_t fit = 1, Int_t charge = 1) const;
  Float_t dEdxPullToF(Float_t mass, UChar_t fit = 1, Int_t charge = 1) const;
  Float_t nSigmaPion()      const {return dEdxPull(0.13956995,1);}
  Float_t nSigmaKaon()      const {return dEdxPull(0.493677,1);}
  Float_t nSigmaProton()    const {return dEdxPull(0.93827231,1);}
  Float_t nSigmaElectron()  const {return dEdxPull(0.51099907e-3,1);}
#else /* _VANILLA_ROOT_ */
  Float_t nSigmaPion()      const;
  Float_t nSigmaKaon()      const;
  Float_t nSigmaProton()    const;
  Float_t nSigmaElectron()  const;
#endif
  /// Track topology map, seet StEvent/StTrackTopologyMap.cxx
  UInt_t  topologyMap(UInt_t) const;

  Bool_t hasPxl1Hit() const;
  Bool_t hasPxl2Hit() const;
  Bool_t hasIstHit() const;
  Bool_t hasSstHit() const;
  Bool_t isHft() const;
  Bool_t isHFTTrack() const;
  Bool_t hasHft4Layers() const;

  /// Checks whether this track is associated with a primary vertex
  Bool_t isPrimary() const;
#if ! defined (_VANILLA_ROOT_)
  Float_t* params()                   { return StPicoDst::instance()->trackCovMatrix(trackCovMatrixIndex())->params(); }
  const Float_t* params()       const { return StPicoDst::instance()->trackCovMatrix(trackCovMatrixIndex())->params(); }
  const Float_t* sigmas()       const { return StPicoDst::instance()->trackCovMatrix(trackCovMatrixIndex())->sigmas(); }
  const Float_t* correlations() const { return StPicoDst::instance()->trackCovMatrix(trackCovMatrixIndex())->correlations(); }
#endif /* ! _VANILLA_ROOT_ */
  Int_t bemcPidTraitsIndex() const;
  Int_t bTofPidTraitsIndex() const;
  Int_t mtdPidTraitsIndex() const;
  Int_t trackCovMatrixIndex() const;

  StDcaGeometry dcaGeometry() const {return StPicoDst::instance()->trackCovMatrix(trackCovMatrixIndex())->dcaGeometry();}
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
  void setNHitsFit(Int_t nhits);     // nhits MUST be (charge * nHitsFit) !!!
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
  void setTrackCovMatrixIndex(Int_t index);

 protected:

  UShort_t mId;               // track Id, copied from StMuTrack, StTrack
  UShort_t mChi2;             // chi2*1000
  Float_t mPMomentumX;        // Px of the primary track (0. if not primary)
  Float_t mPMomentumY;        // Py of the primary track (0. if not primary)
  Float_t mPMomentumZ;        // Pz of the primary track (0. if not primary)
  Float_t mGMomentumX;        // At DCA to primary vertex
  Float_t mGMomentumY;        // At DCA to primary vertex
  Float_t mGMomentumZ;        // At DCA to primary vertex
  Float_t mOriginX;           // At DCA to primary vertex
  Float_t mOriginY;           // At DCA to primary vertex
  Float_t mOriginZ;           // At DCA to primary vertex
  Float16_t  mDedx;           // dEdx in KeV/cm.
  Float16_t mDedxError;       //   
  //Float16_t  mDnDx;             // fitted dN/dx
  //Float16_t  mDnDxError;        // fitted dN/dx error
  Char_t   mNHitsFit;         // charge * nHitsFit
  UChar_t  mNHitsMax;         // nHitsMax - TPC
  UChar_t  mNHitsDedx;        // nHitsDedx - TPC
  Short_t  mNSigmaPion;       // nsigmaPi * 100
  Short_t  mNSigmaKaon;       // nsigmaK * 100
  Short_t  mNSigmaProton;     // nsigmaP * 100
  Short_t  mNSigmaElectron;   // nsigmaE * 100
  UInt_t   mTopologyMap[3];   // Toplogy Map data0 and data1, and data2 (for iTPC) . See StEvent/StTrackTopologyMap.cxx

  /// Indicex to pidTraits and covariance matrix
  Short_t  mBEmcPidTraitsIndex;  // index of the EMC  pidTratis in the event
  Short_t  mBTofPidTraitsIndex;  // index of the BTOF pidTratis in the event
  Short_t  mMtdPidTraitsIndex;   // index of the MTD  pidTratis in the event
  Short_t  mTrackCovMatrixIndex; // index of the track covariance matrix in the event

  ClassDef(StPicoTrack, 3)
};

/**
 * Getters
 */
inline void StPicoTrack::setBEmcPidTraitsIndex(Int_t index) { mBEmcPidTraitsIndex = (Short_t)index; }
inline void StPicoTrack::setBTofPidTraitsIndex(Int_t index) { mBTofPidTraitsIndex = (Short_t)index; }
inline void StPicoTrack::setMtdPidTraitsIndex(Int_t index) { mMtdPidTraitsIndex = (Short_t)index; }
inline void StPicoTrack::setTrackCovMatrixIndex(Int_t index) { mTrackCovMatrixIndex = (Short_t)index; }
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
inline Float_t StPicoTrack::dEdx() const { return mDedx;}
//inline Float_t StPicoTrack::dNdx() const { return mDnDx;}
//inline Float_t StPicoTrack::dNdxError() const { return mDnDxError;}
#if defined (_VANILLA_ROOT_)
inline Float_t StPicoTrack::nSigmaPion() const { return mNSigmaPion / 100.f; }
inline Float_t StPicoTrack::nSigmaKaon() const { return mNSigmaKaon / 100.f; }
inline Float_t StPicoTrack::nSigmaProton() const { return mNSigmaProton / 100.f; }
inline Float_t StPicoTrack::nSigmaElectron() const { return mNSigmaElectron / 100.f; }
#endif /* _VANILLA_ROOT_ */
inline UInt_t  StPicoTrack::topologyMap(UInt_t idx) const { return mTopologyMap[idx]; }
inline Int_t   StPicoTrack::bemcPidTraitsIndex() const { return mBEmcPidTraitsIndex; }
inline Int_t   StPicoTrack::bTofPidTraitsIndex() const { return mBTofPidTraitsIndex; }
inline Int_t   StPicoTrack::mtdPidTraitsIndex() const { return mMtdPidTraitsIndex; }
inline Int_t   StPicoTrack::trackCovMatrixIndex() const { return mTrackCovMatrixIndex; }
inline Bool_t  StPicoTrack::hasPxl1Hit() const { return hftHitsMap() >> 0 & 0x1; }
inline Bool_t  StPicoTrack::hasPxl2Hit() const { return hftHitsMap() >> 1 & 0x3; }
inline Bool_t  StPicoTrack::hasIstHit()  const { return hftHitsMap() >> 3 & 0x3; }
inline Bool_t  StPicoTrack::hasSstHit()  const { return hftHitsMap() >> 5 & 0x3; }
inline Bool_t  StPicoTrack::isHft() const {
  return hasPxl1Hit() && hasPxl2Hit() && (hasIstHit() || hasSstHit());
}
inline Bool_t  StPicoTrack::isHFTTrack() const { return isHft(); }
inline Bool_t  StPicoTrack::hasHft4Layers() const {
  return hasPxl1Hit() && hasPxl2Hit() && hasIstHit() && hasSstHit();
}
/**
 * The default "primary" momentum is (0, 0, 0) but it is expected to have
 * a non-zero length when the track is associated with a primary vertex.
 */
inline Bool_t StPicoTrack::isPrimary() const {
  return ( pMom().Mag()>0);
}

/// Return the global momentum at the dca point to the pVtx (usually it is the primary vertex.   B - magnetic field from PicoEvent::bField()
//_________________
inline TVector3 StPicoTrack::gMom(TVector3 pVtx, Float_t const B) const {
  StPicoPhysicalHelix gHelix = helix(B);
  return gHelix.momentumAt( gHelix.pathLength( pVtx ), B * kilogauss );
}

//_________________
inline StPicoPhysicalHelix StPicoTrack::helix(Float_t const B) const {
  return StPicoPhysicalHelix( gMom(), origin(), B * kilogauss,
			      static_cast<float>( charge() ) );
}

/**
 * Setters
 */
inline void StPicoTrack::setId(Int_t id) { mId = (UShort_t)id; }
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
