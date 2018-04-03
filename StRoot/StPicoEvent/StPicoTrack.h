#ifndef StPicoTrack_h
#define StPicoTrack_h

#include <cmath>

#include "TObject.h"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StarClassLibrary/SystemOfUnits.h"

class StMuTrack;
class StDcaGeometry;

//_________________
class StPicoTrack : public TObject {

 public:
  /// Default constructor
  StPicoTrack();
  /// Constructor that takes global and primary trakcs
  /// Note: primary track should be associated with the StPicoEvent::mPrimaryVertex
  StPicoTrack(StMuTrack const* globalTrack, StMuTrack const* primaryTrack,
	      double magField, StThreeVectorD const& pVtx, StDcaGeometry const& dcaG);
  /// Copy constructor
  StPicoTrack(const StPicoTrack &track);
  /// Destructor
  virtual ~StPicoTrack();
  /// Print track parameters
  virtual void Print(Char_t const* option = "") const;

  /// track id, copied from StMuTrack, StTrack
  Int_t   id() const;
  Float_t chi2() const;
  /// primary momentum, only if track is primary with selected vertex StPicoEvent::mPrimaryVertex
  StThreeVectorF const& pMom() const;
  /// global momentum at point of DCA to StPicoEvent::mPrimaryVertex
  StThreeVectorF const& gMom() const;
  Float_t gPt() const;
  Float_t gPtot() const;
  /// global momentum at point of DCA to pVtx, B should be in kilogauss
  StThreeVectorF gMom(StThreeVectorF const& pVtx, float B) const;
  /// origin at DCA to StPicoEvent::mPrimaryVertex
  StThreeVectorF const& origin() const;
  /// dca point to StPicoEvent::mPrimaryVertex in global coordinates .
  /// It is the same as origin. To get gDCA = origin(or dcaPoint) + prim.vtx.position
  StThreeVectorF const& dcaPoint() const;
  /// Next functions return DCA (or its components) of the global track
  /// to the point with coordinates (pVtxX, pVtxY, pVtxZ)
  Float_t gDCAx(Float_t pVtxX) const;
  Float_t gDCAy(Float_t pVtxY) const;
  Float_t gDCAz(Float_t pVtxZ) const;
  Float_t gDCAxy(Float_t pVtxX, Float_t pVtxZ);
  Float_t gDCA(Float_t pVtxX, Float_t pVtxY, Float_t pVtxZ);
  /// Charge of the track is encoded in nHitsFit as: nHitsFit * charge
  Short_t charge() const;
  Int_t   nHits() const;       //if(isPrimary)? nHitsFit-1 : nHits=nHitsFit 
  Int_t   nHitsFit() const;
  Int_t   nHitsMax() const;
  Int_t   nHitsDedx() const;
  UInt_t  hftHitsMap() const;
  Float_t dEdx() const;
  //Float_t dNdx() const;
  //Float_t dNdxError() const;
  Float_t nSigmaPion() const;
  Float_t nSigmaKaon() const;
  Float_t nSigmaProton() const;
  Float_t nSigmaElectron() const;

  /// track topology map, seet StEvent/StTrackTopologyMap.cxx
  UInt_t  topologyMap(unsigned int) const;

  /// helix at point of DCA to StPicoEvent::mPrimaryVertex
  StPhysicalHelixD helix(float B) const;
  bool hasPxl1Hit() const;
  bool hasPxl2Hit() const;
  bool hasIstHit() const;
  bool hasSstHit() const;
  bool isHft() const;
  bool isHFTTrack() const;
  bool hasHft4Layers() const;

  /// Checks whether this track is associated with a primary vertex
  bool isPrimary() const;

  /// MTD pid traits
  void setBEmcPidTraitsIndex(Int_t index);
  void setBTofPidTraitsIndex(Int_t index);
  void setMtdPidTraitsIndex(Int_t index);

  Int_t bemcPidTraitsIndex() const;
  Int_t bTofPidTraitsIndex() const;
  Int_t mtdPidTraitsIndex() const;

 protected:

  UShort_t mId;               // track Id, copied from StMuTrack, StTrack
  UShort_t mChi2;             // chi2*1000
  StThreeVectorF mPMomentum;  // primary momentum, (0.,0.,0.) if none
  StThreeVectorF mGMomentum;  // global momentum at point of DCA to StPicoEvent::mPrimaryVertex
  StThreeVectorF mOrigin;     // origin at dca to primary vertex
  Float_t  mDedx;             // dEdx in KeV/cm.
  //Float_t  mDnDx;             // fitted dN/dx
  //Float_t  mDnDxError;        // fitted dN/dx error
  Char_t   mNHitsFit;         // charge * nHitsFit - TPC; if(pTrk) ? nHitsFit=nHits+1 : nHitsFit=nHits
  UChar_t  mNHitsMax;         // nHitsMax - TPC
  UChar_t  mNHitsDedx;        // nHitsDedx - TPC
  Short_t  mNSigmaPion;       // nsigmaPi * 100
  Short_t  mNSigmaKaon;       // nsigmaK * 100
  Short_t  mNSigmaProton;     // nsigmaP * 100
  Short_t  mNSigmaElectron;   // nsigmaE * 100
  UInt_t   mTopologyMap[2];   // Toplogy Map data0 and data1. See StEvent/StTrackTopologyMap.cxx

  /// pidTraits
  Short_t  mBEmcPidTraitsIndex;  // index of the EMC  pidTratis in the event
  Short_t  mBTofPidTraitsIndex; // index of the BTOF pidTratis in the event
  Short_t  mMtdPidTraitsIndex;  // index of the MTD  pidTratis in the event

  ClassDef(StPicoTrack, 2)
};

inline void StPicoTrack::setBEmcPidTraitsIndex(Int_t index) { mBEmcPidTraitsIndex = (Short_t)index; }
inline void StPicoTrack::setBTofPidTraitsIndex(Int_t index) { mBTofPidTraitsIndex = (Short_t)index; }
inline void StPicoTrack::setMtdPidTraitsIndex(Int_t index) { mMtdPidTraitsIndex = (Short_t)index; }
inline Int_t   StPicoTrack::id() const { return mId; }
inline Float_t StPicoTrack::chi2() const { return mChi2 / 1000.f; }
inline Float_t StPicoTrack::gPt() const { return mGMomentum.perp(); }
inline Float_t StPicoTrack::gPtot() const { return mGMomentum.mag(); }
inline StThreeVectorF const& StPicoTrack::pMom() const { return mPMomentum; }
inline StThreeVectorF const& StPicoTrack::gMom() const { return mGMomentum; }
inline StThreeVectorF const& StPicoTrack::origin() const { return mOrigin; }
inline StThreeVectorF const& StPicoTrack::dcaPoint() const { return mOrigin; }
inline Float_t StPicoTrack::gDCAx(Float_t x) const { return (mOrigin.x() - x); }
inline Float_t StPicoTrack::gDCAy(Float_t y) const { return (mOrigin.y() - y); }
inline Float_t StPicoTrack::gDCAz(Float_t z) const { return (mOrigin.z() - z); }
inline Short_t StPicoTrack::charge() const { return (mNHitsFit > 0) ? 1 : -1; }
inline Int_t   StPicoTrack::nHits() const { return ( isPrimary() ) ? (nHitsFit() - 1) : nHitsFit(); }
inline Int_t   StPicoTrack::nHitsFit() const { return (mNHitsFit > 0) ? (Int_t)mNHitsFit : (Int_t)(-1 * mNHitsFit); }
inline Int_t   StPicoTrack::nHitsMax() const { return mNHitsMax; }
inline Int_t   StPicoTrack::nHitsDedx() const { return mNHitsDedx; }
inline UInt_t  StPicoTrack::hftHitsMap() const { return topologyMap(0) >> 1 & 0x7F; }
inline Float_t StPicoTrack::dEdx() const { return mDedx;}
//inline Float_t StPicoTrack::dNdx() const { return mDnDx;}
//inline Float_t StPicoTrack::dNdxError() const { return mDnDxError;}
inline Float_t StPicoTrack::nSigmaPion() const { return mNSigmaPion / 100.f; }
inline Float_t StPicoTrack::nSigmaKaon() const { return mNSigmaKaon / 100.f; }
inline Float_t StPicoTrack::nSigmaProton() const { return mNSigmaProton / 100.f; }
inline Float_t StPicoTrack::nSigmaElectron() const { return mNSigmaElectron / 100.f; }
inline UInt_t  StPicoTrack::topologyMap(unsigned int idx) const { return mTopologyMap[idx]; }
inline Int_t   StPicoTrack::bemcPidTraitsIndex() const { return mBEmcPidTraitsIndex; }
inline Int_t   StPicoTrack::bTofPidTraitsIndex() const { return mBTofPidTraitsIndex; }
inline Int_t   StPicoTrack::mtdPidTraitsIndex() const { return mMtdPidTraitsIndex; }
inline bool    StPicoTrack::hasPxl1Hit() const { return hftHitsMap() >> 0 & 0x1; }
inline bool    StPicoTrack::hasPxl2Hit() const { return hftHitsMap() >> 1 & 0x3; }
inline bool    StPicoTrack::hasIstHit()  const { return hftHitsMap() >> 3 & 0x3; }
inline bool    StPicoTrack::hasSstHit()  const { return hftHitsMap() >> 5 & 0x3; }
inline bool    StPicoTrack::isHft() const { return hasPxl1Hit() && hasPxl2Hit() && (hasIstHit() || hasSstHit()); }
inline bool    StPicoTrack::isHFTTrack() const { return isHft(); }
inline bool    StPicoTrack::hasHft4Layers() const { return hasPxl1Hit() && hasPxl2Hit() && hasIstHit() && hasSstHit(); }

/**
 * The default "primary" momentum is (0, 0, 0) but it is expected to have
 * a non-zero length when the track is associated with a primary vertex.
 */
inline bool StPicoTrack::isPrimary() const {
  return mPMomentum.magnitude() > 0;
}

/// Return the global momentum at the dca point to the pVtx (usually it is the primary vertex.   B - magnetic field from PicoEvent::bField()
//_________________
inline StThreeVectorF StPicoTrack::gMom(StThreeVectorF const& pVtx, float const B) const {
  StPhysicalHelixD gHelix = helix(B);
  return gHelix.momentumAt(gHelix.pathLength(pVtx), B * kilogauss);
}

//_________________
inline StPhysicalHelixD StPicoTrack::helix(float const B) const {
  return StPhysicalHelixD(mGMomentum, mOrigin, B * kilogauss, static_cast<float>(charge()));
}

#endif
