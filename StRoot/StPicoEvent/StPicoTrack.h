#ifndef StPicoTrack_h
#define StPicoTrack_h

#include <cmath>

#include "TObject.h"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StarClassLibrary/SystemOfUnits.h"
#include "StEvent/StDcaGeometry.h"

class StMuTrack;

//_________________
class StPicoTrack : public TObject {

 public:
  /// Default constructor
  StPicoTrack();
  /// Constructor that takes global and primary trakcs
  /// Note: primary track should be associated with the StPicoEvent::mPrimaryVertex
  StPicoTrack(StMuTrack const* globalTrack, StMuTrack const* primaryTrack,
	      double magField, StThreeVectorD const& pVtx, StDcaGeometry const& dcaG);
  //Copy constructor
  // Take default  StPicoTrack(const StPicoTrack &track);
  //Destructor
  virtual ~StPicoTrack() {}
  //Print track parameters
  virtual void Print(Char_t const* option = "") const;
  //! track id, copied from StMuTrack, StTrack
  Int_t   id() const   { return mId; }
  Float_t chi2() const { return mChi2; }
  //! primary momentum, only if track is primary with selected vertex StPicoEvent::mPrimaryVertex
  StThreeVectorF pMom() const { return StThreeVectorF(mPMomentum); }
  //! global momentum at point of DCA to StPicoEvent::mPrimaryVertex
  StThreeVectorF gMom() const { return dcaGeometry().momentum();}
  //! global momentum at point of DCA to pVtx, B should be in kilogauss
  StThreeVectorD gMom(StThreeVectorF const& pVtx, float B) const {
    StPhysicalHelixD gHelix = helix();
    Double_t path = gHelix.pathLength(pVtx);
    return gHelix.momentumAt(path,B);
  }
  Float_t gPt() const { return gMom().perp(); }
  Float_t gPtot() const { return gMom().mag(); }
  //! dca point to StPicoEvent::mPrimaryVertex in global coordinates
  StThreeVectorD dcaPoint(StThreeVectorD const &pVtx) const { 
    StPhysicalHelixD gHelix = helix(); 
    gHelix.moveOrigin(gHelix.pathLength(pVtx)); 
    return gHelix.origin();
  }
  //! origin at DCA to StPicoEvent::mPrimaryVertex
  StThreeVectorD origin(StThreeVectorD const &pVtx) const { return dcaPoint(pVtx); }
  Float_t charge()          const { return mCharge; }
  Int_t   nHitsFit()        const { return (mNHitsFit > 0) ? (Int_t)mNHitsFit : (Int_t)(-1 * mNHitsFit); }
  Int_t   nHitsMax()        const { return mNHitsMax; }
  Int_t   nHitsDedx()       const { return mNHitsDedx; }
  UInt_t  hftHitsMap()      const { return topologyMap(0) >> 1 & 0x7F; } 
  Float_t dEdx() 	    const { return mDedx;}      		       		      
  Float_t dEdxError() 	    const { return mDedxError;}      		      
  Float_t dNdx() 	    const { return mDnDx;}                       
  Float_t dNdxError() 	    const { return mDnDxError;}
  Float_t dEdxPull(Float_t mass, UChar_t fit = 1, Int_t charge = 1) const;
  Float_t dEdxPullToF(Float_t mass, UChar_t fit = 1, Int_t charge = 1) const;
  Float_t nSigmaPion()      const {return dEdxPull(0.13956995,1);}
  Float_t nSigmaKaon()      const {return dEdxPull(0.493677,1);}
  Float_t nSigmaProton()    const {return dEdxPull(0.93827231,1);}
  Float_t nSigmaElectron()  const {return dEdxPull(0.51099907e-3,1);}

  //! track topology map, seet StEvent/StTrackTopologyMap.cxx
  UInt_t  topologyMap(UInt_t idx) const { return mTopologyMap[idx]; }

  Bool_t hasPxl1Hit() const { return hftHitsMap() >> 0 & 0x1; }
  Bool_t hasPxl2Hit() const { return hftHitsMap() >> 1 & 0x3; }
  Bool_t hasIstHit()  const { return hftHitsMap() >> 3 & 0x3; }
  Bool_t hasSstHit()  const { return hftHitsMap() >> 5 & 0x3; }
  Bool_t isHft()      const { return hasPxl1Hit() && hasPxl2Hit() && (hasIstHit() || hasSstHit()); }
  Bool_t isHFTTrack() const { return isHft(); }
  Bool_t hasHft4Layers() const { return hasPxl1Hit() && hasPxl2Hit() && hasIstHit() && hasSstHit(); } 

  /** Checks whether this track is associated with a primary vertex. */
  Bool_t isPrimary()            const { return pMom().magnitude() > 0;}
  Float_t* params()                   { return &mImp; }
  const Float_t* params()       const { return &mImp; }
  const Float_t* sigmas()       const { return mSigma; }
  const Float_t* correlations() const { return mCorr; }

  StDcaGeometry dcaGeometry() const;
  StPhysicalHelixD helix() const {return dcaGeometry().helix();}

  // MTD pid traits
  void setBEmcPidTraitsIndex(Int_t index) { mBEmcPidTraitsIndex = (Short_t)index; }
  void setBTofPidTraitsIndex(Int_t index) { mBTofPidTraitsIndex = (Short_t)index; }
  void setMtdPidTraitsIndex(Int_t index)  { mMtdPidTraitsIndex = (Short_t)index; } 

  Int_t bemcPidTraitsIndex() const { return mBEmcPidTraitsIndex; }
  Int_t bTofPidTraitsIndex() const { return mBTofPidTraitsIndex; }
  Int_t mtdPidTraitsIndex()  const { return mMtdPidTraitsIndex; } 

protected:
  UShort_t  mId;             // track Id, copied from StMuTrack, StTrack
  Float16_t mChi2;           // 
  Float16_t mPMomentum[3];   // 
  Float16_t mDedx;           // dEdx in KeV/cm.
  Float16_t mDedxError;      // 
  Float16_t mDnDx;           // fitted dN/dx
  Float16_t mDnDxError;      // fitted dN/dx error
  Char_t    mNHitsFit;       // nHitsFit - TPC
  Char_t    mNHitsMax;       // nHitsMax - TPC
  UChar_t   mNHitsDedx;      // nHitsDedx - TPC
  Float16_t mCharge;         // [-1,1,2]|
  UInt_t    mTopologyMap[3]; // Toplogy Map data0 and data1. data2 is iTPC extension See StEvent/StTrackTopologyMap.cxx

  /// pidTraits
  Short_t  mBEmcPidTraitsIndex;  // index of the EMC  pidTratis in the event
  Short_t  mBTofPidTraitsIndex; // index of the BTOF pidTratis in the event
  Short_t  mMtdPidTraitsIndex;  // index of the MTD  pidTratis in the event
  // dcaG
  Float16_t  mImp;     // signed impact parameter; Signed in such a way that: x =  -impact*sin(Psi), y =   impact*cos(Psi)
  Float16_t  mZ;       //               Z-coordinate of this track (reference plane)
  Float16_t  mPsi;     //[-pi,pi,20]    Psi angle of the track
  Float16_t  mPti;     //               signed invert pt [sign = sign(-qB)]
  Float16_t  mTan;     //[-10,10,20]    tangent of the track momentum dip angle
  Float16_t  mCurv;    //               signed curvature
  Float16_t mSigma[5];
  Float16_t mCorr[10]; //[-1,1,20] 
  ClassDef(StPicoTrack, 6)
};

#endif
