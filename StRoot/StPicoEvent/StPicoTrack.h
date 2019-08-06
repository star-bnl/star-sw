/**
 * \class StPicoTrack
 * \brief Holds information about track parameters
 *
 * The class stores information about the tracks reconstructed in TPC
 */

#ifndef StPicoTrack_h
#define StPicoTrack_h

// C++ headers
#include <cmath>

// ROOT headers
#include "TObject.h"
#include "TVector3.h"

// PicoDst headers
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

  //
  // Getters
  //

  /// Return unique Id of the track
  Int_t   id() const              { return mId; }
  /// Return chi2 of the track
  Float_t chi2() const            { return mChi2 / 1000.f; }
  /// Return momentum (GeV/c) of the primary track. Return (0,0,0) if not primary track
  TVector3 pMom() const           { return TVector3(mPMomentumX, mPMomentumY, mPMomentumZ); }
  /// Return momentum (GeV/c) of the global tracks at the point of DCA to the primary vertex
  TVector3 gMom() const           { return TVector3(mGMomentumX, mGMomentumY, mGMomentumZ); }
  /// Return space coordinate (cm) of DCA to the primary vertex
  TVector3 origin() const         { return TVector3(mOriginX, mOriginY, mOriginZ); }
  /// Return global tranverse momentum (GeV/c)
  Float_t gPt() const             { return gMom().Perp(); }
  /// Return global total momentum (GeV/c)
  Float_t gPtot() const           { return gMom().Mag(); }
  /// Return transverse momentum (GeV/c) of the primary track
  Float_t pPt() const             { return isPrimary() ? pMom().Perp() : 0.; }
  /// Return total momentum (GeV/c) of the primary track
  Float_t pPtot() const           { return isPrimary() ? pMom().Mag() : 0.; }
  /// Global momentum at point of DCA to pVtx, B should be in kilogauss
  TVector3 gMom(TVector3 pVtx, Float_t B) const;
  /// Helix at point of DCA to StPicoEvent::mPrimaryVertex
  StPicoPhysicalHelix helix(Float_t const B) const;

  // Next functions return DCA (or its components) of the global track
  // to the point with coordinates (pVtxX, pVtxY, pVtxZ)

  /// Return signed distance in x direction (cm) between the value and x position of the DCA point (gDCAx - x)
  Float_t gDCAx(Float_t pVtxX) const     { return (mOriginX - pVtxX); }
  /// Return signed distance in y direction between the value and y position of the DCA point (gDCAy - y)
  Float_t gDCAy(Float_t pVtxY) const     { return (mOriginY - pVtxY); }
  /// Return signed distance in z direction (cm) between the value and z position of the DCA point (gDCAz - z)
  Float_t gDCAz(Float_t pVtxZ) const     { return (mOriginZ - pVtxZ); }
  /// Return distance in xy direction (cm) between the (x,y) point and the DCA point to primary vertex
  Float_t gDCAxy(Float_t pVtxX, Float_t pVtxY) const;
  /// Return distance in xyz direction (cm) between the (x,y,z) point and the DCA point to primary vertex
  Float_t gDCA(Float_t pVtxX, Float_t pVtxY, Float_t pVtxZ) const;
  /// Return 3-vector (distance) between the DCA point and the point (gDCA - point)
  TVector3 gDCA(TVector3 pVtx) const;
  /// Return charge of the track (encoded in nHitsFit as: nHitsFit * charge)
  Short_t charge() const                 { return (mNHitsFit > 0) ? 1 : -1; }
  /// Return number of hits
  Int_t   nHits() const                  { return (mNHitsFit > 0) ? (Int_t)mNHitsFit : (Int_t)(-1 * mNHitsFit); }
  /// Return number of hits fit
  Int_t   nHitsFit() const               { return (mNHitsFit > 0) ? (Int_t)mNHitsFit : (Int_t)(-1 * mNHitsFit); }
  /// Return number of hits possible
  Int_t   nHitsMax() const               { return (Int_t)mNHitsMax; }
  /// Return number of hits possible
  Int_t   nHitsPoss() const              { return nHitsMax(); }
  /// Return number of hits used for dE/dx measurement
  Int_t   nHitsDedx() const              { return (Int_t)mNHitsDedx; }
  /// Return a map of hits in HFT
  UInt_t  hftHitsMap() const             { return topologyMap(0) >> 1 & 0x7F; }
  /// Return dE/dx (GeV/cm) of the track
  Float_t dEdx() const                   { return mDedx; }
  /// Return dE/dx error of the track
  Float_t dEdxError() const              { return mDedxError; }
  //Float_t dNdx() const                   { return mDnDx;}
  //Float_t dNdxError() const              { return mDnDxError;}

  /// Return nSigma(pion)
  Float_t nSigmaPion() const             { return (Float_t)mNSigmaPion / 1000.f; }
  /// Return nSigma(kaon)
  Float_t nSigmaKaon() const             { return (Float_t)mNSigmaKaon / 1000.f; }
  /// Return nSigma(proton)
  Float_t nSigmaProton() const           { return (Float_t)mNSigmaProton / 1000.f; }
  /// Return nSigma(electron)
  Float_t nSigmaElectron() const         { return (Float_t)mNSigmaElectron / 1000.f; }

  /// Return track topology map (return 0 in case when requested index is >1)
  UInt_t  topologyMap(UInt_t idx) const  { return (idx>1) ? 0 : mTopologyMap[idx]; }

  /// Return if the track has an inner PXL hit
  Bool_t hasPxl1Hit() const              { return hftHitsMap() >> 0 & 0x1; }        
  /// Return if the track has an outer PXL hit
  Bool_t hasPxl2Hit() const              { return hftHitsMap() >> 1 & 0x3; }
  /// Return if the track has an IST hit
  Bool_t hasIstHit() const               { return hftHitsMap() >> 3 & 0x3; }
  /// Return if the track has an inner SST hit
  Bool_t hasSstHit() const               { return hftHitsMap() >> 5 & 0x3; }
  /// Return if trach has hit in HFT
  Bool_t isHft() const                   { return hasPxl1Hit() && hasPxl2Hit() && (hasIstHit() || hasSstHit()); }
  /// Return if trach has hit in HFT
  Bool_t isHFTTrack() const              { return isHft(); }
  /// Return if track has hits in all 4 layers of silicon
  Bool_t hasHft4Layers() const           { return hasPxl1Hit() && hasPxl2Hit() && hasIstHit() && hasSstHit(); }
  /// Return if track has TOF hit
  Bool_t isTofTrack() const              { return (mBTofPidTraitsIndex<0) ? false : true; }
  /// Return if track has BEMC hit
  Bool_t isBemcTrack() const             { return (mBEmcPidTraitsIndex<0) ? false : true; }
  /// Return if track has MTD hit
  Bool_t isMtdTrack() const              { return (mMtdPidTraitsIndex<0) ? false : true; }
  /// Return if track has ETOF hit
  Bool_t isETofTrack() const             { return (mETofPidTraitsIndex<0) ? false : true; }

  /// Return if track is primary
  Bool_t isPrimary() const               { return ( pMom().Mag()>0 ); }

  /// Return index to the corresponding BEMC PID trait
  Int_t bemcPidTraitsIndex() const       { return mBEmcPidTraitsIndex; }
  /// Return index to the corresponding BTOF PID trait
  Int_t bTofPidTraitsIndex() const       { return mBTofPidTraitsIndex; }
  /// Return index to the corresponding MTD PID trait
  Int_t mtdPidTraitsIndex() const        { return mMtdPidTraitsIndex; }
  /// Return index to the corresponding ETOF PID trait
  Int_t eTofPidTraitsIndex() const       { return mETofPidTraitsIndex; }

  //
  // Setters
  //

  /// Set track ID
  void setId(Int_t id)                   { mId = (UShort_t)id; }
  /// Set chi2 of the track
  void setChi2(Float_t chi2);
  /// Set momentum of the primary track
  void setPrimaryMomentum(Double_t px, Double_t py, Double_t pz)
  { mPMomentumX = (Float_t)px; mPMomentumY = (Float_t)py; mPMomentumZ = (Float_t)pz; }
  /// Set momentum of the primary track
  void setPrimaryMomentum(Float_t px, Float_t py, Float_t pz)
  { mPMomentumX = px; mPMomentumY = py; mPMomentumZ = pz; }
  /// Set momentum of the primary track
  void setPrimaryMomentum(TVector3 mom)
  { mPMomentumX = (Float_t)mom.X(); mPMomentumY = (Float_t)mom.Y(); mPMomentumZ = (Float_t)mom.Z(); }
  /// Set momentum of the global track
  void setGlobalMomentum(Double_t px, Double_t py, Double_t pz)
  { mGMomentumX = (Float_t)px; mGMomentumY = (Float_t)py; mGMomentumZ = (Float_t)pz; }
  /// Set momentum of the global track
  void setGlobalMomentum(Float_t px, Float_t py, Float_t pz)
  { mGMomentumX = px; mGMomentumY = py; mGMomentumZ = pz; }
  /// Set momentum of the global track
  void setGlobalMomentum(TVector3 mom)
  { mGMomentumX = (Float_t)mom.X(); mGMomentumY = (Float_t)mom.Y(); mGMomentumZ = (Float_t)mom.Z(); }
  /// Set origin of the track (DCA point to the primary vertex)
  void setOrigin(Double_t x, Double_t y, Double_t z)
  { mOriginX = (Float_t)x; mOriginY = (Float_t)y; mOriginZ = (Float_t)z; }
  /// Set origin of the track (DCA point to the primary vertex)
  void setOrigin(Float_t x, Float_t y, Float_t z)
  { mOriginX = x; mOriginY = y; mOriginZ = z; }
  /// Set origin of the track (DCA point to the primary vertex)
  void setOrigin(TVector3 orig)
  { mOriginX = (Float_t)orig.X(); mOriginY = (Float_t)orig.Y(); mOriginZ = (Float_t)orig.Z(); }

  /// Set dE/dx of the track
  void setDedx(Float_t dEdx);
  /// Set dE/dx error of the track
  void setDedxError(Float_t dEdxError)     { mDedxError = dEdxError; }
  /// Set nHitsFit ( charge * nHitsFit )
  void setNHitsFit(Int_t nhits)            { mNHitsFit = (Char_t)nhits; }
  /// Set nHitsPoss
  void setNHitsPossible(Int_t nhits);
  /// Set nHitsPoss
  void setNHitsMax(Int_t nhits);
  /// Set nHitsDedx
  void setNHitsDedx(Int_t nhits);
  /// Set nSigma(pion)
  void setNSigmaPion(Float_t ns);
  /// Set nSigma(kaon)
  void setNSigmaKaon(Float_t ns);
  /// Set nSigma(proton)
  void setNSigmaProton(Float_t ns);
  /// Set nSigma(electron)
  void setNSigmaElectron(Float_t ns);
  /// Set topology map (2 words)
  void setTopologyMap(Int_t id, UInt_t word);

  /// Set index to BEMC PID traits
  void setBEmcPidTraitsIndex(Int_t index)  { mBEmcPidTraitsIndex = (Short_t)index; }
  /// Set index to BTOF PID traits
  void setBTofPidTraitsIndex(Int_t index)  { mBTofPidTraitsIndex = (Short_t)index; }
  /// Set index to MTD PID traits
  void setMtdPidTraitsIndex(Int_t index)   { mMtdPidTraitsIndex = (Short_t)index; }
  /// Set index to ETOF PID traits
  void setETofPidTraitsIndex(Int_t index)  { mETofPidTraitsIndex = (Short_t)index; }

 protected:

  /// Unique track ID
  UShort_t mId;
  /// Chi2 of the track (encoding = chi2*1000)
  UShort_t mChi2;
  /// Px momentum (GeV/c) of the primary track ( 0 if not primary )
  Float_t mPMomentumX;
  /// Py momentum (GeV/c) of the primary track ( 0 if not primary )
  Float_t mPMomentumY;
  /// Pz momentum (GeV/c) of the primary track ( 0 if not primary )
  Float_t mPMomentumZ;
  /// Px component of the momentum (GeV/c) of the global track at DCA to primary vertex
  Float_t mGMomentumX;
  /// Py component of the momentum (GeV/c) of the global track at DCA to primary vertex
  Float_t mGMomentumY;
  /// Pz component of the momentum (GeV/c) of the global track at DCA to primary vertex
  Float_t mGMomentumZ;
  /// Track origin x (DCAx to the primary vertex) in cm
  Float_t mOriginX;
  /// Track origin y (DCAy to the primary vertex) in cm
  Float_t mOriginY;
  /// Track origin z (DCAy to the primary vertex) in cm
  Float_t mOriginZ;
  
  /// dE/dx in KeV/cm (dE/dx * 1e6)
  Float16_t  mDedx;
  /// dE/dx error (in GeV/cm)
  Float16_t  mDedxError;
  //Float_t  mDnDx;             // fitted dN/dx
  //Float_t  mDnDxError;        // fitted dN/dx error
  
  /// Charge * nHitsFit
  Char_t   mNHitsFit;
  /// Possible number of hits (in TPC)
  UChar_t  mNHitsMax;
  /// Number of hits used for dE/dx estimation (in TPC)
  UChar_t  mNHitsDedx;
  /// nSigma(pion)  (encoding = nsigma * 1000)
  Short_t  mNSigmaPion;
  /// nSigma(kaon)  (encoding = nsigma * 1000)
  Short_t  mNSigmaKaon;
  /// nSigma(proton)  (encoding = nsigma * 1000)
  Short_t  mNSigmaProton;
  /// nSigma(electron)  (encoding = nsigma * 1000)
  Short_t  mNSigmaElectron;
  /// Toplogy Map data0 and data1. See StEvent/StTrackTopologyMap.cxx
  UInt_t   mTopologyMap[2];

  /// Index of the BEMC pidTratis in the event
  Short_t  mBEmcPidTraitsIndex;
  /// Index of the BTOF pidTratis in the event
  Short_t  mBTofPidTraitsIndex;
  /// Index of the MTD  pidTratis in the event
  Short_t  mMtdPidTraitsIndex;
  /// Index of the ETOF pidTratis in the event
  Short_t  mETofPidTraitsIndex;

  ClassDef(StPicoTrack, 5)
};

#endif
