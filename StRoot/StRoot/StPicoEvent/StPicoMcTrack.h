/**
 * \class StPicoMcTrack
 * \brief Holds information about Monte Carlo track parameters
 *
 * The class stores information about the Monte Carlo tracks
 */

#ifndef StPicoMcTrack_h
#define StPicoMcTrack_h

// C++ headers
#include <vector>

// ROOT headers
#include "TObject.h"
#include "TVector3.h"
#include "TLorentzVector.h"

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
class StPicoMcTrack : public TObject {

 public:

  /// Default constructor
  StPicoMcTrack();
  /// Copy constructor
  StPicoMcTrack(const StPicoMcTrack &track);
  /// Destructor
  virtual ~StPicoMcTrack();
  /// Print MC track parameters
  virtual void Print(const Char_t *option = "") const;

  /// Detector names
  enum EHIT {ktpc, ksvt, kssd, kctb, keem, kemc, kesm, 
             kftp, kgem, khpd, kist, kigt, kfst, kfgt, 
             kfpd, kmwc, kpgc, kpmd, ksmd, kpix, ktof, 
             kvpd, ktot};

  //
  // Getters
  //

  /// Return MC track ID (GEANT track ID)
  Int_t id() const                    { return mId; }
  /// Return particle ID defined by PDG
  Int_t pdgId() const;
  /// Return particle ID defined by GEANT (accordingly to GPART)
  Int_t geantId() const               { return (Int_t)mGePid; }
  /// Return charge of the particle
  Int_t charge() const                { return (Int_t)mCharge; }
  /// Return track three-momentum
  TVector3 p() const                  { return TVector3(mPx, mPy, mPz); }
  /// Return pT of the track
  Float_t pt() const                  { return p().Perp(); }
  /// Return track total momentum
  Float_t ptot() const                { return p().Mag(); }
  /// Return pseudorapidity of the track
  Float_t eta() const                 { return p().PseudoRapidity(); }  
  /// Four-momentum of the track
  TLorentzVector fourMomentum() const { return TLorentzVector(mPx,mPy,mPz,mE); }
  /// Energy of the track
  Float_t energy() const              { return mE; }
  /// Rapidity of the track
  Float_t rapidity() const            { return fourMomentum().Rapidity(); }
  /// Check if track is from shower
  Bool_t isFromShower() const         { return mIsFromShower; }
  /// ID of start MC vertex
  Int_t idVtxStart() const            { return mIdVtxStart; }
  /// ID of stop MC vertex
  Int_t idVtxStop() const             { return mIdVtxStop; }
  /// ID of intermediate MC vertex
  Int_t idVtxItrmd() const            { return mIdVtxItrmd; }
  /// Return total number of hits
  Int_t nHits() const               
  { Int_t n=0; for (Int_t i=ktpc; i<ktot; i++) n+=nHits(i); return n; }
  /// Return number of hits in a given detector
  UChar_t nHits(Int_t k) const        { return mHits[k]; }
  /// Return number of hits in CTB
  UChar_t nHitsCtb() const            { return mHits[kctb]; }
  /// Return number of hits in EEMC
  UChar_t nHitsEEmc() const           { return mHits[keem]; }
  /// Return number of hits in BEMC
  UChar_t nHitsBEmc() const           { return mHits[kemc]; }
  /// Return number of hits in EEMC shower max detector
  UChar_t nHitsEsm() const            { return mHits[kesm]; }
  /// Return number of hits in forward TPC
  UChar_t nHitsFtpc() const           { return mHits[kftp]; }
  /// Return number of hits in barrel GEM
  UChar_t nHitsGem() const            { return mHits[kgem]; }
  /// Return number of hits in HPD
  UChar_t nHitsHpd() const            { return mHits[khpd]; }
  /// Return number of hits in IST
  UChar_t nHitsIst() const            { return mHits[kist]; }
  /// Return number of hits in IGT
  UChar_t nHitsIgt() const            { return mHits[kigt]; }
  /// Return number of hits in FST
  UChar_t nHitsFst() const            { return mHits[kfst]; }
  /// Return number of hits in FGT
  UChar_t nHitsFgt() const            { return mHits[kfgt]; }
  /// Return number of hits in FPD
  UChar_t nHitsFpd() const            { return mHits[kfpd]; }
  /// Return number of hits in MWC
  UChar_t nHitsMwc() const            { return mHits[kmwc]; }
  /// Return number of hits in PGC
  UChar_t nHitsPgc() const            { return mHits[kpgc]; }
  /// Return number of hits in PMD
  UChar_t nHitsPmd() const            { return mHits[kpmd]; }
  /// Return number of hits in BSMD
  UChar_t nHitsBsmd() const           { return mHits[ksmd]; }
  /// Return number of hits in SSD
  UChar_t nHitsSsd() const            { return mHits[kssd]; }
  /// Return number of hits in SVT
  UChar_t nHitsSvt() const            { return mHits[ksvt]; }
  /// Return number of hits in PXL
  UChar_t nHitsPxl() const            { return mHits[kpix]; }
  /// Return number of hits in TOF
  UChar_t nHitsTof() const            { return mHits[ktof]; }
  /// Return number of hits in TPC
  UChar_t nHitsTpc() const            { return mHits[ktpc]; }
  /// Return number of hits in VPD
  UChar_t nHitsVpd() const            { return mHits[kvpd]; }
  /// Return particle names (GEANT ID according to GPART)
  const Char_t *geName();
  /// Return corrected GePid (to take embedding into account)
  Int_t correctGePid(Int_t id);

  //
  // Setters
  //

  /// Set MC track ID (primary key/GEANT track ID)
  void setId(Int_t id);
  /// Set GEANT ID (according to GPART)
  void setGeantId(Int_t id)           { mGePid = id; }
  /// Set particle charge
  void setCharge(Int_t charge)        { mCharge = (Char_t)charge; }
  /// Set nHits in a detector (iteratively)
  /// \par k Detector iterator (enum)
  /// \par n Number of hits in a given detector
  void setNHits(Int_t k, UChar_t n)     { mHits[k] = n; }
  /// Set three-momentum
  void setP(Float_t px, Float_t py, Float_t pz) { mPx = px; mPy = py; mPz = pz; }
  /// Set energy
  void setE(Float_t e)                { mE = e; }
  /// Set flag if track comes from shower
  void setIsFromShower(Bool_t isFrom) { mIsFromShower = isFrom; }
  /// Set start MC vertex index
  void setIdVtxStart(Int_t id)        { mIdVtxStart = (Short_t)id; }
  /// Set stop MC vertex index
  void setIdVtxStop(Int_t id)         { mIdVtxStop = (Short_t)id; }
  /// Set first intermediate MC vertex index
  void setIdVtxItrmd(Int_t id)        { mIdVtxItrmd = (Short_t)id; }


 private:
  
  /// Primary key (GEANT track ID)
  UShort_t mId;
  /// Particle ID according to GEANT (GPART and GPIONS) + STAR embedding values
  /// http://hep.fi.infn.it/geant.pdf
  Int_t mGePid;
  /// Particle charge
  Char_t mCharge;
  /// Number of hits in a detector
  UChar_t mHits[ktot];
  /// X component of track momentum (GeV/c)
  Float_t mPx;
  /// Y component of track momentum (GeV/c)
  Float_t mPy;
  /// Z component of track momentum (GeV/c)
  Float_t mPz;
  /// Energy of the track (GeV)
  Float_t mE;
  /// Is a shower track
  /// \par 1 Is from shower
  /// \par 0 Not from showe
  Bool_t mIsFromShower;
  /// Id of start MC vertex of track
  Short_t mIdVtxStart;
  /// Id of stop MC vertex of track
  Short_t mIdVtxStop;
  /// First intermediate MC vertex
  Short_t mIdVtxItrmd;

  ClassDef(StPicoMcTrack, 1)
};

#endif // #define StPicoMcTrack_h
