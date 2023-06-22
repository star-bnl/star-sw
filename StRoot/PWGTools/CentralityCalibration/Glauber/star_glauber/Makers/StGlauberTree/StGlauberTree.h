//====================================================================================================
// Glauber MC tree
//   - Event-wise tree contains Npart, Ncoll, eccentricity etc
//   - Header tree contains all relevavnt parameters used in the Glauber MC simulation
//
//   You can access the data members by
//   Get*() functions, like GetNpart()
//
//====================================================================================================
// $Id: StGlauberTree.h,v 1.2 2012/04/25 04:45:26 hmasui Exp $
// $Log: StGlauberTree.h,v $
// Revision 1.2  2012/04/25 04:45:26  hmasui
// Expand branches for eccentricity in the main tree, and deformation parameters in header
//
//====================================================================================================

#ifndef __StGlauberTree_h__
#define __StGlauberTree_h__

class TBranch ;
class TFile ;
class TTree ;
#include "TString.h"

//____________________________________________________________________________________________________
// Class StGlauberTree: MC Glauber ROOT tree (read/write)
class StGlauberTree {
  public:
    // Mode
    //  0     read
    //  1     write
    StGlauberTree(const UInt_t mode=0); /// Default constructor
    virtual ~StGlauberTree(); /// Default destructor

    Int_t Clear() ; /// Clear data members
    Int_t Open(const TString filename) ; /// Open ROOT file, initialize tree
    void  Sort() ; /// Sort outputs in ROOT file
    Int_t Fill() ; /// Fill event-wise tree
    Int_t FillHeader() ; /// Fill header tree
    Int_t Close() ; /// Close ROOT file

    Int_t GetEntries() const ; /// Get entries
    Int_t GetEntry(const Int_t ievent) ; /// Get entry for all branches

    // Setter/Getter for event-wise tree
    void SetB            (const Double_t val) ;
    void SetNpart        (const UInt_t val) ;
    void SetNcoll        (const UInt_t val) ;
    void SetMultiplicity (const UInt_t val) ;
    void SetTheta        (const UInt_t id, const Double_t val) ;
    void SetPhi          (const UInt_t id, const Double_t val) ;
    void SetSumX         (const UInt_t id, const Double_t val) ;
    void SetSumY         (const UInt_t id, const Double_t val) ;
    void SetSumX2        (const UInt_t id, const Double_t val) ;
    void SetSumY2        (const UInt_t id, const Double_t val) ;
    void SetSumXY        (const UInt_t id, const Double_t val) ;
    void SetEccRP2       (const UInt_t id, const Double_t val) ;
    void SetEccPP2       (const UInt_t id, const Double_t val) ;
    void SetEccPP3       (const UInt_t id, const Double_t val) ;
    void SetEccPP4       (const UInt_t id, const Double_t val) ;
    void SetPP2          (const UInt_t id, const Double_t val) ;
    void SetPP3          (const UInt_t id, const Double_t val) ;
    void SetPP4          (const UInt_t id, const Double_t val) ;

    Double_t GetB            () const ;
    UInt_t   GetNpart        () const ;
    UInt_t   GetNcoll        () const ;
    UInt_t   GetMultiplicity () const ;
    Double_t GetTheta        (const UInt_t id) const ; // polar angle rotation
    Double_t GetPhi          (const UInt_t id) const ; // azimuthal angle rotation
    Double_t GetSumX         (const UInt_t id) const ; // {x}
    Double_t GetSumY         (const UInt_t id) const ; // {y}
    Double_t GetSumX2        (const UInt_t id) const ; // {x^2}
    Double_t GetSumY2        (const UInt_t id) const ; // {y^2}
    Double_t GetSumXY        (const UInt_t id) const ; // {xy}
    Double_t GetEccRP2       (const UInt_t id) const ; // 2nd order Reaction plane eccentricity
    Double_t GetEccPP2       (const UInt_t id) const ; // 2nd order Participant plane eccentricity
    Double_t GetEccPP3       (const UInt_t id) const ; // 3rd order Participant plane eccentricity
    Double_t GetEccPP4       (const UInt_t id) const ; // 4th order Participant plane eccentricity
    Double_t GetPP2          (const UInt_t id) const ; // 2nd order participant plane
    Double_t GetPP3          (const UInt_t id) const ; // 3rd order participant plane
    Double_t GetPP4          (const UInt_t id) const ; // 4th order participant plane

    Double_t GetSigmaA2      (const Double_t a2, const Double_t a) const ; // {a^2} - {a}^2
    Double_t GetSigmaXY      (const Double_t xy, const Double_t x, const Double_t y) const ; // {xy} - {x}{y}
    Double_t GetSRP          (const UInt_t id) const ; // Reaction plane area (fm^2)
    Double_t GetSPP          (const UInt_t id) const ; // Participant plane area (fm^2)

    // For header tree
    void SetNameNucleusA      (const Char_t* val) ;
    void SetNameNucleusB      (const Char_t* val) ;
    void SetMassNumberA       (const UInt_t  val) ;
    void SetMassNumberB       (const UInt_t  val) ;
    void SetRadiusA           (const Float_t val) ;
    void SetRadiusB           (const Float_t val) ;
    void SetSkinDepthA        (const Float_t val) ;
    void SetSkinDepthB        (const Float_t val) ;
    void SetBeta2A            (const Float_t val) ;
    void SetBeta4A            (const Float_t val) ;
    void SetBeta2B            (const Float_t val) ;
    void SetBeta4B            (const Float_t val) ;
    void SetSigmaNN           (const Float_t val) ;
    void SetSqrtSNN           (const Float_t val) ;
    void SetRepulsionD        (const Float_t val) ;
    void SetTotalXsec         (const Float_t val) ;
    void SetTotalXsecError    (const Float_t val) ;
    void SetSmearHardCore     (const UInt_t  val) ;
    void SetSmearGaussian     (const UInt_t  val) ;
    void SetCollisionHardCore (const UInt_t  val) ;
    void SetCollisionGaussian (const UInt_t  val) ;
    void SetBMax              (const Float_t val) ;
    void SetNeventsAccept     (const UInt_t val) ;
    void SetNeventsThrow      (const UInt_t val) ;
    void SetNpp               (const Float_t val) ;
    void SetK                 (const Float_t val) ;
    void SetX                 (const Float_t val) ;
    void SetEfficiency        (const Float_t val) ;
    void SetIsConstEfficiency (const UInt_t  val) ;
    void SetVersion           (const UInt_t val) ;

    Char_t* GetNameNucleusA      () ;
    Char_t* GetNameNucleusB      () ;
    UInt_t  GetMassNumberA       () const ;
    UInt_t  GetMassNumberB       () const ;
    Float_t GetRadiusA           () const ;
    Float_t GetRadiusB           () const ;
    Float_t GetSkinDepthA        () const ;
    Float_t GetSkinDepthB        () const ;
    Float_t GetBeta2A            () const ;
    Float_t GetBeta4A            () const ;
    Float_t GetBeta2B            () const ;
    Float_t GetBeta4B            () const ;
    Float_t GetSigmaNN           () const ;
    Float_t GetSqrtSNN           () const ;
    Float_t GetRepulsionD        () const ;
    Float_t GetTotalXsec         () const ;
    Float_t GetTotalXsecError    () const ;
    UInt_t  GetSmearHardCore     () const ;
    UInt_t  GetSmearGaussian     () const ;
    UInt_t  GetCollisionHardCore () const ;
    UInt_t  GetCollisionGaussian () const ;
    Float_t GetBMax              () const ;
    UInt_t  GetNeventsAccept     () const ;
    UInt_t  GetNeventsThrow      () const ;
    Float_t GetNpp               () const ;
    Float_t GetK                 () const ;
    Float_t GetX                 () const ;
    Float_t GetEfficiency        () const ;
    UInt_t  GetIsConstEfficiency () const ;
    UInt_t  GetVersion           () const ;

  private:
    // Initialize branches (in read mode)
    Int_t InitBranch() ;

    const UInt_t mMode ; /// Mode (0=read, 1=write)
    TFile* mFile   ; /// Input/output ROOT file
    TTree* mTree   ; /// MC Glauber event-wise tree
    TTree* mHeader ; /// Header tree to store constants

    // Data members in event-wise tree
    Double_t mB          ; /// Impact parameter
    UInt_t mNpart        ; /// Number of participants
    UInt_t mNcoll        ; /// Number of collisions
    UInt_t mMultiplicity ; /// Two component model multiplicity
    Double_t mTheta[2]   ; /// Polar angle rotation (relevant only for deformed nuclei)
    Double_t mPhi[2]     ; /// Azimuthal angle rotation (relevant only for deformed nuclei)
    Double_t mSumX[4]    ; /// <x> (0:Npart, 1:Ncoll, 2:multiplicity, 3:spectator)
    Double_t mSumY[4]    ; /// <y> (0:Npart, 1:Ncoll, 2:multiplicity, 3:spectator)
    Double_t mSumX2[4]   ; /// <x^2> (0:Npart, 1:Ncoll, 2:multiplicity, 3:spectator)
    Double_t mSumY2[4]   ; /// <y^2> (0:Npart, 1:Ncoll, 2:multiplicity, 3:spectator)
    Double_t mSumXY[4]   ; /// <xy> (0:Npart, 1:Ncoll, 2:multiplicity, 3:spectator)
    Double_t mEccRP2[4]  ; /// 2nd order Reaction plane eccentricity (0:Npart, 1:Ncoll, 2:multiplicity, 3:spectator)
    Double_t mEccPP2[4]  ; /// 2nd order Participant plane eccentricity (0:Npart, 1:Ncoll, 2:multiplicity, 3:spectator)
    Double_t mEccPP3[4]  ; /// 3rd order Participant plane eccentricity (0:Npart, 1:Ncoll, 2:multiplicity, 3:spectator)
    Double_t mEccPP4[4]  ; /// 4th order Participant plane eccentricity (0:Npart, 1:Ncoll, 2:multiplicity, 3:spectator)
    Double_t mPP2[4]     ; /// 2nd order participant plane
    Double_t mPP3[4]     ; /// 3rd order participant plane
    Double_t mPP4[4]     ; /// 4th order participant plane

    // Data members in header
    Char_t  mNameNucleusA[4]   ; /// Name of nucleus A (like 'Au')
    Char_t  mNameNucleusB[4]   ; /// Name of nucleus B (like 'Au')
    UInt_t  mMassNumberA       ; /// Mass number of nucleus A
    UInt_t  mMassNumberB       ; /// Mass number of nucleus B
    Float_t mRadiusA           ; /// Radius of nucleus A (fm)
    Float_t mRadiusB           ; /// Radius of nucleus B (fm)
    Float_t mSkinDepthA        ; /// Skin depth of nucleus A (fm)
    Float_t mSkinDepthB        ; /// Skin depth of nucleus B (fm)
    Float_t mBeta2A            ; /// 2nd order deformation parameter for nucleus A
    Float_t mBeta4A            ; /// 4th order deformation parameter for nucleus A
    Float_t mBeta2B            ; /// 2nd order deformation parameter for nucleus B
    Float_t mBeta4B            ; /// 4th order deformation parameter for nucleus B
    Float_t mSigmaNN           ; /// Inelastic nucleon-nucleon cross section (mb)
    Float_t mSqrtSNN           ; /// Center of mass energy (GeV)
    Float_t mRepulsionD        ; /// Repulsion distance (fm)
    Float_t mTotalXsec         ; /// Total geometrical cross section (mb)
    Float_t mTotalXsecError    ; /// Error of total geometrical cross section (mb)
    UInt_t  mSmearHardCore     ; /// Flag for hard-core smearing (1=ON)
    UInt_t  mSmearGaussian     ; /// Flag for gaussian smearing (1=ON)
    UInt_t  mCollisionHardCore ; /// Flag for hard-core collision profile (1=ON)
    UInt_t  mCollisionGaussian ; /// Flag for gaussian collision profile (1=ON)
    Float_t mBMax              ; /// Maximum impact parameter
    UInt_t  mNeventsAccept     ; /// Number of accepted events
    UInt_t  mNeventsThrow      ; /// Number of total events thrown
    Float_t mNpp               ; /// Npp parameter in NBD
    Float_t mK                 ; /// k parameter in NBD
    Float_t mX                 ; /// x parameter in NBD (fraction of hard component)
    Float_t mEfficiency        ; /// Efficiency or d value used in NBD
    UInt_t  mIsConstEfficiency ; /// Constant efficiency(=1) or multiplicity dependent efficiency(=0)
    UInt_t  mVersion           ; /// Version of StFastGlauberMcMaker

    // Branches
    TBranch*  b_b      ;
    TBranch*  b_npart  ;
    TBranch*  b_ncoll  ;
    TBranch*  b_mult   ;
    TBranch*  b_theta   ;
    TBranch*  b_phi   ;
    TBranch*  b_sumx   ;
    TBranch*  b_sumy   ;
    TBranch*  b_sumx2  ;
    TBranch*  b_sumy2  ;
    TBranch*  b_sumxy  ;
    TBranch*  b_eccrp2  ;
    TBranch*  b_eccpp2  ;
    TBranch*  b_eccpp3 ;
    TBranch*  b_eccpp4 ;
    TBranch*  b_pp2  ;
    TBranch*  b_pp3 ;
    TBranch*  b_pp4 ;

    TBranch*  b_nameA             ;
    TBranch*  b_nameB             ;
    TBranch*  b_massNumberA       ;
    TBranch*  b_massNumberB       ;
    TBranch*  b_radiusA           ;
    TBranch*  b_radiusB           ;
    TBranch*  b_skinDepthA        ;
    TBranch*  b_skinDepthB        ;
    TBranch*  b_beta2A            ;
    TBranch*  b_beta4A            ;
    TBranch*  b_beta2B            ;
    TBranch*  b_beta4B            ;
    TBranch*  b_sigmaNN           ;
    TBranch*  b_sqrtSNN           ;
    TBranch*  b_repulsionD        ;
    TBranch*  b_totalXsec         ;
    TBranch*  b_totalXsecError    ;
    TBranch*  b_smearHardCore     ;
    TBranch*  b_smearGaussian     ;
    TBranch*  b_collisionHardCore ;
    TBranch*  b_collisionGaussian ;
    TBranch*  b_maxB              ;
    TBranch*  b_neventsAccept     ;
    TBranch*  b_neventsThrow      ;
    TBranch*  b_npp               ;
    TBranch*  b_k                 ;
    TBranch*  b_x                 ;
    TBranch*  b_efficiency        ;
    TBranch*  b_isConstEfficiency ;
    TBranch*  b_version           ;

    ClassDef(StGlauberTree, 1)
};

// Setter for event-wise tree
inline void StGlauberTree::SetB            (const Double_t val) { mB = val ; }
inline void StGlauberTree::SetNpart        (const UInt_t val) { mNpart = val ; }
inline void StGlauberTree::SetNcoll        (const UInt_t val) { mNcoll = val ; }
inline void StGlauberTree::SetMultiplicity (const UInt_t val) { mMultiplicity = val ; }
inline void StGlauberTree::SetTheta        (const UInt_t id, const Double_t val) { mTheta[id] = val ; }
inline void StGlauberTree::SetPhi          (const UInt_t id, const Double_t val) { mPhi[id] = val ; }
inline void StGlauberTree::SetSumX         (const UInt_t id, const Double_t val) { mSumX[id] = val ; }
inline void StGlauberTree::SetSumY         (const UInt_t id, const Double_t val) { mSumY[id] = val ; }
inline void StGlauberTree::SetSumX2        (const UInt_t id, const Double_t val) { mSumX2[id] = val ; }
inline void StGlauberTree::SetSumY2        (const UInt_t id, const Double_t val) { mSumY2[id] = val ; }
inline void StGlauberTree::SetSumXY        (const UInt_t id, const Double_t val) { mSumXY[id] = val ; }
inline void StGlauberTree::SetEccRP2       (const UInt_t id, const Double_t val) { mEccRP2[id] = val ; }
inline void StGlauberTree::SetEccPP2       (const UInt_t id, const Double_t val) { mEccPP2[id] = val ; }
inline void StGlauberTree::SetEccPP3       (const UInt_t id, const Double_t val) { mEccPP3[id] = val ; }
inline void StGlauberTree::SetEccPP4       (const UInt_t id, const Double_t val) { mEccPP4[id] = val ; }
inline void StGlauberTree::SetPP2          (const UInt_t id, const Double_t val) { mPP2[id] = val ; }
inline void StGlauberTree::SetPP3          (const UInt_t id, const Double_t val) { mPP3[id] = val ; }
inline void StGlauberTree::SetPP4          (const UInt_t id, const Double_t val) { mPP4[id] = val ; }

// Getter for event-wise tree
inline Double_t StGlauberTree::GetB            () const { return mB ; }
inline UInt_t   StGlauberTree::GetNpart        () const { return mNpart ; }
inline UInt_t   StGlauberTree::GetNcoll        () const { return mNcoll ; }
inline UInt_t   StGlauberTree::GetMultiplicity () const { return mMultiplicity ; }
inline Double_t StGlauberTree::GetTheta        (const UInt_t id) const { return mTheta[id] ; }
inline Double_t StGlauberTree::GetPhi          (const UInt_t id) const { return mPhi[id] ; }
inline Double_t StGlauberTree::GetSumX         (const UInt_t id) const { return mSumX[id] ; }
inline Double_t StGlauberTree::GetSumY         (const UInt_t id) const { return mSumY[id] ; }
inline Double_t StGlauberTree::GetSumX2        (const UInt_t id) const { return mSumX2[id] ; }
inline Double_t StGlauberTree::GetSumY2        (const UInt_t id) const { return mSumY2[id] ; }
inline Double_t StGlauberTree::GetSumXY        (const UInt_t id) const { return mSumXY[id] ; }
inline Double_t StGlauberTree::GetEccRP2       (const UInt_t id) const { return mEccRP2[id] ; }
inline Double_t StGlauberTree::GetEccPP2       (const UInt_t id) const { return mEccPP2[id] ; }
inline Double_t StGlauberTree::GetEccPP3       (const UInt_t id) const { return mEccPP3[id] ; }
inline Double_t StGlauberTree::GetEccPP4       (const UInt_t id) const { return mEccPP4[id] ; }
inline Double_t StGlauberTree::GetPP2          (const UInt_t id) const { return mPP2[id] ; }
inline Double_t StGlauberTree::GetPP3          (const UInt_t id) const { return mPP3[id] ; }
inline Double_t StGlauberTree::GetPP4          (const UInt_t id) const { return mPP4[id] ; }

// Setter for header tree
inline void StGlauberTree::SetMassNumberA       (const UInt_t  val) { mMassNumberA       = val ; }
inline void StGlauberTree::SetMassNumberB       (const UInt_t  val) { mMassNumberB       = val ; }
inline void StGlauberTree::SetRadiusA           (const Float_t val) { mRadiusA           = val ; }
inline void StGlauberTree::SetRadiusB           (const Float_t val) { mRadiusB           = val ; }
inline void StGlauberTree::SetSkinDepthA        (const Float_t val) { mSkinDepthA        = val ; }
inline void StGlauberTree::SetSkinDepthB        (const Float_t val) { mSkinDepthB        = val ; }
inline void StGlauberTree::SetBeta2A            (const Float_t val) { mBeta2A            = val ; }
inline void StGlauberTree::SetBeta4A            (const Float_t val) { mBeta4A            = val ; }
inline void StGlauberTree::SetBeta2B            (const Float_t val) { mBeta2B            = val ; }
inline void StGlauberTree::SetBeta4B            (const Float_t val) { mBeta4B            = val ; }
inline void StGlauberTree::SetSigmaNN           (const Float_t val) { mSigmaNN           = val ; }
inline void StGlauberTree::SetSqrtSNN           (const Float_t val) { mSqrtSNN           = val ; }
inline void StGlauberTree::SetRepulsionD        (const Float_t val) { mRepulsionD        = val ; }
inline void StGlauberTree::SetTotalXsec         (const Float_t val) { mTotalXsec         = val ; }
inline void StGlauberTree::SetTotalXsecError    (const Float_t val) { mTotalXsecError    = val ; }
inline void StGlauberTree::SetSmearHardCore     (const UInt_t  val) { mSmearHardCore     = val ; }
inline void StGlauberTree::SetSmearGaussian     (const UInt_t  val) { mSmearGaussian     = val ; }
inline void StGlauberTree::SetCollisionHardCore (const UInt_t  val) { mCollisionHardCore = val ; }
inline void StGlauberTree::SetCollisionGaussian (const UInt_t  val) { mCollisionGaussian = val ; }
inline void StGlauberTree::SetBMax              (const Float_t val) { mBMax              = val ; }
inline void StGlauberTree::SetNeventsAccept     (const UInt_t  val) { mNeventsAccept     = val ; }
inline void StGlauberTree::SetNeventsThrow      (const UInt_t  val) { mNeventsThrow      = val ; }
inline void StGlauberTree::SetNpp               (const Float_t val) { mNpp               = val ; }
inline void StGlauberTree::SetK                 (const Float_t val) { mK                 = val ; }
inline void StGlauberTree::SetX                 (const Float_t val) { mX                 = val ; }
inline void StGlauberTree::SetEfficiency        (const Float_t val) { mEfficiency        = val ; }
inline void StGlauberTree::SetIsConstEfficiency (const UInt_t  val) { mIsConstEfficiency = val ; }
inline void StGlauberTree::SetVersion           (const UInt_t  val) { mVersion           = val ; }

inline Char_t* StGlauberTree::GetNameNucleusA      () { return mNameNucleusA      ; }
inline Char_t* StGlauberTree::GetNameNucleusB      () { return mNameNucleusB      ; }
inline UInt_t  StGlauberTree::GetMassNumberA       () const { return mMassNumberA       ; }
inline UInt_t  StGlauberTree::GetMassNumberB       () const { return mMassNumberB       ; }
inline Float_t StGlauberTree::GetRadiusA           () const { return mRadiusA           ; }
inline Float_t StGlauberTree::GetRadiusB           () const { return mRadiusB           ; }
inline Float_t StGlauberTree::GetSkinDepthA        () const { return mSkinDepthA        ; }
inline Float_t StGlauberTree::GetSkinDepthB        () const { return mSkinDepthB        ; }
inline Float_t StGlauberTree::GetBeta2A            () const { return mBeta2A            ; }
inline Float_t StGlauberTree::GetBeta4A            () const { return mBeta4A            ; }
inline Float_t StGlauberTree::GetBeta2B            () const { return mBeta2B            ; }
inline Float_t StGlauberTree::GetBeta4B            () const { return mBeta4B            ; }
inline Float_t StGlauberTree::GetSigmaNN           () const { return mSigmaNN           ; }
inline Float_t StGlauberTree::GetSqrtSNN           () const { return mSqrtSNN           ; }
inline Float_t StGlauberTree::GetRepulsionD        () const { return mRepulsionD        ; }
inline Float_t StGlauberTree::GetTotalXsec         () const { return mTotalXsec         ; }
inline Float_t StGlauberTree::GetTotalXsecError    () const { return mTotalXsecError    ; }
inline UInt_t  StGlauberTree::GetSmearHardCore     () const { return mSmearHardCore     ; }
inline UInt_t  StGlauberTree::GetSmearGaussian     () const { return mSmearGaussian     ; }
inline UInt_t  StGlauberTree::GetCollisionHardCore () const { return mCollisionHardCore ; }
inline UInt_t  StGlauberTree::GetCollisionGaussian () const { return mCollisionGaussian ; }
inline Float_t StGlauberTree::GetBMax              () const { return mBMax              ; }
inline UInt_t  StGlauberTree::GetNeventsAccept     () const { return mNeventsAccept     ; }
inline UInt_t  StGlauberTree::GetNeventsThrow      () const { return mNeventsThrow      ; }
inline Float_t StGlauberTree::GetNpp               () const { return mNpp               ; }
inline Float_t StGlauberTree::GetK                 () const { return mK                 ; }
inline Float_t StGlauberTree::GetX                 () const { return mX                 ; }
inline Float_t StGlauberTree::GetEfficiency        () const { return mEfficiency        ; }
inline UInt_t  StGlauberTree::GetIsConstEfficiency () const { return mIsConstEfficiency ; }
inline UInt_t  StGlauberTree::GetVersion           () const { return mVersion           ; }


#endif

