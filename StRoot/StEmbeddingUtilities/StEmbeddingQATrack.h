//----------------------------------------------------------------------------------------------------
//  Class StEmbeddingQATrack
//    Store relevant track informations for the embedding/real data QA
//----------------------------------------------------------------------------------------------------
/****************************************************************************************************
 * $Id: StEmbeddingQATrack.h,v 1.5 2009/12/22 21:39:31 hmasui Exp $
 * $Log: StEmbeddingQATrack.h,v $
 * Revision 1.5  2009/12/22 21:39:31  hmasui
 * Add comments for functions and members
 *
 ****************************************************************************************************/

#ifndef _StEmbeddingQATrack_h__
#define _StEmbeddingQATrack_h__

#include "TString.h"
#include "StLorentzVectorD.hh"

class StTinyMcTrack ;
class StMiniMcPair ;
class StContamPair ;
class StMuTrack ;

class StEmbeddingQATrack {
  public:
    /// Default constructor
    StEmbeddingQATrack();

    /// Constructor for Monte Carlo (MC) tracks
    StEmbeddingQATrack(const TString name, const StTinyMcTrack& track) ;

    /// Constructor for Matched pairs
    StEmbeddingQATrack(const TString name, StMiniMcPair* track) ;

    /// Constructor for Contaminated pairs
    StEmbeddingQATrack(const TString name, StContamPair* track) ;

    /// Constructor for real tracks both Primary and Global tracks
    //  Need to put the geantid by hand 
    //  in order to calculate rapidity (and maybe invariant mass if you like)
    //  since the real tracks don't have geantid
    StEmbeddingQATrack(const TString name, const StMuTrack& track, const Short_t geantid);

    /// Destructor
    virtual ~StEmbeddingQATrack();

    Bool_t isPtAndEtaOk() const ;  /// Pt and eta cuts

    Bool_t isNHitOk() const ;      /// Nhits cut
    Bool_t isDcaOk() const ;       /// Dca cut
    Bool_t isCommonHitOk() const ; /// Common hit cut

    /// NSigma cut for electrons, pions, kaons and protons
    // if ( real tracks ) return |nSigma| < 2 
    // else               return true (i.e. no nSigma cut)
    Bool_t isNSigmaOk(const Short_t geantid) const ;

    StLorentzVectorD getVectorMc() const ; /// Get MC 4-momentum
    StLorentzVectorD getVectorRc() const ; /// Get reconstructed 4-momentum

    Short_t getNCommonHit()    const ; /// Get number of common hits
    Short_t getParentGeantId() const ; /// Get parent geant id
    Short_t getGeantId()       const ; /// Get geant id
    Short_t getNHit()          const ; /// Get number of fit points
    Short_t getNHitPoss()      const ; /// Get maximum number of fit points
    Short_t getCharge()        const ; /// Get charge

    Double_t getMassMc()         const ; /// Get MC particle mass
    Float_t getPtMc()            const ; /// Get MC transverse momentum
    Float_t getPxMc()            const ; /// Get MC px
    Float_t getPyMc()            const ; /// Get MC py
    Float_t getPzMc()            const ; /// Get MC pz
    Float_t getPMc()             const ; /// Get MC momentum
    Float_t getEtaMc()           const ; /// Get MC pseudorapidity
    Double_t getMassRc()         const ; /// Get reconstructed particle mass
    Float_t getPtRc()            const ; /// Get reconstructed transverse momentum
    Float_t getPxRc()            const ; /// Get reconstructed px
    Float_t getPyRc()            const ; /// Get reconstructed py
    Float_t getPzRc()            const ; /// Get reconstructed pz
    Float_t getPRc()             const ; /// Get reconstructed momentum
    Float_t getEtaRc()           const ; /// Get reconstructed pseudorapidity

    Float_t getPhi()           const ; /// Get azimuthal angle
    Float_t getdEdx()          const ; /// Get dE/dx
    Float_t getdEdxkeV()       const ; /// Get dE/dx in keV unit
    Float_t getDcaGl()         const ; /// Get global dca

    // nSigma (e, pi, K, p) for real data
    Double_t getNSigmaElectron() const ; /// Get nsigma for electrons/positrons
    Double_t getNSigmaPion()     const ; /// Get nsigma for pions
    Double_t getNSigmaKaon()     const ; /// Get nsigma for kaons
    Double_t getNSigmaProton()   const ; /// Get nsigma for protons/anti-protons

    /// Print track informations
    void print() const ;

    /// Get track node name
    const TString getName() const ;

  private:
    static const Float_t kPtMinCut ;   /// Minimum pt cut
    static const Float_t kPtMaxCut ;   /// Maximum pt cut
    static const Float_t kEtaCut ;     /// Maximum eta cut
    static const Short_t kNHitCut ;    /// NHit cut
    static const Float_t kDcaCut ;     /// Dca cut
    static const Double_t kNSigmaCut ; /// NSigma cut

    const Short_t mNCommonHit ;        /// Number of common hits
    const Short_t mParentGeantId ;     /// Parent geant id
    const Short_t mGeantId ;           /// geant id
    const Short_t mNHit ;              /// Number of fit points
    const Short_t mNHitPoss ;          /// Number of maximum fit points
    const Short_t mCharge ;            /// Charge
    const StLorentzVectorD mVectorMc ; /// MC 4-momentum
    const StLorentzVectorD mVectorRc ; /// Reconstructed 4-momentum
    const Float_t mPhi ;               /// Azimuthal angle
    const Float_t mdEdx ;              /// dE/dx
    const Float_t mDcaGl ;             /// Global dca
    const Double_t mNSigmaElectron ;   /// Nsigma for electrons/positrons
    const Double_t mNSigmaPion ;       /// Nsigma for pions
    const Double_t mNSigmaKaon ;       /// Nsigma for kaons
    const Double_t mNSigmaProton ;     /// Nsigma for protons/anti-protons

    TString mName ; /// Track name

    ClassDef(StEmbeddingQATrack, 1)
};

inline Short_t StEmbeddingQATrack::getNCommonHit()    const { return mNCommonHit ; }
inline Short_t StEmbeddingQATrack::getParentGeantId() const { return mParentGeantId ; }
inline Short_t StEmbeddingQATrack::getGeantId()       const { return mGeantId ; }
inline Short_t StEmbeddingQATrack::getNHit()          const { return mNHit ; }
inline Short_t StEmbeddingQATrack::getNHitPoss()      const { return mNHitPoss ; }
inline Short_t StEmbeddingQATrack::getCharge()        const { return mCharge ; }

inline Double_t StEmbeddingQATrack::getMassMc()       const { return mVectorMc.m() ; }
inline Float_t StEmbeddingQATrack::getPMc()           const { return mVectorMc.vect().mag() ; }
inline Float_t StEmbeddingQATrack::getPtMc()          const { return mVectorMc.perp() ; }
inline Float_t StEmbeddingQATrack::getPxMc()          const { return mVectorMc.x() ; }
inline Float_t StEmbeddingQATrack::getPyMc()          const { return mVectorMc.y() ; }
inline Float_t StEmbeddingQATrack::getPzMc()          const { return mVectorMc.z() ; }
inline Float_t StEmbeddingQATrack::getEtaMc()         const { return mVectorMc.pseudoRapidity() ; }
inline Double_t StEmbeddingQATrack::getMassRc()       const { return mVectorRc.m() ; }
inline Float_t StEmbeddingQATrack::getPRc()           const { return mVectorRc.vect().mag() ; }
inline Float_t StEmbeddingQATrack::getPtRc()          const { return mVectorRc.perp() ; }
inline Float_t StEmbeddingQATrack::getPxRc()          const { return mVectorRc.x() ; }
inline Float_t StEmbeddingQATrack::getPyRc()          const { return mVectorRc.y() ; }
inline Float_t StEmbeddingQATrack::getPzRc()          const { return mVectorRc.z() ; }
inline Float_t StEmbeddingQATrack::getEtaRc()         const { return mVectorRc.pseudoRapidity() ; }

inline Float_t StEmbeddingQATrack::getPhi()           const { return mPhi ; }
inline Float_t StEmbeddingQATrack::getdEdx()          const { return mdEdx ; }
inline Float_t StEmbeddingQATrack::getdEdxkeV()       const { return mdEdx * 1.0e+06 ; }
inline Float_t StEmbeddingQATrack::getDcaGl()         const { return mDcaGl ; }

inline const TString StEmbeddingQATrack::getName() const { return mName ; }

#endif


