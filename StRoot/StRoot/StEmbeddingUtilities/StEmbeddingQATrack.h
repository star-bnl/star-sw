//----------------------------------------------------------------------------------------------------
//  Class StEmbeddingQATrack
//    Store relevant track informations for the embedding/real data QA
//----------------------------------------------------------------------------------------------------
/****************************************************************************************************
 * $Id: StEmbeddingQATrack.h,v 1.13 2019/07/10 05:46:22 zhux Exp $
 * $Log: StEmbeddingQATrack.h,v $
 * Revision 1.13  2019/07/10 05:46:22  zhux
 * added option for btof pid for primary real tracks
 *
 * Revision 1.12  2011/04/01 05:00:18  hmasui
 * Move track cuts into StEmbeddingQAUtilities, added global momentum for embedding
 *
 * Revision 1.11  2011/02/11 23:21:28  hmasui
 * Added missing functions getNSigma...()
 *
 * Revision 1.10  2011/02/11 03:55:48  hmasui
 * Change geantid type to integer
 *
 * Revision 1.9  2011/01/12 21:36:17  hmasui
 * Add nHitsFit/nHitsPoss cut
 *
 * Revision 1.8  2010/05/14 19:49:07  hmasui
 * Add rapidity cut
 *
 * Revision 1.7  2010/04/24 20:20:11  hmasui
 * Add geant process, and modift the type of parent, parent-parent geantid to match the data members in minimc tree
 *
 * Revision 1.6  2010/02/16 02:11:49  hmasui
 * Add parent-parent geantid
 *
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
    StEmbeddingQATrack(const TString name, const StMuTrack& track, const Int_t geantid, const Bool_t btof);

    /// Destructor
    virtual ~StEmbeddingQATrack();

    Bool_t isPtAndEtaOk() const ;  /// Pt and eta cuts
    Bool_t isRapidityOk(const Double_t ycut) const ; /// Rapidity cut

    Bool_t isNHitOk() const ;      /// Nhits cut
    Bool_t isNHitToNPossOk() const ; /// Nhits/NhitsPoss cut
    Bool_t isDcaOk() const ;       /// Dca cut
    Bool_t isCommonHitOk() const ; /// Common hit cut

    /// NSigma cut for electrons, pions, kaons and protons
    // if ( real tracks ) return |nSigma| < 2 
    // else               return true (i.e. no nSigma cut)
    Bool_t isNSigmaOk(const Int_t geantid) const ;

    StLorentzVectorD getVectorMc() const ; /// Get MC 4-momentum
    StLorentzVectorD getVectorRc() const ; /// Get reconstructed 4-momentum (primary)
    StLorentzVectorD getVectorPr() const ; /// Get reconstructed 4-momentum (primary <- return getVectorRc())
    StLorentzVectorD getVectorGl() const ; /// Get reconstructed 4-momentum (global)

    Short_t getNCommonHit()          const ; /// Get number of common hits
    Int_t   getParentParentGeantId() const ; /// Get parent geant id
    Int_t   getParentGeantId()       const ; /// Get parent geant id
    Int_t   getGeantId()             const ; /// Get geant id
    Int_t   getGeantProcess()        const ; /// Get geant process
    Short_t getNHit()                const ; /// Get number of fit points
    Short_t getNHitPoss()            const ; /// Get maximum number of fit points
    Short_t getCharge()              const ; /// Get charge

    Double_t getMassMc()         const ; /// Get MC particle mass
    Float_t getPtMc()            const ; /// Get MC transverse momentum
    Float_t getPxMc()            const ; /// Get MC px
    Float_t getPyMc()            const ; /// Get MC py
    Float_t getPzMc()            const ; /// Get MC pz
    Float_t getPMc()             const ; /// Get MC momentum
    Float_t getEtaMc()           const ; /// Get MC pseudorapidity
    Float_t getRapidityMc()      const ; /// Get MC rapidity
    Double_t getMassRc()         const ; /// Get reconstructed particle mass (primary)
    Float_t getPtRc()            const ; /// Get reconstructed transverse momentum (primary)
    Float_t getPxRc()            const ; /// Get reconstructed px (primary)
    Float_t getPyRc()            const ; /// Get reconstructed py (primary)
    Float_t getPzRc()            const ; /// Get reconstructed pz (primary)
    Float_t getPRc()             const ; /// Get reconstructed momentum (primary)
    Float_t getEtaRc()           const ; /// Get reconstructed pseudorapidity (primary)
    Float_t getRapidityRc()      const ; /// Get reconstructed rapidity (primary)

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
    const Short_t mNCommonHit ;           /// Number of common hits
    const Int_t   mParentParentGeantId ;  /// Parent-parent geant id
    const Int_t   mParentGeantId ;        /// Parent geant id
    const Int_t   mGeantId ;              /// geant id
    const Int_t   mGeantProcess ;         /// geant process
    const Short_t mNHit ;                 /// Number of fit points
    const Short_t mNHitPoss ;             /// Number of maximum fit points
    const Short_t mCharge ;               /// Charge
    const StLorentzVectorD mVectorMc ;    /// MC 4-momentum
    const StLorentzVectorD mVectorRc ;    /// Reconstructed 4-momentum (primary)
    const StLorentzVectorD mVectorGl ;    /// Reconstructed 4-momentum (global)
    const Float_t mPhi ;                  /// Azimuthal angle
    const Float_t mdEdx ;                 /// dE/dx
    const Float_t mDcaGl ;                /// Global dca
    const Double_t mNSigmaElectron ;      /// Nsigma for electrons/positrons
    const Double_t mNSigmaPion ;          /// Nsigma for pions
    const Double_t mNSigmaKaon ;          /// Nsigma for kaons
    const Double_t mNSigmaProton ;        /// Nsigma for protons/anti-protons

    TString mName ; /// Track name

    ClassDef(StEmbeddingQATrack, 2)
};

inline Short_t StEmbeddingQATrack::getNCommonHit()          const { return mNCommonHit ; }
inline Int_t   StEmbeddingQATrack::getParentParentGeantId() const { return mParentParentGeantId ; }
inline Int_t   StEmbeddingQATrack::getParentGeantId()       const { return mParentGeantId ; }
inline Int_t   StEmbeddingQATrack::getGeantId()             const { return mGeantId ; }
inline Int_t   StEmbeddingQATrack::getGeantProcess()        const { return mGeantProcess ; }
inline Short_t StEmbeddingQATrack::getNHit()                const { return mNHit ; }
inline Short_t StEmbeddingQATrack::getNHitPoss()            const { return mNHitPoss ; }
inline Short_t StEmbeddingQATrack::getCharge()              const { return mCharge ; }

inline Double_t StEmbeddingQATrack::getMassMc()       const { return mVectorMc.m() ; }
inline Float_t StEmbeddingQATrack::getPMc()           const { return mVectorMc.vect().mag() ; }
inline Float_t StEmbeddingQATrack::getPtMc()          const { return mVectorMc.perp() ; }
inline Float_t StEmbeddingQATrack::getPxMc()          const { return mVectorMc.x() ; }
inline Float_t StEmbeddingQATrack::getPyMc()          const { return mVectorMc.y() ; }
inline Float_t StEmbeddingQATrack::getPzMc()          const { return mVectorMc.z() ; }
inline Float_t StEmbeddingQATrack::getEtaMc()         const { return mVectorMc.pseudoRapidity() ; }
inline Float_t StEmbeddingQATrack::getRapidityMc()    const { return mVectorMc.rapidity() ; }
inline Double_t StEmbeddingQATrack::getMassRc()       const { return mVectorRc.m() ; }
inline Float_t StEmbeddingQATrack::getPRc()           const { return mVectorRc.vect().mag() ; }
inline Float_t StEmbeddingQATrack::getPtRc()          const { return mVectorRc.perp() ; }
inline Float_t StEmbeddingQATrack::getPxRc()          const { return mVectorRc.x() ; }
inline Float_t StEmbeddingQATrack::getPyRc()          const { return mVectorRc.y() ; }
inline Float_t StEmbeddingQATrack::getPzRc()          const { return mVectorRc.z() ; }
inline Float_t StEmbeddingQATrack::getEtaRc()         const { return mVectorRc.pseudoRapidity() ; }
inline Float_t StEmbeddingQATrack::getRapidityRc()    const { return mVectorRc.rapidity() ; }

inline Float_t StEmbeddingQATrack::getPhi()           const { return mPhi ; }
inline Float_t StEmbeddingQATrack::getdEdx()          const { return mdEdx ; }
inline Float_t StEmbeddingQATrack::getdEdxkeV()       const { return mdEdx * 1.0e+06 ; }
inline Float_t StEmbeddingQATrack::getDcaGl()         const { return mDcaGl ; }

inline Double_t StEmbeddingQATrack::getNSigmaElectron() const { return mNSigmaElectron ; }
inline Double_t StEmbeddingQATrack::getNSigmaPion()     const { return mNSigmaPion ; }
inline Double_t StEmbeddingQATrack::getNSigmaKaon()     const { return mNSigmaKaon ; }
inline Double_t StEmbeddingQATrack::getNSigmaProton()   const { return mNSigmaProton ; }

inline const TString StEmbeddingQATrack::getName() const { return mName ; }

#endif


