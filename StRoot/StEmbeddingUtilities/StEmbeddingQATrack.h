//----------------------------------------------------------------------------------------------------
//  Class StEmbeddingQATrack
//
//
//----------------------------------------------------------------------------------------------------

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
    StEmbeddingQATrack();
    StEmbeddingQATrack(const TString name, const StTinyMcTrack& track, const Double_t mass2);
    StEmbeddingQATrack(const TString name, StMiniMcPair* track, const Double_t mass2);
    StEmbeddingQATrack(const TString name, StContamPair* track, const Double_t mass2);
    StEmbeddingQATrack(const TString name, const StMuTrack& track, const Double_t mass2);
    virtual ~StEmbeddingQATrack();

    Bool_t isMc() const ;
    Bool_t isEmbedding() const ;
    Bool_t isReal() const ;
    Bool_t isPtAndEtaOk() const ;

    Bool_t isNHitOk() const ;
    Bool_t isDcaOk() const ;
    Bool_t isCommonHitOk() const ;

    // if ( real tracks ) return |nSigma| < 2 
    // else               return true (i.e. no nSigma cut)
    Bool_t isNSigmaOk(const Int_t particleId) const ;

    StLorentzVectorD getVectorMc() const ;
    StLorentzVectorD getVectorRc() const ;

    Short_t getNCommonHit()    const ;
    Short_t getParentGeantId() const ;
    Short_t getGeantId()       const ;
    Short_t getNHit()          const ;
    Short_t getNHitPoss()      const ;
    Short_t getCharge()        const ;

    Double_t getMassMc()         const ;
    Float_t getPtMc()            const ;
    Float_t getPxMc()            const ;
    Float_t getPyMc()            const ;
    Float_t getPzMc()            const ;
    Float_t getPMc()             const ;
    Float_t getEtaMc()           const ;
    Double_t getMassRc()         const ;
    Float_t getPtRc()            const ;
    Float_t getPxRc()            const ;
    Float_t getPyRc()            const ;
    Float_t getPzRc()            const ;
    Float_t getPRc()             const ;
    Float_t getEtaRc()           const ;

    Float_t getPhi()           const ;
    Float_t getdEdx()          const ;
    Float_t getDcaGl()         const ;

    // nSigma (e, pi, K, p) for real data
    Double_t getNSigmaElectron() const ;
    Double_t getNSigmaPion()     const ;
    Double_t getNSigmaKaon()     const ;
    Double_t getNSigmaProton()   const ;

    void print() const ;
    const TString getName() const ;

  private:
    static const Float_t kPtMinCut ;   // minimum pt cut
    static const Float_t kPtMaxCut ;   // maximum pt cut
    static const Float_t kEtaCut ;     // maximum eta cut
    static const Short_t kNHitCut ;    // NHit cut
    static const Float_t kDcaCut ;     // Dca cut
    static const Double_t kNSigmaCut ; // NSigma cut

    const Short_t mNCommonHit ;
    const Short_t mParentGeantId ;
    const Short_t mGeantId ;
    const Short_t mNHit ;
    const Short_t mNHitPoss ;
    const Short_t mCharge ;
    const StLorentzVectorD mVectorMc ; // MC 4-momentum
    const StLorentzVectorD mVectorRc ; // Reconstructed 4-momentum
    const Float_t mPhi ;
    const Float_t mdEdx ;
    const Float_t mDcaGl ;
    const Double_t mNSigmaElectron ;
    const Double_t mNSigmaPion ;
    const Double_t mNSigmaKaon ;
    const Double_t mNSigmaProton ;
    TString mName ; // Track name

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
inline Float_t StEmbeddingQATrack::getDcaGl()         const { return mDcaGl ; }

inline const TString StEmbeddingQATrack::getName() const { return mName ; }

#endif


