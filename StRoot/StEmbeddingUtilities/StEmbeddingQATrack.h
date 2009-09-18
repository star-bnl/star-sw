
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

    Bool_t IsMc() const ;
    Bool_t IsEmbedding() const ;
    Bool_t IsPtAndEtaOk() const ;

    Bool_t IsNHitOk() const ;
    Bool_t IsDcaOk() const ;
    Bool_t IsCommonHitOk() const ;

    StLorentzVectorD GetVectorMc() const ;
    StLorentzVectorD GetVectorRc() const ;

    Short_t GetNCommonHit()    const ;
    Short_t GetParentGeantId() const ;
    Short_t GetGeantId()       const ;
    Short_t GetNHit()          const ;
    Short_t GetNHitPoss()      const ;
    Short_t GetCharge()        const ;

    Double_t GetMassMc()         const ;
    Float_t GetPtMc()            const ;
    Float_t GetPxMc()            const ;
    Float_t GetPyMc()            const ;
    Float_t GetPzMc()            const ;
    Float_t GetPMc()             const ;
    Float_t GetEtaMc()           const ;
    Double_t GetMassRc()         const ;
    Float_t GetPtRc()            const ;
    Float_t GetPxRc()            const ;
    Float_t GetPyRc()            const ;
    Float_t GetPzRc()            const ;
    Float_t GetPRc()             const ;
    Float_t GetEtaRc()           const ;

    Float_t GetPhi()           const ;
    Float_t GetdEdx()          const ;
    Float_t GetDcaGl()         const ;

    void Print() const ;
    const TString GetName() const ;

  private:
    static const Float_t kPtMinCut ;  // minimum pt cut
    static const Float_t kPtMaxCut ;  // maximum pt cut
    static const Float_t kEtaCut ;    // maximum eta cut
    static const Short_t kNHitCut ;   // NHit cut
    static const Float_t kDcaCut ;    // Dca cut

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
    TString mName ; // Track name

    ClassDef(StEmbeddingQATrack, 1)
};

inline Short_t StEmbeddingQATrack::GetNCommonHit()    const { return mNCommonHit ; }
inline Short_t StEmbeddingQATrack::GetParentGeantId() const { return mParentGeantId ; }
inline Short_t StEmbeddingQATrack::GetGeantId()       const { return mGeantId ; }
inline Short_t StEmbeddingQATrack::GetNHit()          const { return mNHit ; }
inline Short_t StEmbeddingQATrack::GetNHitPoss()      const { return mNHitPoss ; }
inline Short_t StEmbeddingQATrack::GetCharge()        const { return mCharge ; }

inline Double_t StEmbeddingQATrack::GetMassMc()       const { return mVectorMc.m() ; }
inline Float_t StEmbeddingQATrack::GetPMc()           const { return mVectorMc.vect().mag() ; }
inline Float_t StEmbeddingQATrack::GetPtMc()          const { return mVectorMc.perp() ; }
inline Float_t StEmbeddingQATrack::GetPxMc()          const { return mVectorMc.x() ; }
inline Float_t StEmbeddingQATrack::GetPyMc()          const { return mVectorMc.y() ; }
inline Float_t StEmbeddingQATrack::GetPzMc()          const { return mVectorMc.z() ; }
inline Float_t StEmbeddingQATrack::GetEtaMc()         const { return mVectorMc.pseudoRapidity() ; }
inline Double_t StEmbeddingQATrack::GetMassRc()       const { return mVectorRc.m() ; }
inline Float_t StEmbeddingQATrack::GetPRc()           const { return mVectorRc.vect().mag() ; }
inline Float_t StEmbeddingQATrack::GetPtRc()          const { return mVectorRc.perp() ; }
inline Float_t StEmbeddingQATrack::GetPxRc()          const { return mVectorRc.x() ; }
inline Float_t StEmbeddingQATrack::GetPyRc()          const { return mVectorRc.y() ; }
inline Float_t StEmbeddingQATrack::GetPzRc()          const { return mVectorRc.z() ; }
inline Float_t StEmbeddingQATrack::GetEtaRc()         const { return mVectorRc.pseudoRapidity() ; }

inline Float_t StEmbeddingQATrack::GetPhi()           const { return mPhi ; }
inline Float_t StEmbeddingQATrack::GetdEdx()          const { return mdEdx ; }
inline Float_t StEmbeddingQATrack::GetDcaGl()         const { return mDcaGl ; }

inline const TString StEmbeddingQATrack::GetName() const { return mName ; }

#endif


