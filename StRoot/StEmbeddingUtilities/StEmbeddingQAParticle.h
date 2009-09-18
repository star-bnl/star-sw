
#ifndef __StEmbeddingQAParticle_h__
#define __StEmbeddingQAParticle_h__

class StEmbeddingQAParticle {
  public:
    StEmbeddingQAParticle(const Int_t particleId = 8); // default is piplus
    virtual ~StEmbeddingQAParticle();

    Int_t GetParticleId() const ;
    const TString GetName() const ;
    const TString GetTitle() const ;
    Double_t GetMass2() const ;
    Int_t GetCharge() const ;

  private:
    const Int_t kParticleId ; // particle geant3 id
    const TString kName ;     // particle name
    const TString kTitle ;    // particle name (tex format)
    const Double_t kMass2 ;   // mass square
    const Int_t kCharge ;     // charge

    ClassDef(StEmbeddingQAParticle, 1)
};

inline Int_t StEmbeddingQAParticle::GetParticleId()    const { return kParticleId ; }
inline const TString StEmbeddingQAParticle::GetName()  const { return kName ; }
inline const TString StEmbeddingQAParticle::GetTitle() const { return kTitle ; }
inline Double_t StEmbeddingQAParticle::GetMass2()      const { return kMass2 ; }
inline Int_t StEmbeddingQAParticle::GetCharge()        const { return kCharge ; }

#endif

