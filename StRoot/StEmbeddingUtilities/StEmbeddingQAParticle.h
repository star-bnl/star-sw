//----------------------------------------------------------------------------------------------------
// Class StEmbeddingQAParticle
//
//   - Single particle class to store
//      particle geantid, particle name (text and tex formats), mass square and charge
//----------------------------------------------------------------------------------------------------

#ifndef __StEmbeddingQAParticle_h__
#define __StEmbeddingQAParticle_h__

class StEmbeddingQAParticle {
  public:
    StEmbeddingQAParticle(const Int_t particleId = 8); // default is piplus
    virtual ~StEmbeddingQAParticle();

    Int_t getParticleId() const ;
    const TString getName() const ;
    const TString getTitle() const ;
    Double_t getMass2() const ;
    Int_t getCharge() const ;

  private:
    const Int_t kParticleId ; // particle geant3 id
    const TString kName ;     // particle name
    const TString kTitle ;    // particle name (tex format)
    const Double_t kMass2 ;   // mass square
    const Int_t kCharge ;     // charge

    ClassDef(StEmbeddingQAParticle, 1)
};

inline Int_t StEmbeddingQAParticle::getParticleId()    const { return kParticleId ; }
inline const TString StEmbeddingQAParticle::getName()  const { return kName ; }
inline const TString StEmbeddingQAParticle::getTitle() const { return kTitle ; }
inline Double_t StEmbeddingQAParticle::getMass2()      const { return kMass2 ; }
inline Int_t StEmbeddingQAParticle::getCharge()        const { return kCharge ; }

#endif

