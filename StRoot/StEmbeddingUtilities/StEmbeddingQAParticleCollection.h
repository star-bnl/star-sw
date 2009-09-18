
#ifndef __StEmbeddingQAParticleCollection_h__
#define __StEmbeddingQAParticleCollection_h__

#include <vector>
#include "TString.h"
class StEmbeddingQAParticle ;

class StEmbeddingQAParticleCollection {
  public:
    StEmbeddingQAParticleCollection(); // default is piplus
    StEmbeddingQAParticleCollection(const Int_t particleId);
    StEmbeddingQAParticleCollection(const TString name);
    virtual ~StEmbeddingQAParticleCollection();

    StEmbeddingQAParticle* GetParent() const ;
    StEmbeddingQAParticle* GetDaughter(const UInt_t daughter) const ;

    UInt_t GetNDaughter() const ; // number of daughters

  private:
    StEmbeddingQAParticle* mParent ;
    std::vector<StEmbeddingQAParticle*> mDaughters ;

    Bool_t InitDaughters() ;

    ClassDef(StEmbeddingQAParticleCollection, 1)
};

#endif

