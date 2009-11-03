//----------------------------------------------------------------------------------------------------
//  Class StEmbeddingQAParticleCollection
//
//    - Collection of StEmbeddingQAParticle
//    - Initialized by either particle geant id or particle name
//----------------------------------------------------------------------------------------------------

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

    StEmbeddingQAParticle* getParent() const ;
    StEmbeddingQAParticle* getDaughter(const UInt_t daughter) const ;

    UInt_t getNDaughter() const ; // number of daughters

  private:
    StEmbeddingQAParticle* mParent ;
    std::vector<StEmbeddingQAParticle*> mDaughters ;

    Bool_t InitDaughters() ;

    ClassDef(StEmbeddingQAParticleCollection, 1)
};

#endif

