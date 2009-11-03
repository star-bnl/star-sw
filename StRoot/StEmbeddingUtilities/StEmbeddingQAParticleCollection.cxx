
#include "TError.h"
#include "StEmbeddingQAUtilities.h"
#include "StEmbeddingQAParticle.h"
#include "StEmbeddingQAParticleCollection.h"

ClassImp(StEmbeddingQAParticleCollection)

//____________________________________________________________________________________________________
StEmbeddingQAParticleCollection::StEmbeddingQAParticleCollection()
{
  mParent = new StEmbeddingQAParticle(8); // pi+

  InitDaughters();
}

//____________________________________________________________________________________________________
StEmbeddingQAParticleCollection::StEmbeddingQAParticleCollection(const Int_t particleId)
{
  mParent = new StEmbeddingQAParticle(particleId);

  InitDaughters();
}

//____________________________________________________________________________________________________
StEmbeddingQAParticleCollection::StEmbeddingQAParticleCollection(const TString name)
{
  mParent = new StEmbeddingQAParticle(StEmbeddingQAUtilities::getParticleId(name));

  InitDaughters();
}

//____________________________________________________________________________________________________
StEmbeddingQAParticleCollection::~StEmbeddingQAParticleCollection()
{
  delete mParent ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQAParticleCollection::InitDaughters()
{
  mDaughters.clear();

  const Int_t ndaughters = StEmbeddingQAUtilities::getNDaughter(mParent->getParticleId());

  // Define daughters
  for(Int_t i=0; i<ndaughters; i++){
    StEmbeddingQAParticle* daughter 
      = new StEmbeddingQAParticle( StEmbeddingQAUtilities::getDaughterParticleId(mParent->getName(), i) );
    mDaughters.push_back( daughter );
  }

  return kTRUE ;
}

//____________________________________________________________________________________________________
StEmbeddingQAParticle* StEmbeddingQAParticleCollection::getParent() const
{
  return mParent ;
}

//____________________________________________________________________________________________________
StEmbeddingQAParticle* StEmbeddingQAParticleCollection::getDaughter(const UInt_t daughter) const
{
  if( daughter >= mDaughters.size() ){
    Error("getDaughter", "Unknown daughter index, id=%3d", daughter);
    return 0;
  }

  return mDaughters[daughter] ;
}

//____________________________________________________________________________________________________
UInt_t StEmbeddingQAParticleCollection::getNDaughter() const
{
  return mDaughters.size() ;
}


