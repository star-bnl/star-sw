
#include <iostream>

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
  mParent = new StEmbeddingQAParticle(StEmbeddingQAUtilities::GetParticleId(name));

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

  const Int_t ndaughters = StEmbeddingQAUtilities::GetNDaughter(mParent->GetParticleId());

  // Define daughters
  for(Int_t i=0; i<ndaughters; i++){
    StEmbeddingQAParticle* daughter 
      = new StEmbeddingQAParticle( StEmbeddingQAUtilities::GetDaughterParticleId(mParent->GetName(), i) );
    mDaughters.push_back( daughter );
  }

  return kTRUE ;
}

//____________________________________________________________________________________________________
StEmbeddingQAParticle* StEmbeddingQAParticleCollection::GetParent() const
{
  return mParent ;
}

//____________________________________________________________________________________________________
StEmbeddingQAParticle* StEmbeddingQAParticleCollection::GetDaughter(const UInt_t daughter) const
{
  if( daughter >= mDaughters.size() ){
    Error("GetDaughter", "Unknown daughter index, id=%3d", daughter);
    return 0;
  }

  return mDaughters[daughter] ;
}

//____________________________________________________________________________________________________
UInt_t StEmbeddingQAParticleCollection::GetNDaughter() const
{
  return mDaughters.size() ;
}


