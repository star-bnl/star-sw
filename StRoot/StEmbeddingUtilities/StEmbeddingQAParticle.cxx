
#include "StEmbeddingQAUtilities.h"
#include "StEmbeddingQAParticle.h"

ClassImp(StEmbeddingQAParticle)

//____________________________________________________________________________________________________
StEmbeddingQAParticle::StEmbeddingQAParticle(const Int_t particleId)
  : kParticleId(particleId), 
  kName(StEmbeddingQAUtilities::GetParticleName(particleId, kFALSE)),
  kTitle(StEmbeddingQAUtilities::GetParticleName(particleId, kTRUE)),
  kMass2(StEmbeddingQAUtilities::GetMass2(particleId)), kCharge(StEmbeddingQAUtilities::GetCharge(particleId))
{
}

//____________________________________________________________________________________________________
StEmbeddingQAParticle::~StEmbeddingQAParticle()
{
}


