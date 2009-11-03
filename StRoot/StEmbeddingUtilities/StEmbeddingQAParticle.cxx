
#include "StEmbeddingQAUtilities.h"
#include "StEmbeddingQAParticle.h"

ClassImp(StEmbeddingQAParticle)

//____________________________________________________________________________________________________
StEmbeddingQAParticle::StEmbeddingQAParticle(const Int_t particleId)
  : kParticleId(particleId), 
  kName(StEmbeddingQAUtilities::getParticleName(particleId, kFALSE)),
  kTitle(StEmbeddingQAUtilities::getParticleName(particleId, kTRUE)),
  kMass2(StEmbeddingQAUtilities::getMass2(particleId)), kCharge(StEmbeddingQAUtilities::getCharge(particleId))
{
}

//____________________________________________________________________________________________________
StEmbeddingQAParticle::~StEmbeddingQAParticle()
{
}


