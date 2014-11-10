#include "StarDecayManager.h"
#include "StMessMgr.h"
#include <assert.h>
#include "TClonesArray.h"
 
#include "StarGenerator/UTIL/StarParticleData.h"

static StarParticleData &pdb = StarParticleData::instance();

StarDecayManager::StarDecayManager( const Char_t *name )
  : mDecayer(), mCurrentDecayer(0), mDebug(0)
{


}

StarDecayManager::~StarDecayManager()
{

}

void StarDecayManager::Init()
{

}


void StarDecayManager::Decay( Int_t pdgid, TLorentzVector *p )
{
  ClearEvent();

  mCurrentDecayer = mDecayer[pdgid];
  
  if (!mCurrentDecayer) mCurrentDecayer = mDecayer[0];
  assert(mCurrentDecayer);

  mCurrentDecayer -> Decay( pdgid, p );

}

Int_t StarDecayManager::ImportParticles( TClonesArray *array )
{
  Int_t np = 0;
  if ( mCurrentDecayer ) np = mCurrentDecayer->ImportParticles( array );    
  return np;
}

void StarDecayManager::SetForceDecay( Int_t type ){ assert(0); }
void StarDecayManager::ForceDecay(){ 

  LOG_INFO << "Force Decay" << endm;

}
Float_t StarDecayManager::GetPartialBranchingRatio( Int_t ipdg ){ assert(0); }
void StarDecayManager::ReadDecayTable(){ assert(0); }

Float_t StarDecayManager::GetLifetime( Int_t pdgid )
{
  // Lifetime evaluated by the decayer which handles the decay
  return mCurrentDecayer->GetLifetime(pdgid);
}

void StarDecayManager::ClearEvent()
{

}
