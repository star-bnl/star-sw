#include "StarGenParticle.h"
ClassImp(StarGenParticle);
#include "g2t/St_g2t_particle_Module.h"
#include <iostream>
#include "StarGenerator/UTIL/StarParticleData.h"
using namespace std;
// --------------------------------------------------------------------------
StarGenParticle::StarGenParticle() : 
  mStatus(0),
  mId(0),
  mPx(0), 
  mPy(0), 
  mPz(0), 
  mEnergy(0),  
  mMass(0), 
  mVx(0), 
  mVy(0), 
  mVz(0), 
  mTof(0),
  mIndex(0),
  mStack(0), 
  mPrimaryKey(0),
  mGeneratorId(-1)
{
  for ( Int_t i=0;i<2;i++ ) { mMother[i]=0.0; mDaughter[i]=0.0; }
}

void StarGenParticle::SetId( Int_t id )
{ 
  mId = id; 
  TParticlePDG *pdg = StarParticleData::instance().GetParticle(mId);
  if (pdg) 
    {
      SetMass( pdg->Mass() );
    }
}

// --------------------------------------------------------------------------
void StarGenParticle::Print( const Char_t *opts ) const
{

  TParticlePDG *pdg = StarParticleData::instance().GetParticle(mId);
  TString name = "          ";
  if ( pdg ) name = pdg->GetName();

  cout << Form("[%4i|%4i|%4i] id=%10i %10s stat=%02i p=(%8.3f,%8.3f,%8.3f,%8.3f; %8.3f) v=(%8.4f,%8.4f,%8.3f) [%i %i] [%i %i]",
	       mPrimaryKey, /* Index in primary record */
	       mIndex,      /* Index in event generator record */
	       mStack,      /* Index in particle stack for starsim */
	       mId,name.Data(),mStatus,mPx,mPy,mPz,mEnergy,mMass,mVx,mVy,mVz,
	       mMother[0], mMother[1],
	       mDaughter[0], mDaughter[1]
	       ) << endl;
	       
}
// --------------------------------------------------------------------------

Bool_t StarGenParticle::Simulate()
{

  // Does the particle decay to daughters?  If so, do not stack.
  if ( mDaughter[0]+mDaughter[1] > 0 ) 
    {
      return false;
    }

  // Is the particle a neutrino?  If so, do not stack.
  Int_t id = TMath::Abs(mId);
  if ( id==12 || id==14 || id==16 || id==18 /* tau prime? */ )
    {
      return false;
    }

  // Is the particle flagged as final state?  If so, stack it.
  if ( mStatus == kFinal )
    {
      return true;
    }
  
  // All other cases, do not stack
  return false;

}
