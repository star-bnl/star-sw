//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________

#include "KFMCParticle.h"

#ifndef KFParticleStandalone
ClassImp(KFMCParticle)
#endif

KFMCParticle::KFMCParticle() :fDaughterIds(), fMCTrackID(-1), fMotherId(-1), fPDG(0), fInitialParticleId(-1)
{
  for(int i=0; i<3; i++)
  {
    fIsReconstructable[i] = 0;
    fIsV0[i] = 0;
  }
  fIsReconstructable[3] = 0;
  fIsReconstructable[4] = 0;
}

KFMCParticle::~KFMCParticle()
{
}

void KFMCParticle::AddDaughter( int i )
{
  fDaughterIds.push_back(i);
}
