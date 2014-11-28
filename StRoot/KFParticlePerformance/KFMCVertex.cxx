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

#include "KFMCVertex.h"

KFMCVertex::KFMCVertex():fDaughterTracks(0),fIsReconstructable(0),fIsMCReconstructable(0),fIsReconstructed(0),fNReconstructedDaughters(0),fIsTriggerPV(0)
{
  for( int i = 0; i < 3; i++) fPar[i] = 0;
}


ostream& operator<<(ostream& out, const KFMCVertex &a)
{
  for (int i = 0; i < 3; i++) out << a.fPar[i] << std::endl;
  return out;
}


istream& operator>>(istream& in, KFMCVertex &a)
{
  for (int i = 0; i < 3; i++) in >> a.fPar[i];
  return in;
}

