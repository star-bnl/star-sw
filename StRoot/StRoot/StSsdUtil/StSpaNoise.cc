// $Id: StSpaNoise.cc,v 1.1 2006/10/16 16:43:29 bouchet Exp $
//
// $Log: StSpaNoise.cc,v $
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.2  2005/05/13 08:39:32  lmartin
// CVS tags added
//

#include "StSpaNoise.hh"

StSpaNoise* StSpaNoise::giveCopy()
{
  StSpaNoise *ptrClone  = new StSpaNoise(mNStrip, mPedestal, mSigma);
  
  ptrClone->mNoiseValue = mNoiseValue;
  ptrClone->mIsActive   = mIsActive;
  return ptrClone;
}

