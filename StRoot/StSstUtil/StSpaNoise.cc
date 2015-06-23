// $Id: StSpaNoise.cc,v 1.1 2015/06/23 16:26:19 jeromel Exp $
//
// $Log: StSpaNoise.cc,v $
// Revision 1.1  2015/06/23 16:26:19  jeromel
// First version created from the SSD code and reshaped
//
// Revision 1.1  2015/04/19 17:30:31  bouchet
// initial commit ; SST codes
//
// Revision 1.1  2015/01/29 20:16:37  bouchet
// SSD utils for hit reconstruction
//
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSstUtil regroups now methods for the classes StSstStrip, StSstCluster and StSstPoint
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

