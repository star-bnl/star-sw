// $Id: StSsdPoint.cc,v 1.1 2006/10/16 16:43:29 bouchet Exp $
//
// $Log: StSsdPoint.cc,v $
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.3  2005/03/18 16:29:08  lmartin
//  new members mIdClusterP and mIdClusterN and associated methods
//
// Revision 1.2  2005/03/18 14:19:44  lmartin
// missing CVS header added
//

#include <string.h>
#include "StSsdUtil/StSsdPoint.hh"

StSsdPoint::StSsdPoint(Int_t rNPoint, Int_t rNWafer, Int_t rNumPackage, Int_t rKindPackage)
{
  memset (first, 0, last - first);
  mNPoint         = rNPoint;
  mNCluster       = rNumPackage;
  mNMatched       = rKindPackage;
  mIdClusterP     = -1;
  mIdClusterN     = -1;
}
StSsdPoint::StSsdPoint(Int_t rNId , Int_t rMcHit , Int_t rMcTrack , Float_t *rXg , Float_t rDe, Float_t *rAngle)
{
  memset (first, 0, last - first);
  mNId       = rNId;
  mMcHit[0]  = rMcHit;
  mMcTrack   = rMcTrack;
  for(Int_t k = 0; k < 3; k++)      mXg[k]     = rXg[k];
  for(Int_t i = 0; i < 2; i++)      mAngle[i]  = rAngle[i];
  mDe[0]      = rDe;
}

StSsdPoint::StSsdPoint(const StSsdPoint & originalPoint)
{
  memset (first, 0, last - first);
  mNId            = originalPoint.mNId;//jb 10/11 fill(NId) for the simulation
  mFlag           = originalPoint.mFlag;
  mNPoint         = originalPoint.mNPoint;
  mNCluster       = originalPoint.mNCluster;
  mNMatched       = originalPoint.mNMatched;
  mIdClusterP     = originalPoint.mIdClusterP;
  mIdClusterN     = originalPoint.mIdClusterN;
  mNWafer         = originalPoint.mNWafer;

  Int_t i = 0;
  for (i = 0; i < 5; i++)
      mMcHit[i]    = originalPoint.mMcHit[i];

  for (i = 0; i < 3; i++)
    {
      mXg[i]        = originalPoint.mXg[i];
      mXl[i]        = originalPoint.mXl[i];
    }

  for (i = 0; i < 2; i++)
    {
      mPositionU[i] = originalPoint.mPositionU[i];
      mDe[i]        = originalPoint.mDe[i];
    }
}

StSsdPoint& StSsdPoint::operator=(const StSsdPoint originalPoint)
{
  memset (first, 0, last - first);
  mNId            = originalPoint.mNId;//jb 10/11 fill(NId) for the simulation
  mFlag           = originalPoint.mFlag;
  mNPoint         = originalPoint.mNPoint;
  mNCluster       = originalPoint.mNCluster;
  mNMatched       = originalPoint.mNMatched;
  mIdClusterP     = originalPoint.mIdClusterP;
  mIdClusterN     = originalPoint.mIdClusterN;
  mNWafer         = originalPoint.mNWafer;

  Int_t i = 0;
  for (i = 0; i < 5; i++)
      mMcHit[i]    = originalPoint.mMcHit[i];

  for (i = 0; i < 3; i++)
    {
      mXg[i]        = originalPoint.mXg[i];
      mXl[i]        = originalPoint.mXl[i];
    }

  for (i = 0; i < 2; i++)
    {
      mPositionU[i] = originalPoint.mPositionU[i];
      mDe[i]        = originalPoint.mDe[i];
    }
  return *this;
} 


StSsdPoint* StSsdPoint::giveCopy()
{
  StSsdPoint *ptrClone = new StSsdPoint(mNPoint, mNWafer,  mNCluster, mNMatched);
  Int_t i = 0;
  ptrClone->mNId  = mNId;
  ptrClone->mFlag = mFlag;
  ptrClone->mIdClusterP = mIdClusterP;
  ptrClone->mIdClusterN = mIdClusterN;
  for (i = 0; i < 5; i++)
      ptrClone->mMcHit[i]   = mMcHit[i];

  for (i = 0; i < 3; i++)
    { ptrClone->mXg[i]         = mXg[i];
      ptrClone->mXl[i]         = mXl[i];
    }
  for (i = 0; i < 2; i++)
    { ptrClone->mPositionU[i]  = mPositionU[i];
      ptrClone->mDe[i]         = mDe[i];
    }
  return ptrClone;
}

