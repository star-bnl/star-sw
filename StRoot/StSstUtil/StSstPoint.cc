//$Id: StSstPoint.cc,v 1.2 2016/06/10 19:29:58 bouchet Exp $
//
//$Log: StSstPoint.cc,v $
//Revision 1.2  2016/06/10 19:29:58  bouchet
//coverity : UNINIT_CTOR
//
//Revision 1.1  2015/06/23 16:26:20  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#include <string.h>
#include "StSstUtil/StSstPoint.hh"
#include "TMath.h"
#include "TVector2.h"
#include "StMessMgr.h"

StSstPoint::StSstPoint(Int_t rNPoint, Int_t rNWafer, Int_t rNumPackage, Int_t rKindPackage)
{
  memset (first, 0, last - first);
  mNPoint         = rNPoint;
  mNCluster       = rNumPackage;
  mNMatched       = rKindPackage;
  mIdClusterP     = -1;
  mIdClusterN     = -1;
  mFlag = 0;
  mNId = 0;
  mNWafer = 0;
  mMcTrack = 0;
  mPrevPoint = 0;
  mNextPoint = 0;
}
StSstPoint::StSstPoint(Int_t rNId , Int_t rMcHit , Int_t rMcTrack , Float_t *rXg , Float_t rDe, Float_t *rAngle)
{
  memset (first, 0, last - first);
  mNId       = rNId;
  mMcHit[0]  = rMcHit;
  mMcTrack   = rMcTrack;
  for(Int_t k = 0; k < 3; k++)      mXg[k]     = rXg[k];
  for(Int_t i = 0; i < 2; i++)      mAngle[i]  = rAngle[i];
  mDe[0]      = rDe;
  mNPoint = 0;
  mNCluster = 0;
  mNMatched = 0;
  mIdClusterP = -1;
  mIdClusterN = -1;
  mFlag = 0;
  mNWafer = 0;
  mPrevPoint = 0;
  mNextPoint = 0;
}

StSstPoint::StSstPoint(const StSstPoint & originalPoint)
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
  mPrevPoint = originalPoint.mPrevPoint;
  mNextPoint = originalPoint.mNextPoint;
  mMcTrack = originalPoint.mMcTrack;
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

StSstPoint& StSstPoint::operator=(const StSstPoint originalPoint)
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


StSstPoint* StSstPoint::giveCopy()
{
  StSstPoint *ptrClone = new StSstPoint(mNPoint, mNWafer,  mNCluster, mNMatched);
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

void StSstPoint::setEnergyLossCorrected(Float_t adcP, Float_t adcN, Float_t gain)
{
  if (gain==0) gain = 1;
  setDe((adcP*gain + adcN)/2.,0);
  setDe((adcP*gain - adcN)/2.,1);
}
