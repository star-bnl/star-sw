// $Id: StSsdPoint.cc,v 1.3 2007/07/31 14:51:51 bouchet Exp $
//
// $Log: StSsdPoint.cc,v $
// Revision 1.3  2007/07/31 14:51:51  bouchet
// Gain calibration using the mapping method instead of the rotating vector
//
// Revision 1.2  2007/03/27 23:11:48  bouchet
// Add a method to use the gain calibration for the Charge Matching between pulse of p and n sides
//
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
#include "TMath.h"
#include "TVector2.h"
#include "StMessMgr.h"

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

void StSsdPoint::setEnergyLossCorrected(Float_t adcP, Float_t adcN, Float_t gain)
{
  // the gain is the ratio adcN/adcP
  // in the frame {pulseP,pulseN}, we have the relation adcN = ATan(gain)*adcP (+offset)   
  // the correction consists in a rotation of the hit of coordinates {pulseP,pulseN} by an angle = ratio
  // opposite sign is the gain > 1   
  //Double_t angle  = 0;
  if (gain==0) gain = 1;
  //printf("for this wafer, the gain is %f\n",gain);
  // gain=0 means we don't have any value forthis wafer then we don't do any rotation
  // newAdcp=adcP and newAdcN=adcN since the sin gives 0
  /*
    angle           = TMath::ATan(gain);
    angle           = TMath::Abs(45 - (angle*360)/(TMath::Pi()*2));
    //angle = TMath::Abs(TMath::Pi()/4 - angle) ;//radians
    angle = TMath::Pi()/4 - angle;
    TVector2 pulse;
    TVector2 newpulse;
    pulse.Set(adcP,adcN);
    newpulse = pulse.Rotate(angle); 
    printf("old adcP =%f old adcN=%f\n",pulse.X(),pulse.Y());
    printf("new adcP =%f new adcN=%f\n",newpulse.X(),newpulse.Y());
    setDe((newpulse.X() + newpulse.Y())/2.,0);
    setDe((newpulse.X() - newpulse.Y())/2.,1);
  */
  setDe((adcP*gain + adcN)/2.,0);
  setDe((adcP*gain - adcN)/2.,1);


}
