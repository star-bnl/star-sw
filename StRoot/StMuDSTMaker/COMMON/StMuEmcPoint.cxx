//###########################################################
// EMC Micro Event
// Author: Alexandre A. P. Suaide
// initial version 08/2001
//
// See README for details
//########################################################### 
#include "StMuEmcPoint.h"
#include "TString.h"
ClassImp(StMuEmcPoint)

StMuEmcPoint::StMuEmcPoint()
{
  for(Int_t i=0;i<4;i++) mEmc[i] = NULL;
}
StMuEmcPoint::StMuEmcPoint(StMuEmcPoint *point)
{
  mEta=point->getEta();
  mPhi=point->getPhi();
  mRadius=point->getRadius();
  mDeltaEta=point->getDeltaEta();
  mDeltaPhi=point->getDeltaPhi();
  mEnergy=point->getEnergy();
  mChiSquare=point->getChiSquare();
  for(Int_t d=0;d<4;d++)
  {
    Int_t EmcDet=d+1;
    setCluster(EmcDet,point->getCluster(EmcDet));
  }
}
StMuEmcPoint::~StMuEmcPoint()
{
}
