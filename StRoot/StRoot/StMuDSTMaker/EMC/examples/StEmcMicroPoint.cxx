//###########################################################
// EMC Micro Event
// Author: Alexandre A. P. Suaide
// initial version 08/2001
//
// See README for details
//########################################################### 
#include "StEmcMicroPoint.h"
#include "TString.h"
ClassImp(StEmcMicroPoint)

StEmcMicroPoint::StEmcMicroPoint()
{
  for(Int_t i=0;i<4;i++) mEmc[i] = new TObjArray();
}
StEmcMicroPoint::StEmcMicroPoint(StEmcMicroPoint *point)
{
  mEta=point->getEta();
  mPhi=point->getPhi();
  mDeltaEta=point->getDeltaEta();
  mDeltaPhi=point->getDeltaPhi();
  mEnergy=point->getEnergy();
  mChiSquare=point->getChiSquare();
  for(Int_t d=0;d<4;d++)
  {
    Int_t EmcDet=d+1;
    Int_t nc=point->getNClusters(EmcDet);
    for(Int_t cl=0;cl<nc;cl++) 
    {
      StEmcMicroCluster *cluster=new StEmcMicroCluster(point->getCluster(EmcDet,cl));
      addCluster(EmcDet,cluster);
    }
  }
}
StEmcMicroPoint::~StEmcMicroPoint()
{
  for(Int_t i=0;i<4;i++) delete mEmc[i];
}
