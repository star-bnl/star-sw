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
    : TObject()
{
mEta=mPhi=mRadius=mDeltaEta=mDeltaPhi=mEnergy=mChiSquare=0;    
memset(mEmc,0,sizeof(mEmc));
}
StMuEmcPoint::StMuEmcPoint(StMuEmcPoint *point)
    : TObject(*point)
{
  mEta=point->getEta();
  mPhi=point->getPhi();
  mRadius=point->getRadius();
  mDeltaEta=point->getDeltaEta();
  mDeltaPhi=point->getDeltaPhi();
  mEnergy=point->getEnergy();
  mChiSquare=point->getChiSquare();
  for(Int_t d=0;d<8;d++)
  {
    Int_t EmcDet=d+1;
    setCluster(point->getCluster(EmcDet),EmcDet);
  }
}
StMuEmcPoint::~StMuEmcPoint()
{
  for (int i=0;i<8;i++) {
//VP    delete mEmc[i];   //VP Unclear who is the owner   
    mEmc[i] = 0;
  }  
}
