//###########################################################
// EMC Micro Event
// Author: Alexandre A. P. Suaide
// initial version 08/2001
//
// See README for details
//########################################################### 
#include "StEmcMicroCluster.h"

ClassImp(StEmcMicroCluster)

StEmcMicroCluster::StEmcMicroCluster()
{
  mHits = new TObjArray();
}
StEmcMicroCluster::~StEmcMicroCluster()
{
  delete mHits;
}
StEmcMicroCluster::StEmcMicroCluster(StEmcMicroCluster* cl)
{    
  mEta=cl->getEta();
  mPhi=cl->getPhi();
  mSigmaEta=cl->getSigmaEta();
  mSigmaPhi=cl->getSigmaPhi();
  mEnergy=cl->getEnergy();
  for(Int_t i=0;i<cl->getNHits();i++)
  {
    StEmcMicroHit *hit=new StEmcMicroHit(cl->getHit(i));
    addHit(hit);
  }
}

