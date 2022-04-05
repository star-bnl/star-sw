//###########################################################
// EMC Micro Event
// Author: Alexandre A. P. Suaide
// initial version 08/2001
//
// See README for details
//########################################################### 
#include "StMuEmcCluster.h"

ClassImp(StMuEmcCluster)

StMuEmcCluster::StMuEmcCluster()
    : TObject()
{
  Clear();
}
StMuEmcCluster::~StMuEmcCluster()
{
}
void StMuEmcCluster::Clear(Option_t*)
{
  mEta=0;mPhi=0;mSigmaEta=0;mSigmaPhi=0;mEnergy=0;mNHits=0;
  mHits.Set(0);
}
StMuEmcCluster::StMuEmcCluster(const StMuEmcCluster* cl)
    : TObject(*cl)
{    
  mEta=cl->getEta();
  mPhi=cl->getPhi();
  mSigmaEta=cl->getSigmaEta();
  mSigmaPhi=cl->getSigmaPhi();
  mEnergy=cl->getEnergy();
  setNHits(cl->getNHits());
  for(int i =0;i<getNHits();i++) setHitId(i,cl->getHitId(i));
}

