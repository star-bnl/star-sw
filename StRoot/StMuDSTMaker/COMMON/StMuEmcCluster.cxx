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
{
}
StMuEmcCluster::~StMuEmcCluster()
{
}
StMuEmcCluster::StMuEmcCluster(StMuEmcCluster* cl)
{    
  mEta=cl->getEta();
  mPhi=cl->getPhi();
  mSigmaEta=cl->getSigmaEta();
  mSigmaPhi=cl->getSigmaPhi();
  mEnergy=cl->getEnergy();
  setNHits(cl->getNHits());
  for(int i =0;i<getNHits();i++) setHitId(i,cl->getHitId(i));
}

