//
// $id$
//
// $Log: StBsmdpPreClusterCollection.cxx,v $
// Revision 1.1  2000/05/15 21:24:00  subhasis
// initial version
//
// PreClusters Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhyay,
//         Aleksei Pavlinov , July 1999
//

#include "StBsmdpPreClusterCollection.h"
ClassImp(StBsmdpPreClusterCollection)

//__________________________________________________________________________
StBsmdpPreClusterCollection::StBsmdpPreClusterCollection() : StEmcPreClusterCollection() { /* Nobody */ }
//__________________________________________________________________________
StBsmdpPreClusterCollection::StBsmdpPreClusterCollection(const Char_t *Name) : 
               StEmcPreClusterCollection(Name) 
{
    mDetector  = 4;     mEnergySeed         = 0.05; 
    mEnergyAdd = 0.001; mEnergyThresholdAll = 0.1;  
    mSizeMax = 5;
}
//__________________________________________________________________________
StBsmdpPreClusterCollection::~StBsmdpPreClusterCollection() { /* Nobody */ }
//__________________________________________________________________________
Int_t StBsmdpPreClusterCollection::testOnNeighbor(Int_t jn)
{
  static Int_t phiFirst, phiLast, etaSeed;
  extern Int_t nhit;
  extern TArrayI ew, sw, hitsid;

  if(nhit == 1) {etaSeed=ew[hitsid[0]]; phiFirst=sw[hitsid[0]]; phiLast=phiFirst;}
  
  if(etaSeed == ew[jn]){ // Same eta bin
    if     (phiFirst-sw[jn] == 1) {phiFirst=sw[jn]; return 0;}
    else if(sw[jn]-phiLast  == 1) {phiLast=sw[jn];  return 0;}
    else {return 1;}
  }
}
//_____________________________________________________________________________
void StBsmdpPreClusterCollection::addPreCluster(StEmcHitCollection* hits, TArrayI *hid)
{
  StBsmdpPreCluster *cluster = new StBsmdpPreCluster(hid);
  cluster->calcMeanAndRms(hits);
  mClusters.Add(cluster);
  mNclusters  += 1;
}
