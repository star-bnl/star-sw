//
// $id$
//
// $Log: StBsmdePreClusterCollection.cxx,v $
// Revision 1.1  2000/05/15 21:23:59  subhasis
// initial version
//
// PreClusters Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhyay,
//         Aleksei Pavlinov , July 1999
//

#include "StBsmdePreClusterCollection.h"
ClassImp(StBsmdePreClusterCollection)

//__________________________________________________________________________
StBsmdePreClusterCollection::StBsmdePreClusterCollection() : StEmcPreClusterCollection() { /* Nobody */ }
//__________________________________________________________________________
StBsmdePreClusterCollection::StBsmdePreClusterCollection(const Char_t *Name) : 
               StEmcPreClusterCollection(Name) 
{
    mDetector  = 3;     mEnergySeed         = 0.05; 
    mEnergyAdd = 0.001; mEnergyThresholdAll = 0.1;  
    mSizeMax = 5;
}
//__________________________________________________________________________
StBsmdePreClusterCollection::~StBsmdePreClusterCollection() { /* Nobody */ }
//__________________________________________________________________________
Int_t StBsmdePreClusterCollection::testOnNeighbor(Int_t jn)
{
  static Int_t etaFirst, etaLast;
  extern Int_t nhit;
  extern TArrayI ew,hitsid;

  if(nhit == 1) {etaFirst=ew[hitsid[0]]; etaLast=etaFirst;}

  if     (etaFirst-ew[jn] == 1) {etaFirst=ew[jn]; return 0;}
  else if(ew[jn]-etaLast  == 1) {etaLast=ew[jn];  return 0;}
  else {return 1;}
}
//_____________________________________________________________________________
void StBsmdePreClusterCollection::addPreCluster(StEmcHitCollection* hits, TArrayI *hid)
{
  StBsmdePreCluster *cluster = new StBsmdePreCluster(hid);
  cluster->calcMeanAndRms(hits);
  mClusters.Add(cluster);
  mNclusters  += 1;
}
