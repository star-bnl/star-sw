//
// $id$
//
// $Log: StBemcPreClusterCollection.cxx,v $
// Revision 1.1  2000/05/15 21:23:59  subhasis
// initial version
//
//
// Author: Subhasis Chattopadhyay,
//         Aleksei Pavlinov , July 1999
//

#include "StBemcPreClusterCollection.h"
ClassImp(StBemcPreClusterCollection)

//__________________________________________________________________________
StBemcPreClusterCollection::StBemcPreClusterCollection() : StEmcPreClusterCollection() { /* Nobody */ }
//__________________________________________________________________________
StBemcPreClusterCollection::StBemcPreClusterCollection(const Char_t *Name) : 
               StEmcPreClusterCollection(Name) { /* Nobody */ }
//__________________________________________________________________________
StBemcPreClusterCollection::~StBemcPreClusterCollection() { /* Nobody */ }
//_____________________________________________________________________________
void StBemcPreClusterCollection::addPreCluster(StEmcHitCollection* hits, TArrayI *hid)
{
  //  cout<<"StBemcPreClusterCollection::addPreCluster "<<endl;
  StBemcPreCluster *cluster = new StBemcPreCluster(hid);
  cluster->calcMeanAndRms(hits);
  mClusters.Add(cluster);
  mNclusters  += 1;
}
