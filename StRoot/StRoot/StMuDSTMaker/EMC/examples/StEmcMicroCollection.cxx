//###########################################################
// EMC Micro Event
// Author: Alexandre A. P. Suaide
// initial version 08/2001
//
// See README for details
//########################################################### 
#include "StEmcMicroCollection.h"

ClassImp(StEmcMicroCollection)

StEmcMicroCollection::StEmcMicroCollection()
{    
  mEmcPoints=new TObjArray();
  for(Int_t i=0;i<4;i++) 
  { 
    mEmcHits[i]=new TObjArray();
    mEmcClusters[i]=new TObjArray();
  }
}
StEmcMicroCollection::~StEmcMicroCollection()
{
  clear();
  for(Int_t i=0;i<4;i++) 
  {
    delete mEmcHits[i]; mEmcHits[i]=NULL;
    delete mEmcClusters[i]; mEmcClusters[i]=NULL;
  } 
  delete mEmcPoints;
}
void StEmcMicroCollection::clear(Option_t *option)
{
  // deleting points ...
  for(Int_t i=0;i<getNPoints();i++)
  {
    StEmcMicroPoint *point = getPoint(i);
    if(point) delete point;
  }
  // deleting clusters and hits
  for(Int_t d=0;d<4;d++)
  {
    Int_t EmcDet=d+1;
    for(Int_t i=0; i<getNClusters(EmcDet);i++)
    {
      StEmcMicroCluster* cluster = getCluster(EmcDet,i);
      if(cluster) delete cluster;
    }
    for(Int_t i=0; i<getNHits(EmcDet);i++)
    {
      StEmcMicroHit* hit = getHit(EmcDet,i);
      if(hit) delete hit;
    }
  }
  
  for(Int_t i=0;i<4;i++) { mEmcHits[i]->Clear(); mEmcClusters[i]->Clear();}
  mEmcPoints->Clear();
  return;
}
