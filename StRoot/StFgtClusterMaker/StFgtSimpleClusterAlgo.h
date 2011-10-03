///
// $Id
// $Log
//
//  \author Anselm Vossen (avossen@indiana.edu) 
//  \class StFgtSimpleClusterAlgo
//
// Simple Clustering algorithm taking the hits from the StFgtEvent and adding Clusters
//
//
//


#ifndef STAR_StFgtSimpleClusterAlgo_HH
#define STAR_StFgtSimpleClusterAlgo_HH

#include "StFgtIClusterAlgo.h"
#include "StRoot/StEvent/StFgtEvent/StFgtRawHit.h"

class StFgtSimpleClusterAlgo :public StFgtIClusterAlgo
{
  StFgtSimpleClusterAlgo();

  virtual Int_t doClustering(StFgtRawHitArray&, StFgtClusterArray&);
  virtual Int_t Init();

 protected:
  bool sortHits(StFgtRawHit first, StFgtRawHit second);

 private:
  Bool_t mIsInitialized;
  ClassDef(StFgtSimpleClusterAlgo,1);
};


#endif
