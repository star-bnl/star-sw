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

class StFgtSimpleClusterAlgo :public StFgtIClusterAlgo
{
  StFgtSimpleClusterAlgo();

  virtual Int_t doClustering(const StFgtRawHitArray&, StFgtClusterArray&);
  virtual Int_t Init(StFgtEvent* mEvent);

 protected:
  StFgtEvent* mFgtEvent;
 private:
  Bool_t mIsInitialized;
  ClassDef(StFgtSimpleClusterAlgo,1);
};


#endif
