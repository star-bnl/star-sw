///
// $Id: StFgtSimpleClusterAlgo.h,v 1.5 2011/10/10 20:35:08 avossen Exp $
// $Log: StFgtSimpleClusterAlgo.h,v $
// Revision 1.5  2011/10/10 20:35:08  avossen
// fixed strip-cluster association in MaxCluster algo, made other files cvs compliant
//
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
#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/St_base/Stypes.h"


class StFgtSimpleClusterAlgo :public StFgtIClusterAlgo
{

 public:
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
