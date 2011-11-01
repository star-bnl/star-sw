///
// $Id: StFgtSimpleClusterAlgo.h,v 1.6 2011/11/01 18:46:30 sgliske Exp $
// $Log: StFgtSimpleClusterAlgo.h,v $
// Revision 1.6  2011/11/01 18:46:30  sgliske
// Updated to correspond with StEvent containers, take 2.
//
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

class StFgtSimpleClusterAlgo :public StFgtIClusterAlgo
{

 public:
  StFgtSimpleClusterAlgo();

  virtual Int_t doClustering(  StFgtStripCollection& strips, StFgtHitCollection& clusters );
  virtual Int_t Init();

 protected:

 private:
  Bool_t mIsInitialized;
  ClassDef(StFgtSimpleClusterAlgo,1);
};


#endif
