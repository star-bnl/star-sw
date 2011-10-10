//
//  $Id: StFgtMaxClusterAlgo.h,v 1.3 2011/10/10 20:35:08 avossen Exp $
//  $Log: StFgtMaxClusterAlgo.h,v $
//  Revision 1.3  2011/10/10 20:35:08  avossen
//  fixed strip-cluster association in MaxCluster algo, made other files cvs compliant
//
//
// \class StFgtMaxClusterAlgo
// \author Anselm Vossen (avossen@indiana.edu)
//
// This class finds clusters defined as the max hit in each layer
//

#ifndef STAR_StFgtMaxClusterAlgo_HH
#define STAR_StFgtMaxClusterAlgo_HH

#include "StFgtIClusterAlgo.h"
#include "StRoot/StEvent/StFgtEvent/StFgtRawHit.h"

class StFgtMaxClusterAlgo :public StFgtIClusterAlgo
{


 public:
  StFgtMaxClusterAlgo();

  virtual Int_t doClustering(StFgtRawHitArray&, StFgtClusterArray&);
  virtual Int_t Init();

 protected:

 private:
  Bool_t mIsInitialized;
  ClassDef(StFgtMaxClusterAlgo,1);
};


#endif
