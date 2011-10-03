//
//  $Id
//  $Log
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
  StFgtMaxClusterAlgo();

  virtual Int_t doClustering(StFgtRawHitArray&, StFgtClusterArray&);
  virtual Int_t Init();

 protected:

 private:
  Bool_t mIsInitialized;
  ClassDef(StFgtMaxClusterAlgo,1);
};


#endif
