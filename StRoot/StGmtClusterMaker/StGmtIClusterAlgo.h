//
// \authors K.S. Engle and Richard Witt (witt@usna.edu)
// based on StFgtClusterAlgo
// abstract base class for cluster algorithm implementation
//
//
#ifndef STAR_StGmtIClusterAlgo_HH
#define STAR_StGmtIClusterAlgo_HH

#include "Stypes.h"
class StGmtStripCollection;
class StGmtHitCollection;

class StGmtIClusterAlgo
{
 public:
  /**subclasses must implement this function that takes raw hits from StEvent and fills the Cluster collection
  //the input might be modified, since the clustering checks if the seeds are legitimate and adds the info to the strips if they 
  //are at the beginning or end of a cluster*/
  virtual Int_t doClustering( UInt_t, StGmtStripCollection&, StGmtHitCollection& )=0;
  virtual Int_t Init()=0;
  virtual ~StGmtIClusterAlgo()=0;
  // virtual  Bool_t IsPedOk()=0;
 private:
  ClassDef( StGmtIClusterAlgo, 1 );  
};

#endif
