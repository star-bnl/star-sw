//
// $id$
//
// $Log: StBsmdpPreCluster.h,v $
// Revision 1.1  2000/05/15 21:24:00  subhasis
// initial version
//
// PreClusters Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhyay,
//         Aleksei Pavlinov , July 1999
//

#ifndef STAR_StBsmdpPreCluster
#define STAR_StBsmdpPreCluster

#include "StEmcPreCluster.h"

class StBsmdpPreCluster : public StEmcPreCluster {
public:
  StBsmdpPreCluster(TArrayI*);

  virtual void calcMeanAndRms(StEmcHitCollection*);

  ClassDef(StBsmdpPreCluster,1)// Class for Bsmdp cluster
};

#endif
