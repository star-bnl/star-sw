//
// $id$
//
// $Log: StBsmdePreCluster.h,v $
// Revision 1.1  2000/05/15 21:23:59  subhasis
// initial version
//
// PreClusters Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhyay,
//         Aleksei Pavlinov , July 1999
//

#ifndef STAR_StBsmdePreCluster
#define STAR_StBsmdePreCluster

#include "StEmcPreCluster.h"

class StBsmdePreCluster : public StEmcPreCluster {
public:
  StBsmdePreCluster(TArrayI*);

  virtual void calcMeanAndRms(StEmcHitCollection*);

  ClassDef(StBsmdePreCluster,1)// Class for Bsmde cluster
};

#endif
