//
// $id$
//
// $Log: StBsmdePreClusterCollection.h,v $
// Revision 1.1  2000/05/15 21:24:00  subhasis
// initial version
//
// PreClusters Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhyay,
//         Aleksei Pavlinov , July 1999
//

#ifndef STAR_StBsmdePreClusterCollection
#define STAR_StBsmdePreClusterCollection

#include "StEmcPreClusterCollection.h"
#include "StBsmdePreCluster.h"

class StBsmdePreClusterCollection : public StEmcPreClusterCollection {
public:
  StBsmdePreClusterCollection();
  StBsmdePreClusterCollection(const Char_t*);
  virtual ~StBsmdePreClusterCollection();

  virtual  void addPreCluster(StEmcHitCollection*, TArrayI*);
  virtual Int_t testOnNeighbor(Int_t);

  ClassDef(StBsmdePreClusterCollection,1)//Class for Bsmde cluster collection
};

#endif
