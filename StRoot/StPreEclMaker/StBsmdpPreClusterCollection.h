//
// $id$
//
// $Log: StBsmdpPreClusterCollection.h,v $
// Revision 1.1  2000/05/15 21:24:00  subhasis
// initial version
//
// PreClusters Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhyay,
//         Aleksei Pavlinov , July 1999
//

#ifndef STAR_StBsmdpPreClusterCollection
#define STAR_StBsmdpPreClusterCollection

#include "StEmcPreClusterCollection.h"
#include "StBsmdpPreCluster.h"

class StBsmdpPreClusterCollection : public StEmcPreClusterCollection {
public:
  StBsmdpPreClusterCollection();
  StBsmdpPreClusterCollection(const Char_t*);
  virtual ~StBsmdpPreClusterCollection();

  virtual  void addPreCluster(StEmcHitCollection*, TArrayI*);
  virtual Int_t testOnNeighbor(Int_t);

  ClassDef(StBsmdpPreClusterCollection,1)//Class for Bsmdp cluster collection
};

#endif
