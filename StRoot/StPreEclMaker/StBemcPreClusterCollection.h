//
// $id$
//
// $Log: StBemcPreClusterCollection.h,v $
// Revision 1.1  2000/05/15 21:23:59  subhasis
// initial version
//
//
// Author: Subhasis Chattopadhyay,
//          Aleksei Pavlinov , July 1999
//

#ifndef STAR_StBemcPreClusterCollection
#define STAR_StBemcPreClusterCollection

#include "StEmcPreClusterCollection.h"
#include "StBemcPreCluster.h"

class StBemcPreClusterCollection : public StEmcPreClusterCollection {
public:
  StBemcPreClusterCollection();
  StBemcPreClusterCollection(const Char_t*);
  virtual ~StBemcPreClusterCollection();

  virtual  void addPreCluster(StEmcHitCollection*, TArrayI*);

  ClassDef(StBemcPreClusterCollection,1)//Class for Bemc cluster collection
};

#endif
