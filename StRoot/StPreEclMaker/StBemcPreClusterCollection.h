//
// $id$
//
// $Log: StBemcPreClusterCollection.h,v $
// Revision 1.3  2000/08/24 22:11:33  suaide
// restored some files for background compatibility
//
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

class StBemcPreClusterCollection : public TObject 
{
public:
  StBemcPreClusterCollection();
  StBemcPreClusterCollection(const Char_t*);
  virtual ~StBemcPreClusterCollection();

  ClassDef(StBemcPreClusterCollection,1)//Class for Bemc cluster collection
};

#endif
