//
// $id$
//
// $Log: StBsmdePreClusterCollection.h,v $
// Revision 1.3  2000/08/24 22:11:34  suaide
// restored some files for background compatibility
//
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

class StBsmdePreClusterCollection : public TObject 
{
public:
  StBsmdePreClusterCollection();
  StBsmdePreClusterCollection(const Char_t*);
  virtual ~StBsmdePreClusterCollection();

  ClassDef(StBsmdePreClusterCollection,1)//Class for Bsmde cluster collection
};

#endif
