//
// $id$
//
// $Log: StBemcPreCluster.h,v $
// Revision 1.1  2000/05/15 21:23:59  subhasis
// initial version
//
//
// Author: Subhasis Chattopadhya,
//         Aleksei Pavlinov , July 1999
//

#ifndef STAR_StBemcPreCluster
#define STAR_StBemcPreCluster

#include "StEmcPreCluster.h"

class StBemcPreCluster : public StEmcPreCluster {
public:
  StBemcPreCluster(TArrayI*);
  virtual ~StBemcPreCluster();

  ClassDef(StBemcPreCluster,1)// Class for Bemc cluster
};


ostream &operator<<(ostream&, StBemcPreCluster&);

#endif
