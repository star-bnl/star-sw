//
// $id$
//
// $Log: StBemcPreCluster.h,v $
// Revision 1.3  2000/08/24 22:11:33  suaide
// restored some files for background compatibility
//
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

class StBemcPreCluster : public TObject 
{
public:
  StBemcPreCluster(TArrayI*);
  virtual ~StBemcPreCluster();

  ClassDef(StBemcPreCluster,1)// Class for Bemc cluster
};

#endif
