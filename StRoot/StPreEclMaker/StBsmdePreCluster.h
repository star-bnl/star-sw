//
// $id$
//
// $Log: StBsmdePreCluster.h,v $
// Revision 1.3  2000/08/24 22:11:34  suaide
// restored some files for background compatibility
//
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

class StBsmdePreCluster : public TObject 
{
public:
  StBsmdePreCluster(TArrayI*);

  ClassDef(StBsmdePreCluster,1)// Class for Bsmde cluster
};

#endif
