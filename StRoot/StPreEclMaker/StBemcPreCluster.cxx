//
// $id$
//
// $Log: StBemcPreCluster.cxx,v $
// Revision 1.1  2000/05/15 21:23:59  subhasis
// initial version
//
//
// Author: Subhasis Chattopadhyay,
//         Aleksei Pavlinov , July 1999
//

#include "StBemcPreCluster.h"
ClassImp(StBemcPreCluster)

//__________________________________________________________________________
StBemcPreCluster::StBemcPreCluster(TArrayI *hits) : 
               StEmcPreCluster(hits) { /* Nobody */ }
//__________________________________________________________________________
StBemcPreCluster::~StBemcPreCluster() { /* Nobody */ }
//_____________________________________________________________________________
ostream &operator<<(ostream &os, StBemcPreCluster &cl)
{
  cl.print(&os); return os;
}


