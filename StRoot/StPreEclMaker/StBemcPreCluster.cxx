//
// $id$
//
// $Log: StBemcPreCluster.cxx,v $
// Revision 1.3  2000/08/24 22:11:33  suaide
// restored some files for background compatibility
//
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
StBemcPreCluster::StBemcPreCluster(TArrayI *hits)
{ 
  cout <<"StBemcPreCluster is now obsolete. Use StEmcPreCluster\n";
}
//__________________________________________________________________________
StBemcPreCluster::~StBemcPreCluster() { /* Nobody */ }


