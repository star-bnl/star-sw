//
// $id$
//
// $Log: StBsmdePreClusterCollection.cxx,v $
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

#include "StBsmdePreClusterCollection.h"
ClassImp(StBsmdePreClusterCollection)

//__________________________________________________________________________
StBsmdePreClusterCollection::StBsmdePreClusterCollection() { /* Nobody */ }
//__________________________________________________________________________
StBsmdePreClusterCollection::StBsmdePreClusterCollection(const Char_t *Name) 
{
cout <<"StBsmdePreClusterCollection is now obsolete. Use StEmcPreClusterCollection\n";
 
}
//__________________________________________________________________________
StBsmdePreClusterCollection::~StBsmdePreClusterCollection() { /* Nobody */ }
