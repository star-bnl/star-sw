//
// $id$
//
// $Log: StBsmdpPreClusterCollection.cxx,v $
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

#include "StBsmdpPreClusterCollection.h"
ClassImp(StBsmdpPreClusterCollection)

//__________________________________________________________________________
StBsmdpPreClusterCollection::StBsmdpPreClusterCollection() { /* Nobody */ }
//__________________________________________________________________________
StBsmdpPreClusterCollection::StBsmdpPreClusterCollection(const Char_t *Name) 
{
cout <<"StBsmdpPreClusterCollection is now obsolete. Use StEmcPreClusterCollection\n";

}
//__________________________________________________________________________
StBsmdpPreClusterCollection::~StBsmdpPreClusterCollection() { /* Nobody */ }
