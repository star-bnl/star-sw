//
// $id$
//
// $Log: StBsmdpPreCluster.cxx,v $
// Revision 1.3  2000/08/24 22:11:34  suaide
// restored some files for background compatibility
//
// Revision 1.1  2000/05/15 21:24:00  subhasis
// initial version
//
// PreClusters Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhya,
//         Aleksei Pavlinov , July 1999
//

//////////////////////////////////////////////////////////////////////////
//                                   
// StBsmdpPreCluster
//
// StBsmdpPreCluster differs from the base class StEmcPreCluster only one 
// function => calcMeanAndRms(StEmcHitCollection *emc_hits).
//
//////////////////////////////////////////////////////////////////////////

#include "StBsmdpPreCluster.h"
ClassImp(StBsmdpPreCluster)

//__________________________________________________________________________
StBsmdpPreCluster::StBsmdpPreCluster(TArrayI *hits) 
{
cout <<"StBsmdpPreCluster is now obsolete. Use StEmcPreCluster\n";
}
