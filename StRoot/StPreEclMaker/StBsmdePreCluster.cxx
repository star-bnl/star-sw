//
// $id$
//
// $Log: StBsmdePreCluster.cxx,v $
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

//////////////////////////////////////////////////////////////////////////
//                                   
// StBsmdePreCluster
//
// StBsmdePreCluster differs from the base class StEmcPreCluster only one 
// function => calcMeanAndRms(StEmcHitCollection *emc_hits).
//
//////////////////////////////////////////////////////////////////////////

#include "StBsmdePreCluster.h"
ClassImp(StBsmdePreCluster)

//__________________________________________________________________________
StBsmdePreCluster::StBsmdePreCluster(TArrayI *hits) 
{ 
cout <<"StBsmdePreCluster is now obsolete. Use StEmcPreCluster\n";
}
