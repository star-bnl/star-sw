/********************************************************************
 *
 * $Id: StPmdClusterCollection.cxx,v 1.1 2002/08/27 12:16:41 subhasis Exp $
 *
 * Author: Subhasis Chattopadhyay
 ********************************************************************
 *
 * Description: StPmdClusterCollection is base class for
 *              PMD cluster collection. 
 *
 * $Log: StPmdClusterCollection.cxx,v $
 * Revision 1.1  2002/08/27 12:16:41  subhasis
 * First version
 *
 ********************************************************************/
#include "StPmdClusterCollection.h"
#include <TBrowser.h>
#include <TTableSorter.h>
#include <TDataSetIter.h>
#include <math.h>
#include "StPmdUtil/StPmdGeom.h"
#include "StPmdHit.h"

ClassImp(StPmdClusterCollection)


//_____________________________________________________________________________
//StPmdClusterCollection::StPmdClusterCollection():TDataSet("Default")
StPmdClusterCollection::StPmdClusterCollection()
{
  mNclusters=0;
}
//_____________________________________________________________________________

StPmdClusterCollection::~StPmdClusterCollection()
{
  DeleteCluster();
}

void 
StPmdClusterCollection::DeleteCluster()
{
  Option_t* opt="ALL";
   mClusters.Delete(opt);
}

//_____________________________________________________________________________

void StPmdClusterCollection::addCluster(StPmdCluster* cluster)
{

      mClusters.Add(cluster);
      mNclusters  += 1;

    }



