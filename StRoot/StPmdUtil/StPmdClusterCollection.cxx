/********************************************************************
 *
 * $Id: StPmdClusterCollection.cxx,v 1.2 2003/05/12 12:07:12 subhasis Exp $
 *
 * Author: Subhasis Chattopadhyay
 ********************************************************************
 *
 * Description: StPmdClusterCollection is base class for
 *              PMD cluster collection. 
 *
 * $Log: StPmdClusterCollection.cxx,v $
 * Revision 1.2  2003/05/12 12:07:12  subhasis
 * Mapping added
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




