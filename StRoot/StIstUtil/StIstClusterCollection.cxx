/***************************************************************************
*
* $Id: StIstClusterCollection.cxx,v 1.2 2014/01/29 18:25:03 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* See header file. 
****************************************************************************
*
* $Log: StIstClusterCollection.cxx,v $
* Revision 1.2  2014/01/29 18:25:03  ypwang
* updating scripts
*
*
****************************************************************************
* StIstClusterCollection.cxx,v 1.0
* Revision 1.0 2013/11/04 15:15:30 Yaping
* Initial version
****************************************************************************/

#include "StIstCluster.h"
#include "StIstClusterCollection.h"

//deconstructor
StIstClusterCollection::~StIstClusterCollection() {/* no op */}

void StIstClusterCollection::Clear( Option_t *opt ){
    // clear the vector
    mClusterVec.clear();
}

ClassImp(StIstClusterCollection);
