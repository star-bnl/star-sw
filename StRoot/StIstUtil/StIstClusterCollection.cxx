/***************************************************************************
*
* $Id: StIstClusterCollection.cxx,v 1.1 2014/01/23 20:11:30 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* See header file. 
****************************************************************************
*
* $Log: StIstClusterCollection.cxx,v $
* Revision 1.1  2014/01/23 20:11:30  ypwang
* adding scripts
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
