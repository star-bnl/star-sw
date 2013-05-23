/*!
 * \class StPxlClusterCollection 
 * \author Qiu Hao, March 2013
 */
/***************************************************************************
 *
 * $Id: StPxlClusterCollection.h,v 1.6 2013/05/23 21:58:15 qiuh Exp $
 *
 * Author: Qiu Hao, March 2013
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPxlClusterCollection.h,v $
 * Revision 1.6  2013/05/23 21:58:15  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/
#ifndef StPxlClusterCollection_hh
#define StPxlClusterCollection_hh

#include "StObject.h"
#include "StPxlCluster.h"
#include "StPxlConstants.h"

class StPxlClusterCollection : public StObject {
public:
    StPxlClusterCollection();
    ~StPxlClusterCollection();
    
    vector<StPxlCluster*> clusterVec[nPxlSectors][nPxlLaddersPerSector][nPxlSensorsPerLadder];

    ClassDef(StPxlClusterCollection,1)
};

#endif
