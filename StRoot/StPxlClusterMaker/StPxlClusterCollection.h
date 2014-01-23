/*!
 * \class StPxlClusterCollection 
 * \author Qiu Hao, March 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlClusterCollection.h,v 1.1 2014/01/23 01:04:43 qiuh Exp $
 *
 * Author: Qiu Hao, March 2013
 ***************************************************************************
 *
 * Description:
 * pxl cluster collection
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlClusterCollection.h,v $
 * Revision 1.1  2014/01/23 01:04:43  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/
#ifndef StPxlClusterCollection_hh
#define StPxlClusterCollection_hh

#include "StObject.h"
#include "StPxlCluster.h"
#include "StPxlUtil/StPxlConstants.h"

class StPxlClusterCollection : public StObject {
public:
    StPxlClusterCollection();
    ~StPxlClusterCollection();
    void addCluster(Int_t sector, Int_t ladder, Int_t sensor, StPxlCluster* cluster); ///< add a cluster to the collection
    Int_t numberOfClusters(Int_t sector, Int_t ladder, Int_t sensor); ///< number of clusters in a sensor
    StPxlCluster* cluster(Int_t sector, Int_t ladder, Int_t sensor, Int_t clusterIndex); ///< pointer to a cluster
    virtual const char *GetCVS() const {
        static const char cvs[]="Tag $Name:  $ $Id: StPxlClusterCollection.h,v 1.1 2014/01/23 01:04:43 qiuh Exp $ built "__DATE__" "__TIME__ ;
        return cvs;
    }

protected:
    vector<StPxlCluster*> mClusterVec[nPxlSectors][nPxlLaddersPerSector][nPxlSensorsPerLadder]; ///< vectors to store cluster pointers

    ClassDef(StPxlClusterCollection,1)
};

#endif
