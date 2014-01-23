/*!
 * \class StPxlClusterCollection
 * \author Qiu Hao, March 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlClusterCollection.cxx,v 1.1 2014/01/23 01:04:43 qiuh Exp $
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
 * $Log: StPxlClusterCollection.cxx,v $
 * Revision 1.1  2014/01/23 01:04:43  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#include "StPxlClusterCollection.h"

ClassImp(StPxlClusterCollection)

StPxlClusterCollection::StPxlClusterCollection()
{
}

StPxlClusterCollection::~StPxlClusterCollection()
{
    for (int i=0; i<nPxlSectors; i++)
        for(int j=0; j<nPxlLaddersPerSector; j++)
            for(int k=0; k<nPxlSensorsPerLadder; k++)
                {
                    int vecSize = mClusterVec[i][j][k].size();
                    for(int l=0; l<vecSize; l++)
                        {
                            delete mClusterVec[i][j][k][l];
                        }
                    mClusterVec[i][j][k].clear();
                }
}

void StPxlClusterCollection::addCluster(Int_t sector, Int_t ladder, Int_t sensor, StPxlCluster* cluster)
{
    mClusterVec[sector-1][ladder-1][sensor-1].push_back(cluster);
}

Int_t StPxlClusterCollection::numberOfClusters(Int_t sector, Int_t ladder, Int_t sensor)
{
    return mClusterVec[sector-1][ladder-1][sensor-1].size();
}

StPxlCluster* StPxlClusterCollection::cluster(Int_t sector, Int_t ladder, Int_t sensor, Int_t clusterIndex)
{
    return mClusterVec[sector-1][ladder-1][sensor-1][clusterIndex];
}
