/***************************************************************************
 *
 * $Id: StPxlClusterCollection.cxx,v 1.3 2013/05/23 21:20:17 qiuh Exp $
 *
 * Author: Qiu Hao, March 2013
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPxlClusterCollection.cxx,v $
 * Revision 1.3  2013/05/23 21:20:17  qiuh
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
                    int vecSize = clusterVec[i][j][k].size();
                    for(int l=0; l<vecSize; l++)
                        delete clusterVec[i][j][k][l];
                    clusterVec[i][j][k].clear();
                }
}


