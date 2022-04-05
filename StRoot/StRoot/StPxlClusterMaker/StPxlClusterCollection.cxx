/*!
 * \class StPxlClusterCollection
 * \author Qiu Hao, March 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlClusterCollection.cxx,v 1.5 2017/09/01 02:58:33 dongx Exp $
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
 * Revision 1.5  2017/09/01 02:58:33  dongx
 * Update to ensure idTruth is preserved for MC hits for overlapping scenarios between MC/data and two or more MC hits
 *
 * Revision 1.4  2014/02/27 00:44:08  smirnovd
 * Use constructor initializer list
 *
 * Revision 1.3  2014/01/28 19:29:35  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#include "StPxlClusterCollection.h"

ClassImp(StPxlClusterCollection)

StPxlClusterCollection::StPxlClusterCollection() : StObject(), mClusterVec()
{
}

void StPxlClusterCollection::addCluster(Int_t sector, Int_t ladder, Int_t sensor, const StPxlCluster &cluster)
{
   mClusterVec[sector - 1][ladder - 1][sensor - 1].push_back(cluster);
}

Int_t StPxlClusterCollection::numberOfClusters(Int_t sector, Int_t ladder, Int_t sensor) const
{
   return mClusterVec[sector - 1][ladder - 1][sensor - 1].size();
}

const StPxlCluster *StPxlClusterCollection::cluster(Int_t sector, Int_t ladder, Int_t sensor, Int_t clusterIndex) const
{
   return &mClusterVec[sector - 1][ladder - 1][sensor - 1][clusterIndex];
}

Int_t StPxlClusterCollection::numberOfClusters() const
{
   int n = 0;
   for (int i = 0; i < kNumberOfPxlSectors; i++) {
      for (int j = 0; j < kNumberOfPxlLaddersPerSector; j++) {
        for (int k = 0; k < kNumberOfPxlSensorsPerLadder; k++) {
           n += mClusterVec[i][j][k].size();
        }
      }
   }
   return n;  
}