/*!
 * \class StPxlClusterMaker
 * \author Qiu Hao, Jan 2013, according codes from Xiangming Sun
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlClusterMaker.cxx,v 1.8 2014/02/27 00:44:08 smirnovd Exp $
 *
 * Author: Qiu Hao, Jan 2013, according codes from Xiangming Sun
 ***************************************************************************
 *
 * Description:
 * Group neighboring pixel raw hits from into clusters.
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlClusterMaker.cxx,v $
 * Revision 1.8  2014/02/27 00:44:08  smirnovd
 * Use constructor initializer list
 *
 * Revision 1.7  2014/02/21 21:11:06  smirnovd
 * Minor style and empty space adjustments
 *
 * Revision 1.6  2014/02/21 21:10:58  smirnovd
 * Move zeroing of mRawHitMap outside the loop. The map is zeroed automatically during the cluster finding
 *
 * Revision 1.5  2014/01/28 19:29:35  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#include "StPxlClusterMaker.h"
#include "StMessMgr.h"
#include "StPxlCluster.h"
#include "StPxlClusterCollection.h"
#include "StPxlRawHitMaker/StPxlRawHit.h"
#include "StPxlRawHitMaker/StPxlRawHitCollection.h"
#include "StPxlUtil/StPxlConstants.h"

ClassImp(StPxlClusterMaker);

//________________________________________________________________________________
StPxlClusterMaker::StPxlClusterMaker(const Char_t *name) : StMaker(name),
   mPxlClusterCollection(0),
   mRawHitMap()
{
}
//________________________________________________________________________________
void StPxlClusterMaker::Clear(const Option_t *)
{
   if (mPxlClusterCollection) {
      delete mPxlClusterCollection;
      mPxlClusterCollection = 0;
   }
   return StMaker::Clear();
}
//________________________________________________________________________________
Int_t StPxlClusterMaker::Make()
{
   // input data
   TObjectSet *pxlRawHitDataSet = (TObjectSet *)GetDataSet("pxlRawHit");
   if (! pxlRawHitDataSet) {
      LOG_WARN << "Make() - there is no pxlRawHitDataSet " << endm;
      return kStWarn;
   }

   StPxlRawHitCollection *pxlRawHitCollection = (StPxlRawHitCollection *)pxlRawHitDataSet->GetObject();
   if (!pxlRawHitCollection) {
      LOG_WARN << "Make() - no pxlRawHitCollection." << endm;
      return kStWarn;
   }

   // output cluster data structures
   mPxlClusterCollection = new StPxlClusterCollection();
   ToWhiteBoard("pxlCluster", mPxlClusterCollection);

   // clear rawHitMap
   memset(mRawHitMap, 0, kNumberOfPxlRowsOnSensor * kNumberOfPxlColumnsOnSensor * sizeof(StPxlRawHit *));

   // real work
   int embeddingShortCut = IAttr("EmbeddingShortCut");
   for (int i = 0; i < kNumberOfPxlSectors; i++)
      for (int j = 0; j < kNumberOfPxlLaddersPerSector; j++)
         for (int k = 0; k < kNumberOfPxlSensorsPerLadder; k++) {

            // load rawHitMap
            int vectorSize = pxlRawHitCollection->numberOfRawHits(i + 1, j + 1, k + 1);
            for (int l = 0; l < vectorSize; l++) {
               const StPxlRawHit *rawHit = pxlRawHitCollection->rawHit(i + 1, j + 1, k + 1, l);
               mRawHitMap[rawHit->row()][rawHit->column()] = rawHit;
            }

            // find clusters
            for (int l = 0; l < vectorSize; l++) {
               const StPxlRawHit *rawHit = pxlRawHitCollection->rawHit(i + 1, j + 1, k + 1, l);
               StPxlCluster cluster;
               findCluster(&cluster, rawHit->column(), rawHit->row());
               if (cluster.nRawHits() > 0) {
                  cluster.summarize(embeddingShortCut);
                  mPxlClusterCollection->addCluster(i + 1, j + 1, k + 1, cluster);
               }
            }
         }

   return kStOK;
}


/**
 * Start from (column, row), look at 8 neighboring pixels for fired pixels (raw
 * hits) If a raw hit is found nearby, continue to look from that pixel, until
 * all neighboring raw hits are found Fill the neighboring raw hits into
 * a cluster
 */
void StPxlClusterMaker::findCluster(StPxlCluster *cluster, Int_t column, Int_t row)
{
   const StPxlRawHit *rawHit = mRawHitMap[row][column];
   if ( !rawHit ) return; // skip if already included in another cluster

   mRawHitMap[row][column] = 0; // unmark this used raw hit

   // looking at the 8 neighboring pixels, if fired, continue looking from that pixel
   if ((column - 1) >= 0)
      findCluster(cluster, column - 1, row);

   if ((column + 1) < kNumberOfPxlColumnsOnSensor)
      findCluster(cluster, column + 1, row);

   if ((row - 1) >= 0)
      findCluster(cluster, column, row - 1);

   if ((row + 1) < kNumberOfPxlRowsOnSensor)
      findCluster(cluster, column, row + 1);

   if (((column - 1) >= 0) && ((row - 1) >= 0))
      findCluster(cluster, column - 1, row - 1);

   if (((column - 1) >= 0) && ((row + 1) < kNumberOfPxlRowsOnSensor))
      findCluster(cluster, column - 1, row + 1);

   if (((column + 1) < kNumberOfPxlColumnsOnSensor) && ((row - 1) >= 0))
      findCluster(cluster, column + 1, row - 1);

   if (((column + 1) < kNumberOfPxlColumnsOnSensor) && ((row + 1) < kNumberOfPxlRowsOnSensor))
      findCluster(cluster, column + 1, row + 1);

   // Add hit to the cluster
   cluster->addRawHit(rawHit);
}
