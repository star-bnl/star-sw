/*!
 * \class StPxlClusterMaker
 * \author Qiu Hao, Jan 2013, according codes from Xiangming Sun
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlClusterMaker.cxx,v 1.12 2017/09/08 17:37:18 dongx Exp $
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
 * Revision 1.12  2017/09/08 17:37:18  dongx
 * change std::random_shuffle to std::rand to be consistent with STAR coding
 *
 * Revision 1.11  2017/09/01 02:58:33  dongx
 * Update to ensure idTruth is preserved for MC hits for overlapping scenarios between MC/data and two or more MC hits
 *
 * Revision 1.10  2014/02/27 03:50:17  qiuh
 * *** empty log message ***
 *
 * Revision 1.9  2014/02/27 00:44:20  smirnovd
 * Switch to c++ style array zeroing
 *
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

#include <cstdlib>
#include <map>

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
   
   LOG_INFO << " Before clustering. Number of PxlRawHits = " << pxlRawHitCollection->numberOfRawHits() << endm;

   // output cluster data structures
   mPxlClusterCollection = new StPxlClusterCollection();
   ToWhiteBoard("pxlCluster", mPxlClusterCollection);

   // Set all elements (pointers) of rawHitMap to 0
   fill_n(*mRawHitMap, kNumberOfPxlRowsOnSensor * kNumberOfPxlColumnsOnSensor, static_cast<StPxlRawHit*>(0) );

   // real work
   int embeddingShortCut = IAttr("EmbeddingShortCut");
   int nIdTruth = 0;
   for (int i = 0; i < kNumberOfPxlSectors; i++)
      for (int j = 0; j < kNumberOfPxlLaddersPerSector; j++)
         for (int k = 0; k < kNumberOfPxlSensorsPerLadder; k++) {

            std::map<int, std::vector<int>> firedPixelsMap;
            for (int iHit=0; iHit < pxlRawHitCollection->numberOfRawHits(i+1, j+1, k+1); ++iHit)
            {
              StPxlRawHit const* rawHit = pxlRawHitCollection->rawHit(i + 1, j + 1, k + 1, iHit);
              int const id = rawHit->row() * 1000 + rawHit->column();
              firedPixelsMap[id].push_back(iHit);
            }

            for(auto& pixel: firedPixelsMap)
            {
              std::vector<int> mcHits;
              for(auto const& rawHitIdx: pixel.second)
              {
                StPxlRawHit const* rawHit = pxlRawHitCollection->rawHit(i + 1, j + 1, k + 1, rawHitIdx);
                if(rawHit->idTruth() > 0) mcHits.push_back(rawHitIdx);
              }

              if(!mcHits.empty()) // if any of the hits is MC then pick a random mc hit
              {
                int const rnd_idx = std::rand() % static_cast<int>(mcHits.size());
                StPxlRawHit const* rawHit = pxlRawHitCollection->rawHit(i + 1, j + 1, k + 1, mcHits[rnd_idx]);
                mRawHitMap[rawHit->row()][rawHit->column()] = rawHit;
              }
              else // pick a random hit
              {
                int const rnd_idx = std::rand() % static_cast<int>(pixel.second.size());
                StPxlRawHit const* rawHit = pxlRawHitCollection->rawHit(i + 1, j + 1, k + 1, pixel.second[rnd_idx]);
                mRawHitMap[rawHit->row()][rawHit->column()] = rawHit;
              }
                          
              if(Debug() && pixel.second.size()>1) // in case of overlapping raw pixels
              {
                LOG_INFO << " ++ Two or more rawHits found in this pixel row/column = " << pixel.first/1000 << "/" << pixel.first%1000 << endm;
                for(size_t ih = 0; ih<pixel.second.size(); ++ih)
                {
                  StPxlRawHit const* rawHit = pxlRawHitCollection->rawHit(i + 1, j + 1, k + 1, pixel.second[ih]);                                  
                  LOG_INFO << "      rawHit #" << ih << "\t idTruth=" << rawHit->idTruth() << endm;
                }
                LOG_INFO << "        => Selected rawHit idTruth = " << mRawHitMap[pixel.first/1000][pixel.first%1000]->idTruth() << endm;
              }
            }

            // find clusters
            for (int l = 0; l < pxlRawHitCollection->numberOfRawHits(i+1, j+1, k+1); l++) {
               const StPxlRawHit *rawHit = pxlRawHitCollection->rawHit(i + 1, j + 1, k + 1, l);
               StPxlCluster cluster;
               findCluster(&cluster, rawHit->column(), rawHit->row());
               if (cluster.nRawHits() > 0) {
                  cluster.summarize(embeddingShortCut);
                  mPxlClusterCollection->addCluster(i + 1, j + 1, k + 1, cluster);
                  if(cluster.idTruth()>0) {
                    LOG_DEBUG << " ==> A new cluster added sector/ladder/sensor/row/column = " << i+1 <<"/" << j+1 << "/" << k+1 << "/" << cluster.rowCenter() << "/" << cluster.columnCenter() << "\t nRawHits=" << cluster.nRawHits() << "\t idTruth=" << cluster.idTruth() << endm;
                    nIdTruth++;
                  }
               }
            }
         }
         
   LOG_INFO << " After clustering. Number of PxlClusters = " << mPxlClusterCollection->numberOfClusters() << " w/ idTruth = " << nIdTruth << endm;
            
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
