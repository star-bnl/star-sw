#include "StEvent.h"
#include "StEvent/StEnumerations.h"
#include "StMessMgr.h"
#include "StIstScanClusterAlgo.h"
#include "StIstUtil/StIstCollection.h"
#include "StIstUtil/StIstRawHitCollection.h"
#include "StIstUtil/StIstRawHit.h"
#include "StIstUtil/StIstClusterCollection.h"
#include "StIstUtil/StIstCluster.h"
#include "StIstUtil/StIstConsts.h"

#include <math.h>
#include <iostream>
#include <iterator>
#include <vector>
#include <algorithm>


Int_t StIstScanClusterAlgo::doClustering(const StIstCollection &istCollection, StIstRawHitCollection &rawHitsOriginal, StIstClusterCollection &clusters )
{
   StIstCluster *newCluster = 0;
   StIstRawHit *rawHitTemp = 0;
   StIstRawHit *rawHitMaxAdcTemp = 0;
   Int_t clusterType = kIstScanClusterAlgo;
   Int_t maxTb = -1, usedTb = -1;
   Int_t ladder = 0, sensor = 0;
   Float_t meanRow = 0., meanColumn = 0.;
   Float_t totCharge = 0., totChargeErr = 0.;
   Int_t clusterSize = 0, clusterSizeRPhi = 0, clusterSizeZ = 0;
   unsigned short idTruth = 0;

   //get number of time bin used in this event
   Int_t nTimeBins = istCollection.getNumTimeBins();

   //sort raw hits in increasing order by geometry ID
   rawHitsOriginal.sortByGeoId();

   //copy raw hit collection to temporary vectors
   std::vector<StIstRawHit *>  rawHitsVec[kIstNumSensorsPerLadder][kIstNumColumnsPerSensor];
   std::vector<StIstCluster *> clustersVec[kIstNumSensorsPerLadder][kIstNumColumnsPerSensor];
   std::vector<StIstRawHit *>  rawHitsToMerge;

   for (int sensorIdx = 0; sensorIdx < kIstNumSensorsPerLadder; sensorIdx++) {
      for (int columnIdx = 0; columnIdx < kIstNumColumnsPerSensor; columnIdx++) {
         rawHitsVec[sensorIdx][columnIdx].reserve(kIstNumRowsPerSensor);
         clustersVec[sensorIdx][columnIdx].reserve(kIstNumRowsPerSensor);
      }
   }

   rawHitsToMerge.reserve(kIstNumRowsPerSensor);

   for (std::vector< StIstRawHit * >::iterator rawHitPtr = rawHitsOriginal.getRawHitVec().begin(); rawHitPtr != rawHitsOriginal.getRawHitVec().end(); ++rawHitPtr) {
      int sensorIndex = (int)(*rawHitPtr)->getSensor();
      int columnIndex = (int)(*rawHitPtr)->getColumn();
      rawHitsVec[sensorIndex - 1][columnIndex - 1].push_back( new StIstRawHit( *(*rawHitPtr)) );
   }

   //do clustering
   int clusterLabel = 0;

   for (int sensorIdx = 0; sensorIdx < kIstNumSensorsPerLadder; sensorIdx++)
   {
      //step 1: do clustering for each column
      for (int columnIdx = 0; columnIdx < kIstNumColumnsPerSensor; columnIdx++)
      {
         while ( !rawHitsVec[sensorIdx][columnIdx].empty() )
         {
            rawHitTemp 	     = rawHitsVec[sensorIdx][columnIdx].back();
            rawHitsVec[sensorIdx][columnIdx].pop_back();
            rawHitsToMerge.push_back(rawHitTemp);

            //count number to merge
            int nToMerge = 1;
            //find all raw hits that are neighboring
            std::vector<StIstRawHit *>::iterator rawHitsToMergePtr = rawHitsToMerge.begin();
            rawHitMaxAdcTemp = *rawHitsToMergePtr;
            StIstRawHit *rawHitNext = 0;
            StIstRawHit *rawHitTempBack = 0;

            while (rawHitsToMergePtr != rawHitsToMerge.end() && !rawHitsVec[sensorIdx][columnIdx].empty()) {
               rawHitTemp       = rawHitsVec[sensorIdx][columnIdx].back();
               rawHitTempBack   = rawHitsVec[sensorIdx][columnIdx].back();

               if ( (*rawHitsToMergePtr)->getRow() == rawHitTemp->getRow() + 1 ) {
                  rawHitsVec[sensorIdx][columnIdx].pop_back();

                  if (!rawHitsVec[sensorIdx][columnIdx].empty()) {
                     rawHitNext = rawHitsVec[sensorIdx][columnIdx].back();

                     if ( (rawHitTemp->getRow() == rawHitNext->getRow() + 1) &&
                           (rawHitTemp->getCharge(rawHitTemp->getMaxTimeBin()) < (*rawHitsToMergePtr)->getCharge((*rawHitsToMergePtr)->getMaxTimeBin())) &&
                           (rawHitTemp->getCharge(rawHitTemp->getMaxTimeBin()) < rawHitNext->getCharge(rawHitNext->getMaxTimeBin())) )
                     {
                        float weightBack = rawHitNext->getCharge(rawHitNext->getMaxTimeBin()) / ((*rawHitsToMergePtr)->getCharge((*rawHitsToMergePtr)->getMaxTimeBin()) + rawHitNext->getCharge(rawHitNext->getMaxTimeBin()));

                        for (int iTB = 0; iTB < nTimeBins; iTB++) {
                           rawHitTempBack->setCharge(weightBack * rawHitTemp->getCharge(iTB), iTB);
                           rawHitTemp->setCharge((1.0 - weightBack) * rawHitTemp->getCharge(iTB), iTB);
                        }

                        rawHitsVec[sensorIdx][columnIdx].push_back(rawHitTempBack);
                     }
                  }

                  ++nToMerge;
                  rawHitsToMerge.push_back(rawHitTemp);

                  if ( rawHitTemp->getCharge(rawHitTemp->getMaxTimeBin()) > (*rawHitsToMergePtr)->getCharge((*rawHitsToMergePtr)->getMaxTimeBin()) )
                     rawHitMaxAdcTemp = rawHitTemp;
               }

               ++rawHitsToMergePtr;
            }

            //used time bin index (raw hits with maximum ADC holds the time-bin priority)
            maxTb       = rawHitMaxAdcTemp->getMaxTimeBin();
            idTruth = rawHitMaxAdcTemp->getIdTruth();

            if (maxTb < 0 || maxTb >= nTimeBins)         maxTb  = rawHitMaxAdcTemp->getDefaultTimeBin();

            if (mTimeBin < nTimeBins)                    usedTb = mTimeBin;
            else                                         usedTb = maxTb;

            ladder      	= rawHitMaxAdcTemp->getLadder();
            sensor      	= rawHitMaxAdcTemp->getSensor(); // = sensorIdx + 1
            meanColumn      	= (float)rawHitMaxAdcTemp->getColumn(); // = columnIdx + 1
            clusterSize 	= nToMerge;
            clusterSizeRPhi 	= nToMerge;
            clusterSizeZ 	= 1;

            float tempCharge[nToMerge], tempChargeErr[nToMerge], tempRow[nToMerge];

            for (int i = 0; i < nToMerge; i++) {
               tempCharge[i] = 0.; tempChargeErr[i] = 0.; tempRow[i] = 0.;
            }

            float tempSumCharge = 0, tempSumChargeErrSquare = 0.;
            int mergeIdx = 0;

            for (rawHitsToMergePtr = rawHitsToMerge.begin(); rawHitsToMergePtr != rawHitsToMerge.end() && mergeIdx < nToMerge; ++rawHitsToMergePtr, ++mergeIdx) {
               tempCharge[mergeIdx]    = (*rawHitsToMergePtr)->getCharge(usedTb);
               tempChargeErr[mergeIdx] = (*rawHitsToMergePtr)->getChargeErr(usedTb);
               tempRow[mergeIdx]       = (float)(*rawHitsToMergePtr)->getRow();
               tempSumCharge += (*rawHitsToMergePtr)->getCharge(usedTb);
            }

            meanRow = 0;

            for (int iRawHit = 0; iRawHit < nToMerge; iRawHit++)  {
               meanRow		+= tempRow[iRawHit] * tempCharge[iRawHit] / tempSumCharge;
               tempSumChargeErrSquare	+= tempChargeErr[iRawHit] * tempChargeErr[iRawHit];
            }

            totCharge    = tempSumCharge;
            totChargeErr = sqrt(tempSumChargeErrSquare / nToMerge);

            newCluster = new StIstCluster((int)ladder * 10000 + clusterLabel, ladder, sensor, meanRow, meanColumn, totCharge, totChargeErr, clusterType);
            newCluster->setNRawHits(clusterSize);
            newCluster->setNRawHitsRPhi(clusterSizeRPhi);
            newCluster->setNRawHitsZ(clusterSizeZ);
            newCluster->setMaxTimeBin(maxTb);
            newCluster->setIdTruth(idTruth);

            clustersVec[sensorIdx][columnIdx].push_back(newCluster);
            clusterLabel++;

            rawHitsToMerge.clear();
         }//end current column raw hits loop
      }//end current sensor raw hits loop

      //step 2: do clustering for neighboring rows
      std::vector<StIstCluster *>::iterator clusterIt1, clusterIt2;

      for (int columnIdx1 = 0; columnIdx1 < kIstNumColumnsPerSensor - 1; columnIdx1++) {
         int columnIdx2 = columnIdx1 + 1;

         if (clustersVec[sensorIdx][columnIdx1].size() > 0 && clustersVec[sensorIdx][columnIdx2].size() > 0) {
            for (clusterIt1 = clustersVec[sensorIdx][columnIdx1].begin(); clusterIt1 != clustersVec[sensorIdx][columnIdx1].end() && !clustersVec[sensorIdx][columnIdx1].empty(); clusterIt1++) {
               for (clusterIt2 = clustersVec[sensorIdx][columnIdx2].begin(); clusterIt2 != clustersVec[sensorIdx][columnIdx2].end() && !clustersVec[sensorIdx][columnIdx1].empty(); clusterIt2++) {
                  float rowDistance = (*clusterIt1)->getMeanRow() - (*clusterIt2)->getMeanRow();

                  if (TMath::Abs(rowDistance) < 0.5) { //here 0.5 means the distance between two clusters' weighted centers in row direction smaller than 0.5
                     maxTb = (*clusterIt1)->getMaxTimeBin();
                     idTruth = (*clusterIt1)->getIdTruth();
                     if((*clusterIt1)->getTotCharge() < (*clusterIt2)->getTotCharge()) {
                        maxTb = (*clusterIt2)->getMaxTimeBin();
                        idTruth = (*clusterIt2)->getIdTruth();
                     }

                     totCharge       = (*clusterIt1)->getTotCharge() + (*clusterIt2)->getTotCharge();
                     totChargeErr    = sqrt(((*clusterIt1)->getTotChargeErr() * (*clusterIt1)->getTotChargeErr() * (*clusterIt1)->getNRawHits() + (*clusterIt2)->getTotChargeErr() * (*clusterIt2)->getTotChargeErr() * (*clusterIt2)->getNRawHits()) / ((*clusterIt1)->getNRawHits() + (*clusterIt2)->getNRawHits()));
                     clusterSize     = (*clusterIt1)->getNRawHits() + (*clusterIt2)->getNRawHits();
                     clusterSizeRPhi = (*clusterIt1)->getNRawHitsRPhi() + (*clusterIt2)->getNRawHitsRPhi() - 1;
                     clusterSizeZ    = (*clusterIt1)->getNRawHitsZ() + (*clusterIt2)->getNRawHitsZ();
                     meanRow         = (*clusterIt1)->getMeanRow() * (*clusterIt1)->getTotCharge() / totCharge + (*clusterIt2)->getMeanRow() * (*clusterIt2)->getTotCharge() / totCharge;
                     meanColumn      = (*clusterIt1)->getMeanColumn() * (*clusterIt1)->getTotCharge() / totCharge + (*clusterIt2)->getMeanColumn() * (*clusterIt2)->getTotCharge() / totCharge;

                     (*clusterIt2)->setMeanRow(meanRow);
                     (*clusterIt2)->setMeanColumn(meanColumn);
                     (*clusterIt2)->setTotCharge(totCharge);
                     (*clusterIt2)->setTotChargeErr(totChargeErr);
                     (*clusterIt2)->setNRawHits(clusterSize);
                     (*clusterIt2)->setNRawHitsRPhi(clusterSizeRPhi);
                     (*clusterIt2)->setNRawHitsZ(clusterSizeZ);
                     (*clusterIt2)->setIdTruth(idTruth);

                     int distance1 = std::distance(clustersVec[sensorIdx][columnIdx1].begin(), clusterIt1);
                     clustersVec[sensorIdx][columnIdx1].erase(clusterIt1);

                     if (distance1 == 0)
                        clusterIt1 = clustersVec[sensorIdx][columnIdx1].begin();
                     else
                        --clusterIt1;
                  }//end merge
               }//end clusters vector 2 loop
            }//end clusters vector 1 loop
         }//end column cluster number cut
      }//end current sensor cluster loop
   }//end all sensor clustering loop

   //fill output container
   std::vector<StIstCluster *>::iterator clusterIt;

   for (int sensorIdx = 0; sensorIdx < kIstNumSensorsPerLadder; sensorIdx++) {
      for (int columnIdx = 0; columnIdx < kIstNumColumnsPerSensor; columnIdx++) {
         if (clustersVec[sensorIdx][columnIdx].size() <= 0) continue;

         for (clusterIt = clustersVec[sensorIdx][columnIdx].begin(); clusterIt != clustersVec[sensorIdx][columnIdx].end(); ++clusterIt)
            clusters.getClusterVec().push_back(*clusterIt);

         rawHitsVec[sensorIdx][columnIdx].clear();
         clustersVec[sensorIdx][columnIdx].clear();
      }
   }

   return kStOk;
}


/***************************************************************************
*
* $Log: StIstScanClusterAlgo.cxx,v $
* Revision 1.20  2018/01/04 17:34:37  smirnovd
* [Cosmetic] Remove StRoot/ from include path
*
* $STAR/StRoot is already in the default path search
*
* Revision 1.19  2016/02/18 17:27:19  smirnovd
* StIstScanClusterAlgo: Whitespace corrected in "Add idTruth for clusters."
*
* Corrected commit 45e825e1
*
* Revision 1.18  2016/02/17 19:52:50  huangbc
* Add idTruth for clusters.
*
* Revision 1.17  2015/05/20 20:53:57  smirnovd
* Removed a priori true condition without changing the logic
*
* mTimeBin is unsigned char always >= 0
*
* Revision 1.16  2015/03/04 16:17:16  smirnovd
* Added a check for empty container of to-be-merged proto-clusters
*
* This is done to avoid a situation when we run out of column-wise clusters in the
* current (happens when all clusters are merged and removed from the temporary
* container) column but the next one still has some cluster left.
*
* Revision 1.15  2015/03/04 16:17:07  smirnovd
* Revert "empty check was added for the to-be-merged column-wise proto-clusters container, to fix the RT #3056 oberserved by Lidia"
*
* Revision 1.13  2014/09/18 06:27:25  ypwang
* remove unneccessary check for raw hit electroincis ID check
*
* Revision 1.12  2014/09/17 20:39:45  smirnovd
* Squashed commit of the following:
*
* commit 37d3d404a31c9b152811232af55d37177162269d
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 16:11:22 2014 -0400
*
*     Added an author to reflect on contributions
*
* commit 6ceacb443d2d35bc21295b81a3d25b7433d40260
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 16:09:48 2014 -0400
*
*     [Minor] Reversed the logic and saved one level of intentation
*
* commit 4bc24031445ecce9f19b940697d13cc8a755aaf1
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 16:06:42 2014 -0400
*
*     Do not use standard ROOT's dictionary macroses since the classes are transient by design
*
* Revision 1.11  2014/09/17 20:33:32  smirnovd
* Squashed commit of the following:
*
* commit 72dc19a6663ea31c719c1a61f6d2b4752dd766aa
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:34:42 2014 -0400
*
*     Minor code refactoring, clean up
*
* commit e083a10a9fb60b7dcce692ef8043b9227c12768b
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:18:16 2014 -0400
*
*     Removed pointless comments
*
* commit 88d51857362c91c954704cec4a31a0b0fa7fccc5
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:17:26 2014 -0400
*
*     Updated description in doxygen comments
*
* commit eb09527489179fc7dab6aa7f23fd132b25185bb1
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Tue Sep 9 15:15:56 2014 -0400
*
*     StIstScanClusterAlgo: Removed unused variable
*
* commit 1a8df63533c71a0e2ba4d8275ebf89f4e3004765
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Fri Aug 22 16:04:47 2014 -0400
*
*     Neatened headers: Removed unused, spelled paths in includes explicitly as it slightly helps in identifying dependencies
*
* commit 972e8ed41403bd680ade5ecc509f8bca004e86ee
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:34:20 2014 -0400
*
*     Minor stylistic changes
*
* commit 57daf5a1e0b3246fd12f1dd1c2ca089b62930c83
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Tue Sep 16 16:29:14 2014 -0400
*
*     Improved doxygen comments
*
* Revision 1.10  2014/09/09 07:34:07  ypwang
* data type was updated from UChar_t to Int_t for several varibales, and the nTimeBins was corrected as a non-static varible
*
* Revision 1.9  2014/09/07 13:54:45  ypwang
* move setUsedTimeBin() and setSplitFlag() setters from inherited classes to their base class StIstIClusterAlgo.h
*
* Revision 1.8  2014/09/07 11:41:36  ypwang
* ClassDef version updated from 1 to 0, and remove Init() function
*
* Revision 1.7  2014/08/22 15:55:15  smirnovd
* Fixed style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.6  2014/08/22 15:50:00  smirnovd
* Moved CVS history to the end of file
*
* Revision 1.5  2014/03/17 21:51:56  ypwang
* minor update due to some IST constants moved to StEnumurations.h
*
* Revision 1.4  2014/02/16 23:18:34  ypwang
* getting number of time bins used in current event by StIstCollection::getNumTimeBins() function
*
* Revision 1.3  2014/02/08 03:34:16  ypwang
* updating scripts
*
*
****************************************************************************
* StIstScanClusterAlgo.cxx,v 1.0
* Revision 1.0 2013/11/04 15:55:30 Yaping
* Initial version
****************************************************************************/
