/***************************************************************************
*
* $Id: StIstScanClusterAlgo.cxx,v 1.7 2014/08/22 15:55:15 smirnovd Exp $
*
* Author: Yaping Wang, October 2013
****************************************************************************
* Description:
* See header file.
***************************************************************************/

#include "StEvent.h"
#include "StEvent/StEnumerations.h"
#include "StMessMgr.h"
#include "StIstScanClusterAlgo.h"
#include "StRoot/StIstUtil/StIstCollection.h"
#include "StRoot/StIstUtil/StIstRawHitCollection.h"
#include "StRoot/StIstUtil/StIstRawHit.h"
#include "StRoot/StIstUtil/StIstClusterCollection.h"
#include "StRoot/StIstUtil/StIstCluster.h"
#include "StRoot/StIstUtil/StIstConsts.h"

#include <math.h>
#include <iostream>
#include <iterator>
#include <vector>
#include <algorithm>

StIstScanClusterAlgo::StIstScanClusterAlgo(): mSplitCluster(1), mTimeBin(-1)
{
   //nothing to do....
};

Int_t StIstScanClusterAlgo::Init()
{
   return kStOk;
};

Int_t StIstScanClusterAlgo::doClustering(const StIstCollection &istCollection, StIstRawHitCollection &rawHitsOriginal, StIstClusterCollection &clusters )
{
   StIstCluster *newCluster = 0;
   StIstRawHit *rawHitTemp = 0;
   StIstRawHit *rawHitMaxAdcTemp = 0;
   unsigned char clusterType = kIstScanClusterAlgo;
   unsigned char maxTb = -1, usedTb = -1;
   unsigned char ladder = 0, sensor = 0;
   float meanRow = 0., meanColumn = 0.;
   float totCharge = 0., totChargeErr = 0.;
   unsigned char clusterSize = 0, clusterSizeRPhi = 0, clusterSizeZ = 0;

   //get number of time bin used in this event
   static unsigned char nTimeBins = istCollection.getNumTimeBins();

   //sort raw hits in increasing order by geometry ID
   rawHitsOriginal.sortByGeoId();

   //copy raw hit collection to temporary vectors
   std::vector<StIstRawHit *>  rawHitsVec[kIstNumSensorsPerLadder][kIstNumColumnsPerSensor];
   std::vector<StIstCluster *> clustersVec[kIstNumSensorsPerLadder][kIstNumColumnsPerSensor];
   std::vector<StIstRawHit *>  rawHitsToMerge;
   std::vector<StIstRawHit *>::iterator rawHitIt;

   for (int sensorIdx = 0; sensorIdx < kIstNumSensorsPerLadder; sensorIdx++) {
      for (int columnIdx = 0; columnIdx < kIstNumColumnsPerSensor; columnIdx++) {
         rawHitsVec[sensorIdx][columnIdx].reserve(kIstNumRowsPerSensor);
         clustersVec[sensorIdx][columnIdx].reserve(kIstNumRowsPerSensor);
      }
   }

   rawHitsToMerge.reserve(kIstNumRowsPerSensor);

   for (std::vector< StIstRawHit * >::iterator rawHitPtr = rawHitsOriginal.getRawHitVec().begin(); rawHitPtr != rawHitsOriginal.getRawHitVec().end(); ++rawHitPtr) {
      int channelId   = (*rawHitPtr)->getChannelId();
      int sensorIndex = (int)(*rawHitPtr)->getSensor();
      int columnIndex = (int)(*rawHitPtr)->getColumn();

      if ( channelId < 0 || channelId >= kIstNumElecIds) {
         rawHitsOriginal.getRawHitVec().erase( rawHitPtr );
         --rawHitPtr;
         continue;
      }

      rawHitsVec[sensorIndex - 1][columnIndex - 1].push_back( new StIstRawHit( *(*rawHitPtr)) );
   }

   //do clustering
   int clusterLabel = 0;

   for (int sensorIdx = 0; sensorIdx < kIstNumSensorsPerLadder; sensorIdx++) {
      //step 1: do clustering for each column
      for (int columnIdx = 0; columnIdx < kIstNumColumnsPerSensor; columnIdx++) {
         while ( !rawHitsVec[sensorIdx][columnIdx].empty() ) {
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
                           (rawHitTemp->getCharge(rawHitTemp->getMaxTimeBin()) < rawHitNext->getCharge(rawHitNext->getMaxTimeBin())) ) {
                        float weightBack = rawHitNext->getCharge(rawHitNext->getMaxTimeBin()) / ((*rawHitsToMergePtr)->getCharge((*rawHitsToMergePtr)->getMaxTimeBin()) + rawHitNext->getCharge(rawHitNext->getMaxTimeBin()));

                        for (unsigned char iTB = 0; iTB < nTimeBins; iTB++) {
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

            if (maxTb < 0 || maxTb >= nTimeBins)               	maxTb   = rawHitMaxAdcTemp->getDefaultTimeBin();

            if (mTimeBin >= 0 && mTimeBin < nTimeBins)       		usedTb  = mTimeBin;
            else                                                usedTb  = maxTb;

            ladder      	= rawHitMaxAdcTemp->getLadder();
            sensor      	= rawHitMaxAdcTemp->getSensor(); // = sensorIdx + 1
            meanColumn      	= (float)rawHitMaxAdcTemp->getColumn(); // = columnIdx + 1
            clusterSize 	= (unsigned char)nToMerge;
            clusterSizeRPhi 	= (unsigned char)nToMerge;
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
               for (clusterIt2 = clustersVec[sensorIdx][columnIdx2].begin(); clusterIt2 != clustersVec[sensorIdx][columnIdx2].end() && !clustersVec[sensorIdx][columnIdx2].empty(); clusterIt2++) {
                  float rowDistance = (*clusterIt1)->getMeanRow() - (*clusterIt2)->getMeanRow();

                  if (TMath::Abs(rowDistance) < 0.5) { //here 0.5 means the distance between two clusters' weighted centers in row direction smaller than 0.5
                     totCharge = (*clusterIt1)->getTotCharge() + (*clusterIt2)->getTotCharge();
                     totChargeErr = sqrt(((*clusterIt1)->getTotChargeErr() * (*clusterIt1)->getTotChargeErr() * (*clusterIt1)->getNRawHits() + (*clusterIt2)->getTotChargeErr() * (*clusterIt2)->getTotChargeErr() * (*clusterIt2)->getNRawHits()) / ((*clusterIt1)->getNRawHits() + (*clusterIt2)->getNRawHits()));
                     clusterSize = (*clusterIt1)->getNRawHits() + (*clusterIt2)->getNRawHits();
                     clusterSizeRPhi = (*clusterIt1)->getNRawHitsRPhi() + (*clusterIt2)->getNRawHitsRPhi() - 1;
                     clusterSizeZ = (*clusterIt1)->getNRawHitsZ() + (*clusterIt2)->getNRawHitsZ();
                     meanRow = (*clusterIt1)->getMeanRow() * (*clusterIt1)->getTotCharge() / totCharge + (*clusterIt2)->getMeanRow() * (*clusterIt2)->getTotCharge() / totCharge;
                     meanColumn = (*clusterIt1)->getMeanColumn() * (*clusterIt1)->getTotCharge() / totCharge + (*clusterIt2)->getMeanColumn() * (*clusterIt2)->getTotCharge() / totCharge;

                     (*clusterIt2)->setMeanRow(meanRow);
                     (*clusterIt2)->setMeanColumn(meanColumn);
                     (*clusterIt2)->setTotCharge(totCharge);
                     (*clusterIt2)->setTotChargeErr(totChargeErr);
                     (*clusterIt2)->setNRawHits(clusterSize);
                     (*clusterIt2)->setNRawHitsRPhi(clusterSizeRPhi);
                     (*clusterIt2)->setNRawHitsZ(clusterSizeZ);

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
         if (clustersVec[sensorIdx][columnIdx].size() > 0) {
            for (clusterIt = clustersVec[sensorIdx][columnIdx].begin(); clusterIt != clustersVec[sensorIdx][columnIdx].end(); ++clusterIt)
               clusters.getClusterVec().push_back(*clusterIt);

            rawHitsVec[sensorIdx][columnIdx].clear();
            clustersVec[sensorIdx][columnIdx].clear();
         }
      }
   }

   return kStOk;
};
ClassImp(StIstScanClusterAlgo);


/***************************************************************************
*
* $Log: StIstScanClusterAlgo.cxx,v $
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
