#include "StEvent/StEvent.h"
#include "StMessMgr.h"
#include "StIstClusterMaker/StIstSimpleClusterAlgo.h"
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


Int_t StIstSimpleClusterAlgo::splitCluster(int cSize, int clusterSizeList[], StIstRawHit *rawHitPtr[], StIstCluster *clusterIt, StIstClusterCollection &clusters, unsigned char numTimeBins)
{
   StIstRawHit *rawHitPtr0 = 0;
   StIstRawHit *rawHitPtr1 = 0;
   StIstRawHit *rawHitPtr2 = 0;
   StIstRawHit *rawHitPtr3 = 0;

   StIstCluster *newClusterTmp = 0;

   unsigned char maxTb = UCHAR_MAX;
   unsigned char ladder = 0, sensor = 0;
   float meanRow = 0, meanColumn = 0;
   float totCharge = 0., totChargeErr = 0.;
   unsigned char clusterSize = 0; unsigned char clusterSizeRPhi = 0;
   unsigned char clusterSizeZ = 0;
   unsigned char clusterType = kIstSimpleClusterAlgo;

   float weightSplit = 0., weightSplit2 = 0., tmpSumChargeSplit = 0.;

   //split cluster w/ size = 3
   if (cSize == 3) {
      rawHitPtr0 = rawHitPtr[0];
      rawHitPtr1 = rawHitPtr[1];
      rawHitPtr2 = rawHitPtr[2];

      unsigned char tmpRawHitMaxTb0, tmpRawHitMaxTb1, tmpRawHitMaxTb2;

      if (mTimeBin < numTimeBins) {
         tmpRawHitMaxTb0 = tmpRawHitMaxTb1 = tmpRawHitMaxTb2 = mTimeBin;
      }
      else {
         tmpRawHitMaxTb0 = tmpRawHitMaxTb1 = tmpRawHitMaxTb2 = clusterIt->getMaxTimeBin();
      }

      float tmpRawHitMaxCharge0 = rawHitPtr0->getCharge(tmpRawHitMaxTb0);
      float tmpRawHitMaxCharge1 = rawHitPtr1->getCharge(tmpRawHitMaxTb1);
      float tmpRawHitMaxCharge2 = rawHitPtr2->getCharge(tmpRawHitMaxTb2);

      //cluster spliting: raw hit 0 + part of raw hit 1
      tmpSumChargeSplit = tmpRawHitMaxCharge0 + tmpRawHitMaxCharge1 * tmpRawHitMaxCharge0 / (tmpRawHitMaxCharge0 + tmpRawHitMaxCharge2);
      weightSplit = tmpRawHitMaxCharge0 / tmpSumChargeSplit;

      clusterSize     = clusterSizeList[0];
      clusterSizeZ    = clusterSizeList[1];
      clusterSizeRPhi = clusterSizeList[2];

      ladder    = rawHitPtr0->getLadder();
      sensor    = rawHitPtr0->getSensor();
      maxTb     = tmpRawHitMaxTb0;
      meanColumn = weightSplit * rawHitPtr0->getColumn() + (1.0 - weightSplit) * rawHitPtr1->getColumn();
      meanRow   = weightSplit * rawHitPtr0->getRow()    + (1.0 - weightSplit) * rawHitPtr1->getRow();
      totCharge = tmpSumChargeSplit;
      totChargeErr = sqrt( (rawHitPtr0->getChargeErr(tmpRawHitMaxTb0) * rawHitPtr0->getChargeErr(tmpRawHitMaxTb0) + rawHitPtr1->getChargeErr(tmpRawHitMaxTb1) * rawHitPtr1->getChargeErr(tmpRawHitMaxTb1)) / clusterSize );

      clusterIt->setLadder(ladder);
      clusterIt->setSensor(sensor);
      clusterIt->setMeanColumn(meanColumn);
      clusterIt->setMeanRow(meanRow);
      clusterIt->setTotCharge(totCharge);
      clusterIt->setTotChargeErr(totChargeErr);
      clusterIt->setNRawHits(clusterSize);
      clusterIt->setNRawHitsRPhi(clusterSizeRPhi);
      clusterIt->setNRawHitsZ(clusterSizeZ);
      clusterIt->setMaxTimeBin(maxTb);
      clusterIt->setClusteringType(clusterType);

      std::vector< StIstRawHit * > &rawHitVecTmp = clusterIt->getRawHitVec();
      std::vector< StIstRawHit * >::iterator rawHitIterator = rawHitVecTmp.end();
      rawHitIterator--;
      rawHitVecTmp.erase(rawHitIterator); //remove the rawHit[2]

      //cluster spliting: part of raw hit 1 + raw hit 2
      tmpSumChargeSplit = tmpRawHitMaxCharge2 + tmpRawHitMaxCharge1 * tmpRawHitMaxCharge2 / (tmpRawHitMaxCharge0 + tmpRawHitMaxCharge2);
      weightSplit = tmpRawHitMaxCharge2 / tmpSumChargeSplit;

      clusterSize     = clusterSizeList[3];
      clusterSizeZ    = clusterSizeList[4];
      clusterSizeRPhi = clusterSizeList[5];

      ladder    = rawHitPtr2->getLadder();
      sensor    = rawHitPtr2->getSensor();
      maxTb     = tmpRawHitMaxTb2;
      meanColumn = weightSplit * rawHitPtr2->getColumn() + (1.0 - weightSplit) * rawHitPtr1->getColumn();
      meanRow   = weightSplit * rawHitPtr2->getRow()    + (1.0 - weightSplit) * rawHitPtr1->getRow();
      totCharge    = tmpSumChargeSplit;
      totChargeErr = sqrt( (rawHitPtr2->getChargeErr(tmpRawHitMaxTb2) * rawHitPtr2->getChargeErr(tmpRawHitMaxTb2) + rawHitPtr1->getChargeErr(tmpRawHitMaxTb1) * rawHitPtr1->getChargeErr(tmpRawHitMaxTb1)) / clusterSize );

      newClusterTmp = new StIstCluster((int)ladder * 10000 + clusters.getClusterVec().size(), ladder, sensor, meanRow, meanColumn, totCharge, totChargeErr, clusterType);
      newClusterTmp->setNRawHits(clusterSize);
      newClusterTmp->setNRawHitsRPhi(clusterSizeRPhi);
      newClusterTmp->setNRawHitsZ(clusterSizeZ);
      newClusterTmp->setMaxTimeBin(maxTb);

      std::vector< StIstRawHit * > &rawHitVecTmp2 = newClusterTmp->getRawHitVec();
      rawHitVecTmp2.push_back(rawHitPtr1);
      rawHitVecTmp2.push_back(rawHitPtr2);

      clusters.getClusterVec().push_back(newClusterTmp);
   }

   // split cluster w/ size = 4
   if (cSize == 4) {
      rawHitPtr0 = rawHitPtr[0];
      rawHitPtr1 = rawHitPtr[1];
      rawHitPtr2 = rawHitPtr[2];
      rawHitPtr3 = rawHitPtr[3];

      unsigned char tmpRawHitMaxTb0, tmpRawHitMaxTb1, tmpRawHitMaxTb2, tmpRawHitMaxTb3;

      if (mTimeBin < numTimeBins) {
         tmpRawHitMaxTb0 = tmpRawHitMaxTb1 = tmpRawHitMaxTb2 = tmpRawHitMaxTb3 = mTimeBin;
      }
      else {
         tmpRawHitMaxTb0 = tmpRawHitMaxTb1 = tmpRawHitMaxTb2 = tmpRawHitMaxTb3 = clusterIt->getMaxTimeBin();
      }

      float tmpRawHitMaxCharge0 = rawHitPtr0->getCharge(tmpRawHitMaxTb0);
      float tmpRawHitMaxCharge1 = rawHitPtr1->getCharge(tmpRawHitMaxTb1);
      float tmpRawHitMaxCharge2 = rawHitPtr2->getCharge(tmpRawHitMaxTb2);
      float tmpRawHitMaxCharge3 = rawHitPtr3->getCharge(tmpRawHitMaxTb3);

      //cluster spliting: raw hit 0 + part of raw hit 1
      tmpSumChargeSplit = tmpRawHitMaxCharge0 + tmpRawHitMaxCharge1 * tmpRawHitMaxCharge0 / (tmpRawHitMaxCharge0 + tmpRawHitMaxCharge2 + tmpRawHitMaxCharge3);
      weightSplit = tmpRawHitMaxCharge0 / tmpSumChargeSplit;

      clusterSize     = clusterSizeList[0];
      clusterSizeZ    = clusterSizeList[1];
      clusterSizeRPhi = clusterSizeList[2];

      ladder    = rawHitPtr0->getLadder();
      sensor    = rawHitPtr0->getSensor();
      maxTb     = tmpRawHitMaxTb0;
      meanColumn = weightSplit * rawHitPtr0->getColumn() + (1.0 - weightSplit) * rawHitPtr1->getColumn();
      meanRow   = weightSplit * rawHitPtr0->getRow()    + (1.0 - weightSplit) * rawHitPtr1->getRow();
      totCharge    = tmpSumChargeSplit;
      totChargeErr = sqrt( (rawHitPtr0->getChargeErr(tmpRawHitMaxTb0) * rawHitPtr0->getChargeErr(tmpRawHitMaxTb0) + rawHitPtr1->getChargeErr(tmpRawHitMaxTb1) * rawHitPtr1->getChargeErr(tmpRawHitMaxTb1)) / clusterSize );

      clusterIt->setLadder(ladder);
      clusterIt->setSensor(sensor);
      clusterIt->setMeanColumn(meanColumn);
      clusterIt->setMeanRow(meanRow);
      clusterIt->setTotCharge(totCharge);
      clusterIt->setTotChargeErr(totChargeErr);
      clusterIt->setNRawHits(clusterSize);
      clusterIt->setNRawHitsRPhi(clusterSizeRPhi);
      clusterIt->setNRawHitsZ(clusterSizeZ);
      clusterIt->setMaxTimeBin(maxTb);
      clusterIt->setClusteringType(clusterType);

      std::vector< StIstRawHit * > &rawHitVecTmp3 = clusterIt->getRawHitVec();
      std::vector< StIstRawHit * >::iterator rawHitIterator = rawHitVecTmp3.end();
      rawHitIterator--;
      rawHitVecTmp3.erase(rawHitIterator); //remove the rawHit[3]
      rawHitIterator--;
      rawHitVecTmp3.erase(rawHitIterator); //remove the rawHit[2]

      //cluster spliting: part of raw hit 1 + raw hit 2 + raw hit 3
      tmpSumChargeSplit = tmpRawHitMaxCharge2 + tmpRawHitMaxCharge3 + tmpRawHitMaxCharge1 * (tmpRawHitMaxCharge2 + tmpRawHitMaxCharge3) / (tmpRawHitMaxCharge0 + tmpRawHitMaxCharge2 + tmpRawHitMaxCharge3);
      weightSplit = tmpRawHitMaxCharge2 / tmpSumChargeSplit;
      weightSplit2 = tmpRawHitMaxCharge3 / tmpSumChargeSplit;

      clusterSize     = clusterSizeList[3];
      clusterSizeZ    = clusterSizeList[4];
      clusterSizeRPhi = clusterSizeList[5];

      ladder    = rawHitPtr2->getLadder();
      sensor    = rawHitPtr2->getSensor();
      maxTb     = tmpRawHitMaxTb2;
      meanColumn = weightSplit * rawHitPtr2->getColumn() + weightSplit2 * rawHitPtr3->getColumn() + (1.0 - weightSplit - weightSplit2) * rawHitPtr1->getColumn();
      meanRow   = weightSplit * rawHitPtr2->getRow()    + weightSplit2 * rawHitPtr3->getRow()    + (1.0 - weightSplit - weightSplit2) * rawHitPtr1->getRow();
      totCharge    = tmpSumChargeSplit;
      totChargeErr  = sqrt( (rawHitPtr3->getChargeErr(tmpRawHitMaxTb3) * rawHitPtr3->getChargeErr(tmpRawHitMaxTb3) + rawHitPtr2->getChargeErr(tmpRawHitMaxTb2) * rawHitPtr2->getChargeErr(tmpRawHitMaxTb2) + rawHitPtr1->getChargeErr(tmpRawHitMaxTb1) * rawHitPtr1->getChargeErr(tmpRawHitMaxTb1)) / clusterSize );

      newClusterTmp = new StIstCluster((int)ladder * 10000 + clusters.getClusterVec().size(), ladder, sensor, meanRow, meanColumn, totCharge, totChargeErr, clusterType);
      newClusterTmp->setNRawHits(clusterSize);
      newClusterTmp->setNRawHitsRPhi(clusterSizeRPhi);
      newClusterTmp->setNRawHitsZ(clusterSizeZ);
      newClusterTmp->setMaxTimeBin(maxTb);

      std::vector< StIstRawHit * > &rawHitVecTmp4 = newClusterTmp->getRawHitVec();
      rawHitVecTmp4.push_back(rawHitPtr1);
      rawHitVecTmp4.push_back(rawHitPtr2);
      rawHitVecTmp4.push_back(rawHitPtr3);

      clusters.getClusterVec().push_back(newClusterTmp);
   }

   return kStOk;
};

Int_t StIstSimpleClusterAlgo::doSplitting(StIstClusterCollection &clusters, unsigned char numTimeBins)
{
   //loop found clusters in current event
   for (std::vector< StIstCluster * >::iterator clusterIt = clusters.getClusterVec().begin(); clusterIt != clusters.getClusterVec().end(); clusterIt++)      {
      StIstRawHit *rawHitPtr[]        =   {0, 0, 0, 0};
      unsigned char tmpRawHitMaxTb[]  =   {0, 0, 0, 0};
      float tmpRawHitMaxCharge[]      =   {0., 0., 0., 0.};
      int tmpClusterSizeList[]        =   {1, 1, 1, 1, 1, 1};//{clusterSize_1, clusterSizeZ_1, clusterSizeRPhi_1, clusterSize_2, clusterSizeZ_2, clusterSizeRPhi_2}

      //splitting clusters w/ size = 3
      if ( (*clusterIt)->getNRawHits() == 3 )  {
         tmpClusterSizeList[0] = 2; tmpClusterSizeList[3] = 2;
         int index = 0;

         for (std::vector< StIstRawHit * >::iterator rawHitVecIt = (*clusterIt)->getRawHitVec().begin(); index < 3 && rawHitVecIt != (*clusterIt)->getRawHitVec().end(); rawHitVecIt++)      {
            rawHitPtr[index]        = *rawHitVecIt;

            if (mTimeBin < numTimeBins) {
               tmpRawHitMaxTb[index]   = mTimeBin;
            }
            else {
               tmpRawHitMaxTb[index]   = (*clusterIt)->getMaxTimeBin();//the raw hits belong to the same cluster should be at same time bin
            }

            tmpRawHitMaxCharge[index]       = rawHitPtr[index]->getCharge(tmpRawHitMaxTb[index]);
            index ++;
         }

         //cases 1-4:        2   2 1   2
         //topology: 2 1 0 , 1 ,   0 , 1 0
         //                  0
         if ( ((*clusterIt)->getNRawHitsZ() == 3) || ((*clusterIt)->getNRawHitsRPhi() == 3) ||
               (((*clusterIt)->getNRawHitsZ() == 2) && ((*clusterIt)->getNRawHitsRPhi() == 2) && (rawHitPtr[0]->getColumn() == rawHitPtr[2]->getColumn() - 1) && (rawHitPtr[0]->getRow() == rawHitPtr[2]->getRow() - 1)) ) {
            if ( (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[2]) )      {
               if ( (*clusterIt)->getNRawHitsZ() == 3 )  {
                  tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 1;
               }
               else if ( (*clusterIt)->getNRawHitsRPhi() == 3 )     {
                  tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 2;
               }
               else if ( ((*clusterIt)->getNRawHitsZ() == 2) && (rawHitPtr[0]->getColumn() == rawHitPtr[1]->getColumn()) )  {
                  tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 1;
               }
               else        {
                  tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 2;
               }

               Int_t split3_4_ierr = splitCluster(3, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split3_4_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split3_4_ierr << endm;
            }
         } //end cases 1-4

         //cases 5-6:  2 0     1
         //topology:   1   , 2 0
         else if ( ((*clusterIt)->getNRawHitsZ() == 2) && (rawHitPtr[0]->getColumn() == rawHitPtr[2]->getColumn() - 1) && (rawHitPtr[0]->getRow() == rawHitPtr[2]->getRow()) ) {
            //case 5
            if ( (rawHitPtr[1]->getColumn() == rawHitPtr[2]->getColumn()) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[1]) )    {
               tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 2;
               //exchange raw hit 1 and raw hit 2 position in the temperary raw hits array
               rawHitPtr[3] = rawHitPtr[2];
               rawHitPtr[2] = rawHitPtr[1];
               rawHitPtr[1] = rawHitPtr[3];
               rawHitPtr[3] = 0;

               Int_t split3_5_ierr = splitCluster(3, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split3_5_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split3_5_ierr << endm;
            }

            //case 6
            if ( (rawHitPtr[0]->getColumn() == rawHitPtr[1]->getColumn()) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[2]) )    {
               tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 1;
               //exchange raw hit 0 and raw hit 1 position in the temperary raw hits array
               rawHitPtr[3] = rawHitPtr[0];
               rawHitPtr[0] = rawHitPtr[1];
               rawHitPtr[1] = rawHitPtr[3];
               rawHitPtr[3] = 0;

               Int_t split3_6_ierr = splitCluster(3, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split3_6_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split3_6_ierr << endm;
            }
         }
      } //end splitting loop for clusters with size = 3

      //splitting clusters w/ size = 4
      if ( (*clusterIt)->getNRawHits() == 4 )  {
         tmpClusterSizeList[0] = 2; tmpClusterSizeList[3] = 3;
         Int_t index = 0;

         for (std::vector< StIstRawHit * >::iterator rawHitVecIt = (*clusterIt)->getRawHitVec().begin(); index < 4 && rawHitVecIt != (*clusterIt)->getRawHitVec().end(); rawHitVecIt++)      {
            rawHitPtr[index]        = *rawHitVecIt;

            if (mTimeBin < numTimeBins) {
               tmpRawHitMaxTb[index]   = mTimeBin;
            }
            else {
               tmpRawHitMaxTb[index]   = (*clusterIt)->getMaxTimeBin();//the raw hits belong to the same cluster should be at same time bin
            }

            tmpRawHitMaxCharge[index]   = rawHitPtr[index]->getCharge(tmpRawHitMaxTb[index]);
            index ++;
         }

         //                     3
         //cases 1-8:           2   3 2 1   3       3 2   3     3 2     3
         //topology:  3 2 1 0 , 1 ,     0 , 2 1 0 ,   1 , 2   ,   1 0 , 2 1
         //                     0                     0   1 0             0
         if ( ((*clusterIt)->getNRawHitsZ() == 4)      ||
               ((*clusterIt)->getNRawHitsRPhi() == 4)   ||
               (((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow() + 1) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()))     ||
               (((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow() + 1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()))     ||
               (((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn() + 1) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn()))     ||
               (((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn() + 1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn()) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()))     ||
               (((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow() + 1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()))     ||
               (((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[1]->getRow() == rawHitPtr[2]->getRow()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn() + 1) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()))  ) {
            //the 2nd raw hit with minimum ADC
            if ( (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[2]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[3]) )    {
               //0, 1, 2, 3
               if ( (*clusterIt)->getNRawHitsZ() == 4 )     {
                  tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 3; tmpClusterSizeList[5] = 1;
               }
               else if ( (*clusterIt)->getNRawHitsRPhi() == 4 )     {
                  tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 3;
               }
               else if ( ((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow() + 1) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) )      {
                  tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 3; tmpClusterSizeList[5] = 1;
               }
               else if ( ((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow() + 1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()) )      {
                  tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
               }
               else if ( ((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn() + 1) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn()) )       {
                  tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
               }
               else if ( ((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn() + 1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn()) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) )       {
                  tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 3;
               }
               else if ( ((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow() + 1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) )      {
                  tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
               }
               else        {
                  tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
               }

               Int_t split4_8_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split4_8_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_8_ierr << endm;
            }

            //the 3rd raw hit with minimum ADC
            if ( (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[3]) )    {
               StIstRawHit *rawHitPtrTmp = 0;
               //3, 2, 1, 0
               rawHitPtrTmp = rawHitPtr[0];
               rawHitPtr[0] = rawHitPtr[3];
               rawHitPtr[3] = rawHitPtrTmp;
               rawHitPtrTmp = rawHitPtr[2];
               rawHitPtr[2] = rawHitPtr[1];
               rawHitPtr[1] = rawHitPtrTmp;

               if ( (*clusterIt)->getNRawHitsZ() == 4 )
               { tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 3; tmpClusterSizeList[5] = 1; }
               else if ( (*clusterIt)->getNRawHitsRPhi() == 4 )
               { tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 3; }
               else if ( ((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow() + 1) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) )
               { tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2; }
               else if ( ((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow() + 1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()) )
               { tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 3; tmpClusterSizeList[5] = 1; }
               else if ( ((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn() + 1) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn()) )
               { tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 3; }
               else if ( ((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn() + 1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn()) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) )
               { tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2; }
               else if ( ((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow() + 1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) )
               { tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2; }
               else
               { tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2; }

               Int_t split4_8_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split4_8_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_8_ierr << endm;
            }
         } //end cases 1-8

         //case 9:   3 1 0
         //topology: 2
         else if ( (((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow() + 1) && (rawHitPtr[3]->getRow() == rawHitPtr[1]->getRow()) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow())) )   {
            //the 2nd raw hit with minimum ADC
            if ( (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[2]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[3]) )    {
               tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
               StIstRawHit *rawHitPtrTmp = 0;
               //0, 1, 3, 2
               rawHitPtrTmp = rawHitPtr[3];
               rawHitPtr[3] = rawHitPtr[2];
               rawHitPtr[2] = rawHitPtrTmp;

               Int_t split4_9_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split4_9_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_9_ierr << endm;
            }

            //the 4th raw hit with minimum ADC
            if ( (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[2]) )    {
               tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 3; tmpClusterSizeList[5] = 1;
               StIstRawHit *rawHitPtrTmp = 0;
               //2, 3, 1, 0
               rawHitPtrTmp = rawHitPtr[0];
               rawHitPtr[0] = rawHitPtr[2];
               rawHitPtr[2] = rawHitPtr[1];
               rawHitPtr[1] = rawHitPtr[3];
               rawHitPtr[3] = rawHitPtrTmp;

               Int_t split4_9_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split4_9_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_9_ierr << endm;
            }
         }//end case 9

         //case 10:      1
         //topology: 3 2 0
         else if ( (((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow() + 1) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) && (rawHitPtr[2]->getRow() == rawHitPtr[0]->getRow())) )   {
            //the 1st raw hit with minimum ADC
            if ( (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[2]) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[3]) )    {
               tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 3; tmpClusterSizeList[5] = 1;
               StIstRawHit *rawHitPtrTmp = 0;
               //1, 0, 2, 3
               rawHitPtrTmp = rawHitPtr[0];
               rawHitPtr[0] = rawHitPtr[1];
               rawHitPtr[1] = rawHitPtrTmp;

               Int_t split4_10_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split4_10_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_10_ierr << endm;
            }

            //the 3rd raw hit with minimum ADC
            if ( (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[3]) )    {
               tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
               StIstRawHit *rawHitPtrTmp = 0;
               //3, 2, 0, 1
               rawHitPtrTmp = rawHitPtr[0];
               rawHitPtr[0] = rawHitPtr[3];
               rawHitPtr[3] = rawHitPtr[1];
               rawHitPtr[1] = rawHitPtr[2];
               rawHitPtr[2] = rawHitPtrTmp;

               Int_t split4_10_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split4_10_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_10_ierr << endm;
            }
         }//end case 10

         //case 11:    2
         //topology:   1
         //          3 0
         else if ( (((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[0]->getColumn() + 1) && (rawHitPtr[3]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn()) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn())) )   {
            //the 1st raw hit with minimum ADC
            if ( (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[2]) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[3]) )    {
               tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 3;
               StIstRawHit *rawHitPtrTmp = 0;
               //3, 0, 1, 2
               rawHitPtrTmp = rawHitPtr[0];
               rawHitPtr[0] = rawHitPtr[3];
               rawHitPtr[3] = rawHitPtr[2];
               rawHitPtr[2] = rawHitPtr[1];
               rawHitPtr[1] = rawHitPtrTmp;

               Int_t split4_11_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split4_11_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_11_ierr << endm;
            }

            //the 2nd raw hit with minimum ADC
            if ( (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[2]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[3]) )    {
               tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
               StIstRawHit *rawHitPtrTmp = 0;
               //2, 1, 0, 3
               rawHitPtrTmp = rawHitPtr[0];
               rawHitPtr[0] = rawHitPtr[2];
               rawHitPtr[2] = rawHitPtrTmp;

               Int_t split4_11_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split4_11_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_11_ierr << endm;
            }
         }//end case 11

         //case 12:   3 0
         //topology:  2
         //           1
         else if ( (((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[3]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[3]->getColumn() == rawHitPtr[0]->getColumn() + 1) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn())) )   {
            //the 4th raw hit with minimum ADC
            if ( (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[2]) )    {
               tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 3;
               StIstRawHit *rawHitPtrTmp = 0;
               //0, 3, 2, 1
               rawHitPtrTmp = rawHitPtr[1];
               rawHitPtr[1] = rawHitPtr[3];
               rawHitPtr[3] = rawHitPtrTmp;

               Int_t split4_12_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split4_12_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_12_ierr << endm;
            }

            //the 3rd raw hit with minimum ADC
            if ( (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[3]) )    {
               tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
               StIstRawHit *rawHitPtrTmp = 0;
               //1, 2, 3, 0
               rawHitPtrTmp = rawHitPtr[0];
               rawHitPtr[0] = rawHitPtr[1];
               rawHitPtr[1] = rawHitPtr[2];
               rawHitPtr[2] = rawHitPtr[3];
               rawHitPtr[3] = rawHitPtrTmp;

               Int_t split4_12_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split4_12_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_12_ierr << endm;
            }
         }//end case 12

         //case 13:     2 0
         //topology:  3 1
         else if ( (((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow() + 1) && (rawHitPtr[3]->getRow() == rawHitPtr[1]->getRow()) && (rawHitPtr[2]->getRow() == rawHitPtr[0]->getRow())) )   {
            //the 3rd raw hit with minimum ADC
            if ( (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[3]) )    {
               tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
               StIstRawHit *rawHitPtrTmp = 0;
               //0, 2, 1, 3
               rawHitPtrTmp = rawHitPtr[1];
               rawHitPtr[1] = rawHitPtr[2];
               rawHitPtr[2] = rawHitPtrTmp;

               Int_t split4_13_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split4_13_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_13_ierr << endm;
            }

            //the 2nd raw hit with minimum ADC
            if ( (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[2]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[3]) )    {
               tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
               StIstRawHit *rawHitPtrTmp = 0;
               //3, 1, 2, 0
               rawHitPtrTmp = rawHitPtr[0];
               rawHitPtr[0] = rawHitPtr[3];
               rawHitPtr[3] = rawHitPtrTmp;

               Int_t split4_13_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split4_13_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_13_ierr << endm;
            }
         }//end case 13

         //case 14:     1
         //topology:  3 0
         //           2
         else if ( (((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[3]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[3]->getColumn() == rawHitPtr[0]->getColumn() + 1) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[0]->getColumn() == rawHitPtr[1]->getColumn())) )   {
            //the 1st raw hit with minimum ADC
            if ( (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[2]) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[3]) )    {
               tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
               StIstRawHit *rawHitPtrTmp = 0;
               //1, 0, 3, 2
               rawHitPtrTmp = rawHitPtr[0];
               rawHitPtr[0] = rawHitPtr[1];
               rawHitPtr[1] = rawHitPtrTmp;
               rawHitPtrTmp = rawHitPtr[2];
               rawHitPtr[2] = rawHitPtr[3];
               rawHitPtr[3] = rawHitPtrTmp;

               Int_t split4_14_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split4_14_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_14_ierr << endm;
            }

            //the 4th raw hit with minimum ADC
            if ( (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[2]) )    {
               tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
               StIstRawHit *rawHitPtrTmp = 0;
               //2, 3, 0, 1
               rawHitPtrTmp = rawHitPtr[0];
               rawHitPtr[0] = rawHitPtr[2];
               rawHitPtr[2] = rawHitPtrTmp;
               rawHitPtrTmp = rawHitPtr[1];
               rawHitPtr[1] = rawHitPtr[3];
               rawHitPtr[3] = rawHitPtrTmp;

               Int_t split4_14_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters, numTimeBins); // do splitting for the current cluster

               if (split4_14_ierr != kStOk)
                  LOG_WARN << "Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_14_ierr << endm;
            }
         }//end case 14
      }//end splitting loop for clusters with size = 4

      if ( (*clusterIt)->getNRawHits() > 4 || (*clusterIt)->getNRawHits() < 3 )
         continue;

   }//end clusters loop

   return kStOk;
};

Int_t StIstSimpleClusterAlgo::doClustering(const StIstCollection &istCollection, StIstRawHitCollection &rawHitsOriginal, StIstClusterCollection &clusters )
{
   unsigned char maxTb = UCHAR_MAX, usedTb = UCHAR_MAX;
   unsigned char ladder = 0, sensor = 0, column = 0, row = 0;
   float meanRow = 0., meanColumn = 0.;
   float totCharge = 0., totChargeErr = 0.;
   unsigned char clusterSize = 0, clusterSizeRPhi = 0, clusterSizeZ = 0;
   unsigned char clusterType = kIstSimpleClusterAlgo;

   //get number of time bin used in this event
   static unsigned char nTimeBins = istCollection.getNumTimeBins();
   //sort raw hits in increasing order by geometry ID
   rawHitsOriginal.sortByGeoId();

   //copy raw hit collection to a temporary vector
   std::vector<StIstRawHit *> rawHits;
   rawHits.reserve( rawHitsOriginal.getRawHitVec().size() );

   for ( std::vector< StIstRawHit * >::iterator rawHitIt = rawHitsOriginal.getRawHitVec().begin(); rawHitIt != rawHitsOriginal.getRawHitVec().end(); ++rawHitIt) 
   {
      rawHits.push_back( new StIstRawHit( *(*rawHitIt)) );
   }

   //do clustering
   if (rawHits.size() > 0)  {
      std::vector<StIstRawHit *>::iterator rawHitIt = rawHits.begin();
      //first raw hit
      maxTb       = (*rawHitIt)->getMaxTimeBin();

      if (maxTb >= nTimeBins)   maxTb = (*rawHitIt)->getDefaultTimeBin();

      if (mTimeBin < nTimeBins) {
         usedTb  = mTimeBin;
      }
      else
         usedTb  = maxTb;

      ladder      = (*rawHitIt)->getLadder();
      sensor      = (*rawHitIt)->getSensor();
      column      = (*rawHitIt)->getColumn();
      row         = (*rawHitIt)->getRow();
      totCharge   = (*rawHitIt)->getCharge(usedTb);
      totChargeErr = (*rawHitIt)->getChargeErr(usedTb);
      meanRow     = row;
      meanColumn  = column;
      clusterSize = 1;
      clusterSizeRPhi = 1;
      clusterSizeZ = 1;

      //first cluster (the 1st raw hit)
      StIstCluster *newCluster = new StIstCluster((int)ladder * 10000 + clusters.getClusterVec().size(), ladder, sensor, meanRow, meanColumn, totCharge, totChargeErr, clusterType);
      newCluster->setNRawHits(clusterSize);
      newCluster->setNRawHitsRPhi(clusterSizeRPhi);
      newCluster->setNRawHitsZ(clusterSizeZ);
      newCluster->setMaxTimeBin(maxTb);

      std::vector< StIstRawHit * > &rawHitVec = newCluster->getRawHitVec();
      rawHitVec.push_back(*rawHitIt);

      clusters.getClusterVec().push_back(newCluster); //store the first raw hit as the first cluster in the vector container

      //remove the clustered 1st raw hit from the raw hits list
      rawHits.erase( rawHitIt );

      if ( rawHits.size() != 0 ) { //more than one raw hits found
         float weight, tempSumCharge;
         std::vector< StIstCluster * >::iterator clusterIt = clusters.getClusterVec().begin();

         //loop the existed clusters vector constainer
         while ( clusterIt != clusters.getClusterVec().end() && !rawHits.empty() ) {
            //loop the raw hits belong to the ith cluster
            for (std::vector< StIstRawHit * >::iterator rawHitVecIt = (*clusterIt)->getRawHitVec().begin(); rawHitVecIt != (*clusterIt)->getRawHitVec().end(); rawHitVecIt++) {
               StIstRawHit *rawHitPtr = *rawHitVecIt;
               //loop rest raw hits in vector container
               rawHitIt = rawHits.begin();

               while ( rawHitIt != rawHits.end() ) {
                  // check raw hit[i] and raw hit of ith existed cluster in a same column/row over one sensor area
                  if ( (((*rawHitIt)->getSensor() == rawHitPtr->getSensor()) && ((*rawHitIt)->getRow() == rawHitPtr->getRow()) && (((*rawHitIt)->getColumn() == rawHitPtr->getColumn() + 1) || ((*rawHitIt)->getColumn() == rawHitPtr->getColumn() - 1) )) ||
                        (((*rawHitIt)->getSensor() == rawHitPtr->getSensor()) && ((*rawHitIt)->getColumn() == rawHitPtr->getColumn()) && (((*rawHitIt)->getRow() == rawHitPtr->getRow() + 1) || ((*rawHitIt)->getRow() == rawHitPtr->getRow() - 1) ))   ) {
                     clusterSize = (*clusterIt)->getNRawHits() + 1;

                     if ( ((*rawHitIt)->getRow() == rawHitPtr->getRow()) )
                        clusterSizeZ = (*clusterIt)->getNRawHitsZ() + 1; //same row

                     if ( ((*rawHitIt)->getColumn() == rawHitPtr->getColumn()) )
                        clusterSizeRPhi = (*clusterIt)->getNRawHitsRPhi() + 1; //same column

                     maxTb  = (*clusterIt)->getMaxTimeBin();

                     if (mTimeBin < nTimeBins) {
                        usedTb = mTimeBin;
                     }
                     else
                        usedTb = maxTb;

                     float currentRawHitCharge = (*rawHitIt)->getCharge(usedTb);
                     tempSumCharge = (*clusterIt)->getTotCharge() + currentRawHitCharge;
                     weight = currentRawHitCharge / tempSumCharge;

                     ladder        = (*clusterIt)->getLadder();
                     sensor        = (*clusterIt)->getSensor();
                     meanColumn    = (1.0 - weight) * (*clusterIt)->getMeanColumn() + weight * (*rawHitIt)->getColumn();
                     meanRow       = (1.0 - weight) * (*clusterIt)->getMeanRow()    + weight * (*rawHitIt)->getRow();
                     totCharge     = tempSumCharge;
                     totChargeErr  = sqrt( ( (*clusterIt)->getTotChargeErr() * (*clusterIt)->getTotChargeErr() + (*rawHitIt)->getChargeErr(usedTb) * (*rawHitIt)->getChargeErr(usedTb) ) / clusterSize );

                     (*clusterIt)->setLadder(ladder);
                     (*clusterIt)->setSensor(sensor);
                     (*clusterIt)->setMeanColumn(meanColumn);
                     (*clusterIt)->setMeanRow(meanRow);
                     (*clusterIt)->setTotCharge(totCharge);
                     (*clusterIt)->setTotChargeErr(totChargeErr);
                     (*clusterIt)->setNRawHits(clusterSize);
                     (*clusterIt)->setNRawHitsRPhi(clusterSizeRPhi);
                     (*clusterIt)->setNRawHitsZ(clusterSizeZ);
                     (*clusterIt)->setMaxTimeBin(maxTb);
                     (*clusterIt)->setClusteringType(clusterType);

                     //include the raw hit to the cluster's component vector
                     int itPosition = std::distance((*clusterIt)->getRawHitVec().begin(), rawHitVecIt);
                     (*clusterIt)->getRawHitVec().push_back(*rawHitIt);
                     rawHitVecIt = (*clusterIt)->getRawHitVec().begin() + itPosition;

                     //remove the clustered ith raw hit from the raw hits list
                     int distance = std::distance(rawHits.begin(), rawHitIt);
                     rawHits.erase( rawHitIt );
                     rawHitIt = rawHits.begin() + distance;
                  }//same row/column decision loop over
                  else
                     rawHitIt++;
               } //raw hits loop over
            }//ith cluster's raw hits loop over

            //if the rawHitIt_th raw hit does not belong to the existed ith clusters then create a new cluster.
            if (rawHits.size() < 1)
               continue;

            rawHitIt = rawHits.begin();

            maxTb       = (*rawHitIt)->getMaxTimeBin();

            if (maxTb >= nTimeBins)       maxTb = (*rawHitIt)->getDefaultTimeBin();

            if (mTimeBin < nTimeBins) {
               usedTb  = mTimeBin;
            }
            else
               usedTb = maxTb;

            ladder      = (*rawHitIt)->getLadder();
            sensor      = (*rawHitIt)->getSensor();
            column      = (*rawHitIt)->getColumn();
            row         = (*rawHitIt)->getRow();
            totCharge   = (*rawHitIt)->getCharge(usedTb);
            totChargeErr = (*rawHitIt)->getChargeErr(usedTb);
            meanRow     = row;
            meanColumn  = column;
            clusterSize = 1;
            clusterSizeRPhi = 1;
            clusterSizeZ = 1;

            StIstCluster *newCluster1 = new StIstCluster((int)ladder * 10000 + clusters.getClusterVec().size(), ladder, sensor, meanRow, meanColumn, totCharge, totChargeErr, clusterType);
            newCluster1->setNRawHits(clusterSize);
            newCluster1->setNRawHitsRPhi(clusterSizeRPhi);
            newCluster1->setNRawHitsZ(clusterSizeZ);
            newCluster1->setMaxTimeBin(maxTb);

            int distanceCluster = std::distance(clusters.getClusterVec().begin(), clusterIt);

            std::vector< StIstRawHit * > &rawHitVec1 = newCluster1->getRawHitVec();
            rawHitVec1.push_back(*rawHitIt);

            clusters.getClusterVec().push_back(newCluster1);

            clusterIt = clusters.getClusterVec().begin() + distanceCluster;
            clusterIt++;
            //remove the clustered 1st raw hit from the raw hits list
            rawHits.erase( rawHitIt );
         }//clusters loop over
      }//end raw hits number > 1 cut
   }//end raw hits number > 0 cut

   ////////////////////////////////  do cluster splitting  /////////////////////////////////////////
   if (mSplitCluster)   {
      Int_t loc_ierr = doSplitting(clusters, nTimeBins);

      if (loc_ierr != kStOk) {
         LOG_WARN << "StIstClusterMaker::Make(): Cluster splitting for ladder " << ladder << " returned " << loc_ierr << endm;
      }
   }

   return kStOk;
};


/***************************************************************************
*
* $Log: StIstSimpleClusterAlgo.cxx,v $
* Revision 1.18  2018/01/04 17:34:37  smirnovd
* [Cosmetic] Remove StRoot/ from include path
*
* $STAR/StRoot is already in the default path search
*
* Revision 1.17  2015/05/20 20:53:57  smirnovd
* Removed a priori true condition without changing the logic
*
* mTimeBin is unsigned char always >= 0
*
* Revision 1.16  2015/05/20 20:53:53  smirnovd
* Set default value of unsigned variables in a more explicit way
*
* Revision 1.15  2015/05/20 20:08:06  huangbc
* change unsigned char maxTb = -1 to char maxTb = -1.
*
* Revision 1.14  2015/05/19 16:19:47  perev
* bug #3102 Clearup
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
* Revision 1.10  2014/09/07 13:54:45  ypwang
* move setUsedTimeBin() and setSplitFlag() setters from inherited classes to their base class StIstIClusterAlgo.h
*
* Revision 1.9  2014/09/07 11:41:36  ypwang
* ClassDef version updated from 1 to 0, and remove Init() function
*
* Revision 1.8  2014/08/22 15:55:15  smirnovd
* Fixed style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.7  2014/08/22 15:50:00  smirnovd
* Moved CVS history to the end of file
*
* Revision 1.6  2014/02/16 21:42:54  ypwang
* getting number of time bins used in current event by StIstCollection::getNumTimeBins() function
*
* Revision 1.5  2014/02/15 20:12:58  ypwang
* update due to replace raw hit container type from std::map to std::vector, and minor bug corrected for the simple clustering algorithm
*
* Revision 1.4  2014/02/10 16:33:44  smirnovd
* Trimmed trailing spaces, expanded tabs to eight spaces
*
* Revision 1.3  2014/02/08 03:34:16  ypwang
* updating scripts
*
*
****************************************************************************
* StIstSimpleClusterAlgo.cxx,v 1.0
* Revision 1.0 2013/11/04 15:55:30 Yaping
* Initial version
****************************************************************************/
