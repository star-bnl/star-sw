/***************************************************************************
*
* $Id: StIstSimpleClusterAlgo.cxx,v 1.3 2014/02/08 03:34:16 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* See header file.
****************************************************************************
*
* $Log: StIstSimpleClusterAlgo.cxx,v $
* Revision 1.3  2014/02/08 03:34:16  ypwang
* updating scripts
*
*
****************************************************************************
* StIstSimpleClusterAlgo.cxx,v 1.0
* Revision 1.0 2013/11/04 15:55:30 Yaping
* Initial version
****************************************************************************/

#include "StEvent.h"
#include "StMessMgr.h"
#include "StIstSimpleClusterAlgo.h"
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

StIstSimpleClusterAlgo::StIstSimpleClusterAlgo(): mSplitCluster(1), mTimeBin(-1)
{
  //nothing else to do....
};

Int_t StIstSimpleClusterAlgo::Init()
{
  return kStOk;
};

Int_t StIstSimpleClusterAlgo::splitCluster(int cSize, int clusterSizeList[], StIstRawHit *rawHitPtr[], StIstCluster *clusterIt, StIstClusterCollection& clusters)
{
    StIstRawHit *rawHitPtr0 = 0;
    StIstRawHit *rawHitPtr1 = 0;
    StIstRawHit *rawHitPtr2 = 0;
    StIstRawHit *rawHitPtr3 = 0;
    
    StIstCluster* newClusterTmp = 0;
    
    unsigned char maxTb=-1;
    unsigned char ladder=0, sensor=0;
    float meanRow=0, meanColumn=0;
    float totCharge=0., totChargeErr=0.;
    unsigned char clusterSize=0;
    unsigned char clusterSizeRPhi=0;
    unsigned char clusterSizeZ=0;
    unsigned char clusterType=kIstSimpleClusterAlgo;    

    float weightSplit = 0., weightSplit2 = 0., tmpSumChargeSplit = 0.;
    
    //split cluster w/ size = 3
    if(cSize == 3) {
        rawHitPtr0 = rawHitPtr[0];
        rawHitPtr1 = rawHitPtr[1];
        rawHitPtr2 = rawHitPtr[2];

        unsigned char tmpRawHitMaxTb0, tmpRawHitMaxTb1, tmpRawHitMaxTb2;
        if(mTimeBin>=0 && mTimeBin < kIstNumTimeBins) {
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
        
        ladder	  = rawHitPtr0->getLadder();
        sensor    = rawHitPtr0->getSensor();
        maxTb     = tmpRawHitMaxTb0;
	meanColumn= weightSplit * rawHitPtr0->getColumn() + (1.0 - weightSplit) * rawHitPtr1->getColumn();
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
        rawHitMap_t &rawHitMapTmp = clusterIt->getRawHitMap();
        rawHitMap_t::iterator rawHitIterator = rawHitMapTmp.end();
        rawHitIterator--;
        rawHitMapTmp.erase(rawHitIterator); //remove the rawHit[2]
        
        //cluster spliting: part of raw hit 1 + raw hit 2
        tmpSumChargeSplit = tmpRawHitMaxCharge2 + tmpRawHitMaxCharge1 * tmpRawHitMaxCharge2 / (tmpRawHitMaxCharge0 + tmpRawHitMaxCharge2);
        weightSplit = tmpRawHitMaxCharge2 / tmpSumChargeSplit;
        
        clusterSize     = clusterSizeList[3];
        clusterSizeZ    = clusterSizeList[4];
        clusterSizeRPhi = clusterSizeList[5];
        
        ladder    = rawHitPtr2->getLadder();
        sensor    = rawHitPtr2->getSensor();
        maxTb     = tmpRawHitMaxTb2;
	meanColumn= weightSplit * rawHitPtr2->getColumn() + (1.0 - weightSplit) * rawHitPtr1->getColumn();
	meanRow   = weightSplit * rawHitPtr2->getRow()    + (1.0 - weightSplit) * rawHitPtr1->getRow();
        totCharge    = tmpSumChargeSplit;
        totChargeErr = sqrt( (rawHitPtr2->getChargeErr(tmpRawHitMaxTb2) * rawHitPtr2->getChargeErr(tmpRawHitMaxTb2) + rawHitPtr1->getChargeErr(tmpRawHitMaxTb1) * rawHitPtr1->getChargeErr(tmpRawHitMaxTb1)) / clusterSize );
        
        newClusterTmp = new StIstCluster((int)ladder*10000+clusters.getClusterVec().size(), ladder, sensor, meanRow, meanColumn, totCharge, totChargeErr, clusterType);
        newClusterTmp->setNRawHits(clusterSize);
        newClusterTmp->setNRawHitsRPhi(clusterSizeRPhi);
        newClusterTmp->setNRawHitsZ(clusterSizeZ);
        newClusterTmp->setMaxTimeBin(maxTb);
        
        rawHitMap_t &rawHitMapTmp2 = newClusterTmp->getRawHitMap();
        rawHitMapTmp2[rawHitPtr1] = 1;
        rawHitMapTmp2[rawHitPtr2] = 1;
        clusters.getClusterVec().push_back(newClusterTmp);
    }
    
    // split cluster w/ size = 4
    if(cSize == 4)
    {
        rawHitPtr0 = rawHitPtr[0];
        rawHitPtr1 = rawHitPtr[1];
        rawHitPtr2 = rawHitPtr[2];
        rawHitPtr3 = rawHitPtr[3];

        unsigned char tmpRawHitMaxTb0, tmpRawHitMaxTb1, tmpRawHitMaxTb2, tmpRawHitMaxTb3;
        if(mTimeBin>=0 && mTimeBin < kIstNumTimeBins) {
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
        
        ladder	  = rawHitPtr0->getLadder();
        sensor    = rawHitPtr0->getSensor();
        maxTb     = tmpRawHitMaxTb0;
	meanColumn= weightSplit * rawHitPtr0->getColumn() + (1.0 - weightSplit) * rawHitPtr1->getColumn();
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
        rawHitMap_t &rawHitMapTmp3 = clusterIt->getRawHitMap();
        rawHitMapTmp3.clear();
        rawHitMapTmp3[rawHitPtr0] = 1;
        rawHitMapTmp3[rawHitPtr1] = 1;
         
        //cluster spliting: part of raw hit 1 + raw hit 2 + raw hit 3
        tmpSumChargeSplit = tmpRawHitMaxCharge2 + tmpRawHitMaxCharge3 + tmpRawHitMaxCharge1 * (tmpRawHitMaxCharge2 + tmpRawHitMaxCharge3) / (tmpRawHitMaxCharge0 + tmpRawHitMaxCharge2 + tmpRawHitMaxCharge3);
        weightSplit = tmpRawHitMaxCharge2 / tmpSumChargeSplit;
        weightSplit2= tmpRawHitMaxCharge3 / tmpSumChargeSplit;
         
        clusterSize     = clusterSizeList[3];
        clusterSizeZ    = clusterSizeList[4];
        clusterSizeRPhi = clusterSizeList[5];
         
        ladder    = rawHitPtr2->getLadder();
        sensor    = rawHitPtr2->getSensor();
        maxTb     = tmpRawHitMaxTb2;
	meanColumn= weightSplit * rawHitPtr2->getColumn() + weightSplit2 * rawHitPtr3->getColumn() + (1.0 - weightSplit - weightSplit2) * rawHitPtr1->getColumn();
	meanRow   = weightSplit * rawHitPtr2->getRow()    + weightSplit2 * rawHitPtr3->getRow()    + (1.0 - weightSplit - weightSplit2) * rawHitPtr1->getRow();
        totCharge    = tmpSumChargeSplit;
        totChargeErr  = sqrt( (rawHitPtr3->getChargeErr(tmpRawHitMaxTb3) * rawHitPtr3->getChargeErr(tmpRawHitMaxTb3) + rawHitPtr2->getChargeErr(tmpRawHitMaxTb2) * rawHitPtr2->getChargeErr(tmpRawHitMaxTb2) + rawHitPtr1->getChargeErr(tmpRawHitMaxTb1) * rawHitPtr1->getChargeErr(tmpRawHitMaxTb1)) / clusterSize );
         
        newClusterTmp = new StIstCluster((int)ladder*10000+clusters.getClusterVec().size(), ladder, sensor, meanRow, meanColumn, totCharge, totChargeErr, clusterType);
        newClusterTmp->setNRawHits(clusterSize);
        newClusterTmp->setNRawHitsRPhi(clusterSizeRPhi);
        newClusterTmp->setNRawHitsZ(clusterSizeZ);
        newClusterTmp->setMaxTimeBin(maxTb);
         
        rawHitMap_t &rawHitMapTmp4 = newClusterTmp->getRawHitMap();
        rawHitMapTmp4[rawHitPtr1] = 1;
        rawHitMapTmp4[rawHitPtr2] = 1;
        rawHitMapTmp4[rawHitPtr3] = 1;
        clusters.getClusterVec().push_back(newClusterTmp);
    }
     
    return kStOk;
};

Int_t StIstSimpleClusterAlgo::doSplitting(StIstClusterCollection& clusters )
{
    //loop found clusters in current event
    for(std::vector< StIstCluster* >::iterator clusterIt=clusters.getClusterVec().begin(); clusterIt!=clusters.getClusterVec().end(); clusterIt++)	{
        StIstRawHit *rawHitPtr[]    	=   {0, 0, 0, 0};
        unsigned char tmpRawHitMaxTb[]  =   {0, 0, 0, 0};
        float tmpRawHitMaxCharge[]	=   {0., 0., 0., 0.};
        int tmpClusterSizeList[]  	=   {1, 1, 1, 1, 1, 1};//{clusterSize_1, clusterSizeZ_1, clusterSizeRPhi_1, clusterSize_2, clusterSizeZ_2, clusterSizeRPhi_2}
        
        //splitting clusters w/ size = 3
        if( (*clusterIt)->getNRawHits() == 3 )	{
	    tmpClusterSizeList[0] = 2; tmpClusterSizeList[3] = 2;
            int index = 0;
            
            for(rawHitMap_t::iterator rawHitMapIt=(*clusterIt)->getRawHitMap().begin(); index<3 && rawHitMapIt!=(*clusterIt)->getRawHitMap().end(); rawHitMapIt++)	{
                rawHitPtr[index]        = rawHitMapIt->first;
		if(mTimeBin>=0 && mTimeBin < kIstNumTimeBins) {
		    tmpRawHitMaxTb[index]   = mTimeBin;
		}
		else {
                    tmpRawHitMaxTb[index]   = (*clusterIt)->getMaxTimeBin();//the raw hits belong to the same cluster should be at same time bin
		}
                tmpRawHitMaxCharge[index]	= rawHitPtr[index]->getCharge(tmpRawHitMaxTb[index]);
                index ++;
            }
            
            //cases 1-4:        2   2 1   2
	    //topology: 2 1 0 , 1 ,   0 , 1 0
	    //			0   
            if( ((*clusterIt)->getNRawHitsZ() == 3) || ((*clusterIt)->getNRawHitsRPhi() == 3) ||
               (((*clusterIt)->getNRawHitsZ() == 2) && ((*clusterIt)->getNRawHitsRPhi() == 2) && (rawHitPtr[0]->getColumn() == rawHitPtr[2]->getColumn()-1) && (rawHitPtr[0]->getRow() == rawHitPtr[2]->getRow()-1)) )
            {
                if( (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[2]) )	{
                    if( (*clusterIt)->getNRawHitsZ() == 3 )  {
                        tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 1; }
                    else if( (*clusterIt)->getNRawHitsRPhi() == 3 )	{
			tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 2; }
                    else if( ((*clusterIt)->getNRawHitsZ() == 2) && (rawHitPtr[0]->getColumn() == rawHitPtr[1]->getColumn()) )	{
                        tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 1; }
               	    else	{
                    	tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 2; }

                    Int_t split3_4_ierr = splitCluster(3, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster

                    if(split3_4_ierr != kStOk)
                        LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split3_4_ierr <<endm;
                }
            } //end cases 1-4

	    //cases 5-6:  2 0     1
            //topology:   1   , 2 0
            else if( ((*clusterIt)->getNRawHitsZ() == 2) && (rawHitPtr[0]->getColumn() == rawHitPtr[2]->getColumn()-1) && (rawHitPtr[0]->getRow() == rawHitPtr[2]->getRow()) )
            {
                //case 5
                if( (rawHitPtr[1]->getColumn() == rawHitPtr[2]->getColumn()) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[1]) )    {
		    tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 2;
                    //exchange raw hit 1 and raw hit 2 position in the temperary raw hits array
                    rawHitPtr[3] = rawHitPtr[2];
                    rawHitPtr[2] = rawHitPtr[1];
                    rawHitPtr[1] = rawHitPtr[3];
                    rawHitPtr[3] = 0;
                    
                    Int_t split3_5_ierr = splitCluster(3, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split3_5_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split3_5_ierr <<endm;
                    }
                //case 6
                if( (rawHitPtr[0]->getColumn() == rawHitPtr[1]->getColumn()) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[2]) )    {
		    tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 1;
                    //exchange raw hit 0 and raw hit 1 position in the temperary raw hits array
                    rawHitPtr[3] = rawHitPtr[0];
                    rawHitPtr[0] = rawHitPtr[1];
                    rawHitPtr[1] = rawHitPtr[3];
                    rawHitPtr[3] = 0;
                    
                    Int_t split3_6_ierr = splitCluster(3, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split3_6_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split3_6_ierr <<endm;
                }
            }
        } //end splitting loop for clusters with size = 3
        
        //splitting clusters w/ size = 4
        if( (*clusterIt)->getNRawHits() == 4 )	{
            tmpClusterSizeList[0] = 2; tmpClusterSizeList[3] = 3;
            Int_t index = 0;
         
            for(rawHitMap_t::iterator rawHitMapIt=(*clusterIt)->getRawHitMap().begin(); index<4 && rawHitMapIt!=(*clusterIt)->getRawHitMap().end(); rawHitMapIt++)	{
                rawHitPtr[index]        = rawHitMapIt->first;
		if(mTimeBin>=0 && mTimeBin < kIstNumTimeBins) {
                    tmpRawHitMaxTb[index]   = mTimeBin;
                }
		else {
		    tmpRawHitMaxTb[index]   = (*clusterIt)->getMaxTimeBin();//the raw hits belong to the same cluster should be at same time bin
		}
                tmpRawHitMaxCharge[index]   = rawHitPtr[index]->getCharge(tmpRawHitMaxTb[index]);
                index ++;
            }
	    //                     3 
            //cases 1-8: 	   2   3 2 1   3       3 2   3     3 2     3
	    //topology:  3 2 1 0 , 1 ,     0 , 2 1 0 ,   1 , 2   ,   1 0 , 2 1
	    //			   0			 0   1 0	     0
            if( ((*clusterIt)->getNRawHitsZ() == 4)      ||
                ((*clusterIt)->getNRawHitsRPhi() == 4)   ||
               (((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()+1) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()))     ||
               (((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()+1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()))     ||
               (((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()+1) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn()))     ||
               (((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()+1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn()) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()))     ||
               (((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()+1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()))     ||
               (((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[1]->getRow() == rawHitPtr[2]->getRow()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn()+1) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()))  )
            {
                //the 2nd raw hit with minimum ADC
                if( (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[2]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[3]) )    {
                    //0, 1, 2, 3
                    if( (*clusterIt)->getNRawHitsZ() == 4 )	{
                        tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 3; tmpClusterSizeList[5] = 1; }
                    else if( (*clusterIt)->getNRawHitsRPhi() == 4 )	{
                        tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 3; }
                    else if( ((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()+1) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) )	{
                        tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 3; tmpClusterSizeList[5] = 1; }
                    else if( ((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()+1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()) )	{
                        tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2; }
                    else if( ((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()+1) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn()) )	{
                        tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2; }
                    else if( ((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()+1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn()) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) )	{
                        tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 3; }
                    else if( ((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()+1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) )	{
                        tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2; }
                    else	{
                        tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;}
                    
                    Int_t split4_8_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split4_8_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_8_ierr <<endm;
                }
                //the 3rd raw hit with minimum ADC
                if( (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[3]) )    {
                    StIstRawHit *rawHitPtrTmp = 0;
                    //3, 2, 1, 0
                    rawHitPtrTmp = rawHitPtr[0];
                    rawHitPtr[0] = rawHitPtr[3];
                    rawHitPtr[3] = rawHitPtrTmp;
                    rawHitPtrTmp = rawHitPtr[2];
                    rawHitPtr[2] = rawHitPtr[1];
                    rawHitPtr[1] = rawHitPtrTmp;

                    if( (*clusterIt)->getNRawHitsZ() == 4 )
                        { tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 3; tmpClusterSizeList[5] = 1; }
                    else if( (*clusterIt)->getNRawHitsRPhi() == 4 )
			{ tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 3; }
                    else if( ((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()+1) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) )
                        { tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2; }
                    else if( ((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()+1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()) )
                        { tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 3; tmpClusterSizeList[5] = 1; }
                    else if( ((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()+1) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn()) )
                        { tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 3; }
                    else if( ((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()+1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn()) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) )
                        { tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2; }
                    else if( ((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()+1) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) )
                        { tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2; }
                    else
                        { tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2; }

                    Int_t split4_8_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split4_8_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_8_ierr <<endm;
                }
            } //end cases 1-8
            
	    //case 9:   3 1 0
	    //topology: 2    
            else if( (((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()+1) && (rawHitPtr[3]->getRow() == rawHitPtr[1]->getRow()) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow())) )   {
                //the 2nd raw hit with minimum ADC
                if( (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[2]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[3]) )    {
                    tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2; 
                    StIstRawHit *rawHitPtrTmp = 0;
                    //0, 1, 3, 2
                    rawHitPtrTmp = rawHitPtr[3];
                    rawHitPtr[3] = rawHitPtr[2];
                    rawHitPtr[2] = rawHitPtrTmp;
                    
                    Int_t split4_9_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split4_9_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_9_ierr <<endm;
                }
                //the 4th raw hit with minimum ADC
                if( (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[2]) )    {
                    tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 3; tmpClusterSizeList[5] = 1;
                    StIstRawHit *rawHitPtrTmp = 0;
                    //2, 3, 1, 0
                    rawHitPtrTmp = rawHitPtr[0];
                    rawHitPtr[0] = rawHitPtr[2];
                    rawHitPtr[2] = rawHitPtr[1];
                    rawHitPtr[1] = rawHitPtr[3];
                    rawHitPtr[3] = rawHitPtrTmp;
                    
                    Int_t split4_9_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split4_9_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_9_ierr <<endm;
                }
            }//end case 9

            //case 10:      1
	    //topology: 3 2 0
            else if( (((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn()) && (rawHitPtr[1]->getRow() == rawHitPtr[0]->getRow()+1) && (rawHitPtr[3]->getRow() == rawHitPtr[2]->getRow()) && (rawHitPtr[2]->getRow() == rawHitPtr[0]->getRow())) )   {
                //the 1st raw hit with minimum ADC
                if( (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[2]) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[3]) )    {
                    tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 3; tmpClusterSizeList[5] = 1; 
                    StIstRawHit *rawHitPtrTmp = 0;
                    //1, 0, 2, 3
                    rawHitPtrTmp = rawHitPtr[0];
                    rawHitPtr[0] = rawHitPtr[1];
                    rawHitPtr[1] = rawHitPtrTmp;
                                        
                    Int_t split4_10_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split4_10_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_10_ierr <<endm;
                }
                //the 3rd raw hit with minimum ADC
                if( (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[3]) )    {
                    tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
                    StIstRawHit *rawHitPtrTmp = 0;
                    //3, 2, 0, 1
                    rawHitPtrTmp = rawHitPtr[0];
                    rawHitPtr[0] = rawHitPtr[3];
                    rawHitPtr[3] = rawHitPtr[1];
                    rawHitPtr[1] = rawHitPtr[2];
                    rawHitPtr[2] = rawHitPtrTmp;
                    
                    Int_t split4_10_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split4_10_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_10_ierr <<endm;
                }
            }//end case 10
            
	    //case 11:    2
	    //topology:   1
	    //		3 0
            else if( (((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[3]->getColumn() == rawHitPtr[0]->getColumn()+1) && (rawHitPtr[3]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn()) && (rawHitPtr[1]->getColumn() == rawHitPtr[0]->getColumn())) )   {
                //the 1st raw hit with minimum ADC
                if( (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[2]) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[3]) )    {
                    tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 3;
                    StIstRawHit *rawHitPtrTmp = 0;
                    //3, 0, 1, 2
                    rawHitPtrTmp = rawHitPtr[0];
                    rawHitPtr[0] = rawHitPtr[3];
                    rawHitPtr[3] = rawHitPtr[2];
                    rawHitPtr[2] = rawHitPtr[1];
                    rawHitPtr[1] = rawHitPtrTmp;
                    
                    Int_t split4_11_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split4_11_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_11_ierr <<endm;
                }
                //the 2nd raw hit with minimum ADC
                if( (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[2]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[3]) )    {
                    tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
                    StIstRawHit *rawHitPtrTmp = 0;
                    //2, 1, 0, 3
                    rawHitPtrTmp = rawHitPtr[0];
                    rawHitPtr[0] = rawHitPtr[2];
                    rawHitPtr[2] = rawHitPtrTmp;
                    
                    Int_t split4_11_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split4_11_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_11_ierr <<endm;
                }
            }//end case 11
            
	    //case 12:   3 0
	    //topology:  2
	    //		 1
            else if( (((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[3]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[3]->getColumn() == rawHitPtr[0]->getColumn()+1) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[2]->getColumn() == rawHitPtr[1]->getColumn())) )   {
                //the 4th raw hit with minimum ADC
                if( (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[2]) )    {
                    tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 1; tmpClusterSizeList[5] = 3;
                    StIstRawHit *rawHitPtrTmp = 0;
                    //0, 3, 2, 1
                    rawHitPtrTmp = rawHitPtr[1];
                    rawHitPtr[1] = rawHitPtr[3];
                    rawHitPtr[3] = rawHitPtrTmp;
                    
                    Int_t split4_12_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split4_12_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_12_ierr <<endm;
                }
                //the 3rd raw hit with minimum ADC
                if( (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[3]) )    {
                    tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
                    StIstRawHit *rawHitPtrTmp = 0;
                    //1, 2, 3, 0
                    rawHitPtrTmp = rawHitPtr[0];
                    rawHitPtr[0] = rawHitPtr[1];
                    rawHitPtr[1] = rawHitPtr[2];
                    rawHitPtr[2] = rawHitPtr[3];
                    rawHitPtr[3] = rawHitPtrTmp;
                    
                    Int_t split4_12_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split4_12_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_12_ierr <<endm;
                }
            }//end case 12
           
	    //case 13:     2 0
	    //topology:  3 1 
            else if( (((*clusterIt)->getNRawHitsZ() == 3) && (rawHitPtr[1]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[2]->getRow() == rawHitPtr[1]->getRow()+1) && (rawHitPtr[3]->getRow() == rawHitPtr[1]->getRow()) && (rawHitPtr[2]->getRow() == rawHitPtr[0]->getRow())) )   {
                //the 3rd raw hit with minimum ADC
                if( (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[2] <= tmpRawHitMaxCharge[3]) )    {
                    tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
                    StIstRawHit *rawHitPtrTmp = 0;
                    //0, 2, 1, 3
                    rawHitPtrTmp = rawHitPtr[1];
                    rawHitPtr[1] = rawHitPtr[2];
                    rawHitPtr[2] = rawHitPtrTmp;
                    
                    Int_t split4_13_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split4_13_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_13_ierr <<endm;
                }
                //the 2nd raw hit with minimum ADC
                if( (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[2]) && (tmpRawHitMaxCharge[1] <= tmpRawHitMaxCharge[3]) )    {
                    tmpClusterSizeList[1] = 2; tmpClusterSizeList[2] = 1; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
                    StIstRawHit *rawHitPtrTmp = 0;
                    //3, 1, 2, 0
                    rawHitPtrTmp = rawHitPtr[0];
                    rawHitPtr[0] = rawHitPtr[3];
                    rawHitPtr[3] = rawHitPtrTmp;
                    
                    Int_t split4_13_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split4_13_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_13_ierr <<endm;
                }
            }//end case 13
            
	    //case 14:	   1
	    //topology:  3 0
	    //		 2
            else if( (((*clusterIt)->getNRawHitsRPhi() == 3) && (rawHitPtr[3]->getRow() == rawHitPtr[0]->getRow()) && (rawHitPtr[3]->getColumn() == rawHitPtr[0]->getColumn()+1) && (rawHitPtr[3]->getColumn() == rawHitPtr[2]->getColumn()) && (rawHitPtr[0]->getColumn() == rawHitPtr[1]->getColumn())) )   {
                //the 1st raw hit with minimum ADC
                if( (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[2]) && (tmpRawHitMaxCharge[0] <= tmpRawHitMaxCharge[3]) )    {
                    tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
                    StIstRawHit *rawHitPtrTmp = 0;
                    //1, 0, 3, 2
                    rawHitPtrTmp = rawHitPtr[0];
                    rawHitPtr[0] = rawHitPtr[1];
                    rawHitPtr[1] = rawHitPtrTmp;
                    rawHitPtrTmp = rawHitPtr[2];
                    rawHitPtr[2] = rawHitPtr[3];
                    rawHitPtr[3] = rawHitPtrTmp;
                    
                    Int_t split4_14_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split4_14_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_14_ierr <<endm;
                }
                //the 4th raw hit with minimum ADC
                if( (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[0]) && (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[1]) && (tmpRawHitMaxCharge[3] <= tmpRawHitMaxCharge[2]) )    {
                    tmpClusterSizeList[1] = 1; tmpClusterSizeList[2] = 2; tmpClusterSizeList[4] = 2; tmpClusterSizeList[5] = 2;
                    StIstRawHit *rawHitPtrTmp = 0;
                    //2, 3, 0, 1
                    rawHitPtrTmp = rawHitPtr[0];
                    rawHitPtr[0] = rawHitPtr[2];
                    rawHitPtr[2] = rawHitPtrTmp;
                    rawHitPtrTmp = rawHitPtr[1];
                    rawHitPtr[1] = rawHitPtr[3];
                    rawHitPtr[3] = rawHitPtrTmp;
                    
                    Int_t split4_14_ierr = splitCluster(4, tmpClusterSizeList, rawHitPtr, *clusterIt, clusters); // do splitting for the current cluster
                    if(split4_14_ierr != kStOk)
			LOG_WARN <<"Cluster (ladder/sensor/meanColumn/meanRow: " << (*clusterIt)->getLadder() << "/" << (*clusterIt)->getSensor() << "/" << (*clusterIt)->getMeanColumn() << "/" << (*clusterIt)->getMeanRow() << ") splitting failed! returned " << split4_14_ierr <<endm;
                }
            }//end case 14
        }//end splitting loop for clusters with size = 4
         
        if( (*clusterIt)->getNRawHits() > 4 || (*clusterIt)->getNRawHits() < 3 )
            continue;
        
    }//end clusters loop
    
    return kStOk;
};

Int_t StIstSimpleClusterAlgo::doClustering(const StIstCollection& istCollection, StIstRawHitCollection& rawHitsOriginal, StIstClusterCollection& clusters )
{
    StIstCluster* newCluster = 0;
      
    unsigned char maxTb=-1, usedTb=-1;
    unsigned char ladder=0, sensor=0, column=0, row=0;
    float meanRow=0., meanColumn=0.;
    float totCharge=0., totChargeErr=0.;
    unsigned char clusterSize=0, clusterSizeRPhi=0, clusterSizeZ=0;
    unsigned char clusterType=kIstSimpleClusterAlgo;

    //sort raw hits in increasing order by geometry ID
    rawHitsOriginal.sortByGeoId();

    //copy raw hit collection to a temporary vector
    std::vector<StIstRawHit*> rawHits;
    rawHits.reserve( rawHitsOriginal.getRawHitVec().size() );

    for( std::vector< StIstRawHit* >::iterator rawHitIt=rawHitsOriginal.getRawHitVec().begin();rawHitIt!=rawHitsOriginal.getRawHitVec().end();++rawHitIt)
    {
        int channelId = (*rawHitIt)->getChannelId();
        if( channelId < 0 && channelId >= kIstNumElecIds) {
             rawHitsOriginal.getRawHitVec().erase( rawHitIt );
	     continue;
	}

        rawHits.push_back( new StIstRawHit( *(*rawHitIt)) );
    }
 
    //do clustering 
    if(rawHits.size() > 0)  {
        std::vector<StIstRawHit*>::iterator rawHitIt = rawHits.begin();
        
        //first raw hit
        maxTb       = (*rawHitIt)->getMaxTimeBin();
        if(maxTb<0 || maxTb>=kIstNumTimeBins) 	maxTb = (*rawHitIt)->getDefaultTimeBin();

        if(mTimeBin>=0 && mTimeBin < kIstNumTimeBins) {
            usedTb  = mTimeBin;
        }
	else
	    usedTb  = maxTb;

        ladder      = (*rawHitIt)->getLadder();
        sensor      = (*rawHitIt)->getSensor();
        column      = (*rawHitIt)->getColumn();
        row         = (*rawHitIt)->getRow();
        totCharge   = (*rawHitIt)->getCharge(usedTb);
        totChargeErr= (*rawHitIt)->getChargeErr(usedTb);
        meanRow     = row;
        meanColumn  = column;
        clusterSize = 1;
        clusterSizeRPhi = 1;
        clusterSizeZ = 1;
         
        //first cluster (the 1st raw hit)
        newCluster = new StIstCluster((int)ladder*10000+clusters.getClusterVec().size(), ladder, sensor, meanRow, meanColumn, totCharge, totChargeErr, clusterType);
        newCluster->setNRawHits(clusterSize);
        newCluster->setNRawHitsRPhi(clusterSizeRPhi);
        newCluster->setNRawHitsZ(clusterSizeZ);
        newCluster->setMaxTimeBin(maxTb);

        rawHitMap_t &rawHitMap = newCluster->getRawHitMap();
        rawHitMap[ *rawHitIt ] = 1;
        clusters.getClusterVec().push_back(newCluster); //store the first raw hit as the first cluster in the vector container

        //remove the clustered 1st raw hit from the raw hits list
        rawHits.erase( rawHitIt );

	if( rawHits.size() != 0 ) //more than one raw hits found
	{ 
           float weight, tempSumCharge;
	   std::vector< StIstCluster* >::iterator clusterIt = clusters.getClusterVec().begin();
           //loop the existed clusters vector constainer
	   while( clusterIt != clusters.getClusterVec().end() && !rawHits.empty() )
           {
               //loop the raw hits belong to the ith cluster
               for(rawHitMap_t::iterator rawHitMapIt=(*clusterIt)->getRawHitMap().begin(); rawHitMapIt!=(*clusterIt)->getRawHitMap().end(); rawHitMapIt++)
               {
                   StIstRawHit *rawHitPtr = rawHitMapIt->first;
                   //loop raw hits from the 2nd element to the last one in the vector container
		   rawHitIt = rawHits.begin();
		   while( rawHitIt != rawHits.end() && !rawHits.empty() )
                   {
                        // check raw hit[i] and raw hit of ith existed cluster in a same column/row over one sensor area
                        if( (((*rawHitIt)->getSensor() == rawHitPtr->getSensor()) && ((*rawHitIt)->getRow() == rawHitPtr->getRow()) && (((*rawHitIt)->getColumn() == rawHitPtr->getColumn() + 1) || ((*rawHitIt)->getColumn() == rawHitPtr->getColumn() - 1) )) ||
                            (((*rawHitIt)->getSensor() == rawHitPtr->getSensor()) && ((*rawHitIt)->getColumn() == rawHitPtr->getColumn()) && (((*rawHitIt)->getRow() == rawHitPtr->getRow() + 1) || ((*rawHitIt)->getRow() == rawHitPtr->getRow() - 1) ))   )
                        { 
                            clusterSize = (*clusterIt)->getNRawHits() + 1;

                            if( ((*rawHitIt)->getRow() == rawHitPtr->getRow()) )
                                clusterSizeZ = (*clusterIt)->getNRawHitsZ() + 1; //same row
                            if( ((*rawHitIt)->getColumn() == rawHitPtr->getColumn()) )
                                clusterSizeRPhi = (*clusterIt)->getNRawHitsRPhi() + 1; //same column
			    
			    maxTb  = (*clusterIt)->getMaxTimeBin();
			
                            if(mTimeBin>=0 && mTimeBin < kIstNumTimeBins) {
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

                            rawHitMap_t &rawHitMap = (*clusterIt)->getRawHitMap();
                            rawHitMap[ *rawHitIt ] = 1;//add the raw hit into the cluster rawhits vector

                            //remove the clustered ith raw hit from the raw hits list
			    int distance = std::distance(rawHits.begin(), rawHitIt);
			    rawHits.erase( rawHitIt );
				
			    if(distance == 0)
				rawHitIt = rawHits.begin();
			    else	{
			    	rawHitIt = rawHits.begin() + distance - 1;
			    	rawHitIt++;
			    }
                        }//same row/column decision loop over
			else
				rawHitIt++;
                   } //raw hits loop over                               
            }//ith cluster's raw hits loop over
                
            //if the rawHitIt_th raw hit does not belong to the existed ith clusters then create a new cluster.
            if(rawHits.size() == 0)
	    	continue;

            rawHitIt = rawHits.begin();

            maxTb       = (*rawHitIt)->getMaxTimeBin();
            if(maxTb<0 || maxTb>=kIstNumTimeBins)       maxTb = (*rawHitIt)->getDefaultTimeBin();

            if(mTimeBin>=0 && mTimeBin < kIstNumTimeBins) {
                usedTb  = mTimeBin;
            }
	    else
		usedTb = maxTb;

            ladder      = (*rawHitIt)->getLadder();
            sensor      = (*rawHitIt)->getSensor();
            column      = (*rawHitIt)->getColumn();
            row         = (*rawHitIt)->getRow();
            totCharge   = (*rawHitIt)->getCharge(usedTb);
            totChargeErr= (*rawHitIt)->getChargeErr(usedTb);
            meanRow     = row;
            meanColumn  = column;
            clusterSize = 1;
            clusterSizeRPhi = 1;
            clusterSizeZ = 1;
                    
            newCluster = new StIstCluster((int)ladder*10000+clusters.getClusterVec().size(), ladder, sensor, meanRow, meanColumn, totCharge, totChargeErr, clusterType);
            newCluster->setNRawHits(clusterSize);
            newCluster->setNRawHitsRPhi(clusterSizeRPhi);
            newCluster->setNRawHitsZ(clusterSizeZ);
            newCluster->setMaxTimeBin(maxTb);

            int distanceCluster = std::distance(clusters.getClusterVec().begin(), clusterIt);

            rawHitMap_t &rawHitMap = newCluster->getRawHitMap();
            rawHitMap[ *rawHitIt ] = 1;
            clusters.getClusterVec().push_back(newCluster);

	    clusterIt = clusters.getClusterVec().begin() + distanceCluster;	
	    clusterIt++;
            //remove the clustered 1st raw hit from the raw hits list
            rawHits.erase( rawHitIt );
        }//clusters loop over
      }//end raw hits number > 1 cut
    }//end raw hits number > 0 cut

    ////////////////////////////////  do cluster splitting  /////////////////////////////////////////
    if(mSplitCluster)   {
        Int_t loc_ierr = doSplitting(clusters);
        if(loc_ierr!=kStOk) {
            LOG_WARN <<"StIstClusterMaker::Make(): Cluster splitting for ladder " << ladder << " returned " << loc_ierr <<endm;
        }
    }
 
  return kStOk;
};
ClassImp(StIstSimpleClusterAlgo);
