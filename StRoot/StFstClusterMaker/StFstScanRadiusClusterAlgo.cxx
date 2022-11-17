#include "StEvent.h"
#include "StEvent/StEnumerations.h"
#include "StMessMgr.h"
#include "StFstScanRadiusClusterAlgo.h"
#include "StFstUtil/StFstCollection.h"
#include "StFstUtil/StFstRawHitCollection.h"
#include "StEvent/StFstRawHit.h"
#include "StFstUtil/StFstClusterCollection.h"
#include "StFstUtil/StFstCluster.h"
#include "StEvent/StFstConsts.h"

#include <math.h>
#include <iostream>
#include <iterator>
#include <vector>
#include <algorithm>


Int_t StFstScanRadiusClusterAlgo::doClustering(const StFstCollection &fstCollection, StFstRawHitCollection &rawHitsOriginal, StFstClusterCollection &clusters )
{
  StFstCluster *newCluster = 0;
  StFstRawHit *rawHitTemp = 0;
  StFstRawHit *rawHitMaxAdcTemp = 0;
  Int_t clusterType = kFstScanRadiusClusterAlgo;
  Int_t maxTb = -1, usedTb = -1;
  Int_t disk = 0, wedge = 0, sensor = -1, apv = -1;
  Float_t meanRStrip = -1., maxRStrip = -1, meanPhiStrip = -1;
  Float_t totCharge = 0., totChargeErr = 0.;
  Int_t clusterSize = 0, clusterSizeR = 0, clusterSizePhi = 0;
  unsigned short idTruth = 0;

  //get number of time bin used in this event
  Int_t nTimeBins = fstCollection.getNumTimeBins();

  //sort raw hits in increasing order by geometry ID
  rawHitsOriginal.sortByGeoId();

  //copy raw hit collection to temporary vectors
  std::vector<StFstRawHit *>  rawHitsVec[kFstNumSensorsPerWedge][kFstNumPhiSegPerSensor];
  std::vector<StFstCluster *> clustersVec[kFstNumSensorsPerWedge][kFstNumPhiSegPerSensor];
  std::vector<StFstRawHit *>  rawHitsToMerge;

  for (int sensorIdx = 0; sensorIdx < kFstNumSensorsPerWedge; sensorIdx++) {
    for (int phiIdx = 0; phiIdx < kFstNumPhiSegPerSensor; phiIdx++) {
      rawHitsVec[sensorIdx][phiIdx].reserve(kFstNumRStripsPerSensor);
      clustersVec[sensorIdx][phiIdx].reserve(kFstNumRStripsPerSensor);
    }
  }

  rawHitsToMerge.reserve(kFstNumRStripsPerSensor);

  for (std::vector< StFstRawHit * >::iterator rawHitPtr = rawHitsOriginal.getRawHitVec().begin(); rawHitPtr != rawHitsOriginal.getRawHitVec().end(); ++rawHitPtr) {
    int sensorIndex = (int)(*rawHitPtr)->getSensor();
    int phiIndex = (int)(*rawHitPtr)->getPhiStrip();
    rawHitsVec[sensorIndex][phiIndex].push_back( new StFstRawHit( *(*rawHitPtr)) );
  }

  //do clustering
  int clusterLabel = 0;

  for (int sensorIdx = 0; sensorIdx < kFstNumSensorsPerWedge; sensorIdx++)
  {
    //step 1: do clustering for each phi
    for (int phiIdx = 0; phiIdx < kFstNumPhiSegPerSensor; phiIdx++)
    {
      while ( !rawHitsVec[sensorIdx][phiIdx].empty() )
      {
	rawHitTemp = rawHitsVec[sensorIdx][phiIdx].back();
	rawHitsVec[sensorIdx][phiIdx].pop_back();
	rawHitsToMerge.push_back(rawHitTemp);
	//count number to merge
	int nToMerge = 1;
	//find all raw hits that are neighboring
	std::vector<StFstRawHit *>::iterator rawHitsToMergePtr = rawHitsToMerge.begin();
	rawHitMaxAdcTemp = *rawHitsToMergePtr;

	// put all raw hits in one phi strip to rawHitsToMerge
	while (rawHitsToMergePtr != rawHitsToMerge.end() && !rawHitsVec[sensorIdx][phiIdx].empty()) {
	  rawHitTemp = rawHitsVec[sensorIdx][phiIdx].back();
	  rawHitsVec[sensorIdx][phiIdx].pop_back();

	  ++nToMerge;
	  rawHitsToMerge.push_back(rawHitTemp);

	  if ( rawHitTemp->getCharge(rawHitTemp->getMaxTimeBin()) > (*rawHitsToMergePtr)->getCharge((*rawHitsToMergePtr)->getMaxTimeBin()) )
	    rawHitMaxAdcTemp = rawHitTemp;

	  ++rawHitsToMergePtr;
	}

	//used time bin index (raw hits with maximum ADC holds the time-bin priority)
	maxTb   = rawHitMaxAdcTemp->getMaxTimeBin();
	idTruth = rawHitMaxAdcTemp->getIdTruth();

	if (maxTb < 0 || maxTb >= nTimeBins)         maxTb  = rawHitMaxAdcTemp->getDefaultTimeBin();

	if (mTimeBin < nTimeBins)                    usedTb = mTimeBin;
	else                                         usedTb = maxTb;

	disk            = rawHitMaxAdcTemp->getDisk();
	wedge      	= rawHitMaxAdcTemp->getWedge();
	sensor      	= rawHitMaxAdcTemp->getSensor(); // = sensorIdx
	apv      	= rawHitMaxAdcTemp->getApv();
	meanPhiStrip    = (float)rawHitMaxAdcTemp->getPhiStrip(); // = phiIdx
	clusterSize 	= nToMerge;
	clusterSizeR 	= nToMerge;
	clusterSizePhi 	= 1;

	float tempCharge[nToMerge], tempChargeErr[nToMerge], tempRStrip[nToMerge];

	for (int i = 0; i < nToMerge; i++) {
	  tempCharge[i] = 0.; tempChargeErr[i] = 0.; tempRStrip[i] = 0.;
	}

	float tempSumCharge = 0, tempSumChargeErrSquare = 0.;
	int mergeIdx = 0;
	//count number to seed hit
	int nToSeedhit = 0;

	for (rawHitsToMergePtr = rawHitsToMerge.begin(); rawHitsToMergePtr != rawHitsToMerge.end() && mergeIdx < nToMerge; ++rawHitsToMergePtr, ++mergeIdx) {
	  tempCharge[mergeIdx]    = (*rawHitsToMergePtr)->getCharge(usedTb);
	  tempChargeErr[mergeIdx] = (*rawHitsToMergePtr)->getChargeErr(usedTb);
	  tempRStrip[mergeIdx]    = (float)(*rawHitsToMergePtr)->getRStrip();
	  tempSumCharge += (*rawHitsToMergePtr)->getCharge(usedTb);
	  if ( (*rawHitsToMergePtr)->getSeedhitflag() == 1 ) ++nToSeedhit;
	}

	meanRStrip = 0;
	maxRStrip = 0;

	for (int iRawHit = 0; iRawHit < nToMerge; iRawHit++)  {
	  if(tempRStrip[iRawHit]>maxRStrip)
	    maxRStrip = tempRStrip[iRawHit];
	  meanRStrip = maxRStrip;
	  tempSumChargeErrSquare += tempChargeErr[iRawHit] * tempChargeErr[iRawHit];
	}

	totCharge    = tempSumCharge;
	totChargeErr = sqrt(tempSumChargeErrSquare / nToMerge);

	newCluster = new StFstCluster((int)wedge * 10000 + clusterLabel, disk, wedge, sensor, apv, meanRStrip, meanPhiStrip, totCharge, totChargeErr, clusterType);
	newCluster->setNRawHits(clusterSize);
	newCluster->setNRawHitsR(clusterSizeR);
	newCluster->setNRawHitsPhi(clusterSizePhi);
	newCluster->setMaxTimeBin(maxTb);
	newCluster->setIdTruth(idTruth);

	if(nToSeedhit>0) {
	  clustersVec[sensorIdx][phiIdx].push_back(newCluster);
	  clusterLabel++;
	}

	rawHitsToMerge.clear();
      }//end current phi raw hits loop
    }//end current sensor raw hits loop

    //step 2: do clustering for neighboring phistrips
    std::vector<StFstCluster *>::iterator clusterIt1, clusterIt2;
    for (int phiIdx1 = 0; phiIdx1 < kFstNumPhiSegPerSensor - 1; phiIdx1++) {
      int phiIdx2 = phiIdx1 + 1;

      if (clustersVec[sensorIdx][phiIdx1].size() > 0 && clustersVec[sensorIdx][phiIdx2].size() > 0) {
	for (clusterIt1 = clustersVec[sensorIdx][phiIdx1].begin(); clusterIt1 != clustersVec[sensorIdx][phiIdx1].end() && !clustersVec[sensorIdx][phiIdx1].empty(); clusterIt1++) {
	  for (clusterIt2 = clustersVec[sensorIdx][phiIdx2].begin(); clusterIt2 != clustersVec[sensorIdx][phiIdx2].end() && !clustersVec[sensorIdx][phiIdx1].empty(); clusterIt2++) {
	    float rstripDfstance = (*clusterIt1)->getMeanRStrip() - (*clusterIt2)->getMeanRStrip();

	    if (TMath::Abs(rstripDfstance) < 3.5) { //here 3.5 means the dfstance between two clusters' weighted centers in r direction smaller than 3.5 rStrip
	      maxTb   = (*clusterIt1)->getMaxTimeBin();
	      idTruth = (*clusterIt1)->getIdTruth();
	      apv     = (*clusterIt1)->getApv();
	      if((*clusterIt1)->getTotCharge() < (*clusterIt2)->getTotCharge()) {
		maxTb   = (*clusterIt2)->getMaxTimeBin();
		idTruth = (*clusterIt2)->getIdTruth();
		apv     = (*clusterIt2)->getApv();
	      }

	      totCharge      = (*clusterIt1)->getTotCharge() + (*clusterIt2)->getTotCharge();
	      totChargeErr   = sqrt(((*clusterIt1)->getTotChargeErr() * (*clusterIt1)->getTotChargeErr() * (*clusterIt1)->getNRawHits() + (*clusterIt2)->getTotChargeErr() * (*clusterIt2)->getTotChargeErr() * (*clusterIt2)->getNRawHits()) / ((*clusterIt1)->getNRawHits() + (*clusterIt2)->getNRawHits()));
	      clusterSize    = (*clusterIt1)->getNRawHits() + (*clusterIt2)->getNRawHits();
	      int maxClusterR1 = (*clusterIt1)->getMeanRStrip(); 
	      int minClusterR1 = (*clusterIt1)->getMeanRStrip() - (*clusterIt1)->getNRawHitsR() + 1; 
	      int maxClusterR2 = (*clusterIt2)->getMeanRStrip(); 
	      int minClusterR2 = (*clusterIt2)->getMeanRStrip() - (*clusterIt2)->getNRawHitsR() + 1; 
	      int maxClusterR = maxClusterR1 > maxClusterR2 ? maxClusterR1 : maxClusterR2;
	      int minClusterR = minClusterR1 < minClusterR2 ? minClusterR1 : minClusterR2;
	      clusterSizeR   = maxClusterR - minClusterR + 1; // max at 4
	      clusterSizePhi = (*clusterIt1)->getNRawHitsPhi() + (*clusterIt2)->getNRawHitsPhi();
	      meanRStrip     = (*clusterIt1)->getMeanRStrip();
	      if ((*clusterIt2)->getMeanRStrip() > (*clusterIt1)->getMeanRStrip()) 
		meanRStrip  = (*clusterIt2)->getMeanRStrip();
	      meanPhiStrip = (*clusterIt1)->getMeanPhiStrip() * (*clusterIt1)->getTotCharge() / totCharge + (*clusterIt2)->getMeanPhiStrip() * (*clusterIt2)->getTotCharge() / totCharge;

	      (*clusterIt2)->setMeanRStrip(meanRStrip);
	      (*clusterIt2)->setMeanPhiStrip(meanPhiStrip);
	      (*clusterIt2)->setTotCharge(totCharge);
	      (*clusterIt2)->setTotChargeErr(totChargeErr);
	      (*clusterIt2)->setNRawHits(clusterSize);
	      (*clusterIt2)->setNRawHitsR(clusterSizeR);
	      (*clusterIt2)->setNRawHitsPhi(clusterSizePhi);
	      (*clusterIt2)->setIdTruth(idTruth);
	      (*clusterIt2)->setApv(apv);

	      int distance1 = std::distance(clustersVec[sensorIdx][phiIdx1].begin(), clusterIt1);
	      clustersVec[sensorIdx][phiIdx1].erase(clusterIt1);

	      if (distance1 == 0)
		clusterIt1 = clustersVec[sensorIdx][phiIdx1].begin();
	      else
		--clusterIt1;
	    }//end merge
	  }//end clusters vector 2 loop
	}//end clusters vector 1 loop
      }//end phi cluster number cut
    }//end current sensor cluster loop
  }//end all sensor clustering loop

  //fill output container
  std::vector<StFstCluster *>::iterator clusterIt;

  for (int sensorIdx = 0; sensorIdx < kFstNumSensorsPerWedge; sensorIdx++) {
    for (int phiIdx = 0; phiIdx < kFstNumPhiSegPerSensor; phiIdx++) {
      if (clustersVec[sensorIdx][phiIdx].size() <= 0) continue;

      for (clusterIt = clustersVec[sensorIdx][phiIdx].begin(); clusterIt != clustersVec[sensorIdx][phiIdx].end(); ++clusterIt)
	clusters.getClusterVec().push_back(*clusterIt);

      rawHitsVec[sensorIdx][phiIdx].clear();
      clustersVec[sensorIdx][phiIdx].clear();
    }
  }

  return kStOk;
}
