//
// \class StGmtSimpleClusterAlgo
// \authors K.S. Engle and Richard Witt (witt@usna.edu)

#include "St_base/StMessMgr.h"
#include "StGmtSimpleClusterAlgo.h"

#include "StRoot/StGmtUtil/geometry/StGmtGeom.h"
#include "StRoot/StEvent/StGmtStripCollection.h"
#include "StRoot/StEvent/StGmtStrip.h"
#include "StRoot/StEvent/StGmtHitCollection.h"
#include "StRoot/StEvent/StGmtHit.h"
#include "TMath.h"

//for floor
#include <math.h>
ClassImp(StGmtSimpleClusterAlgo);
//________________________________________________________________________________
Int_t StGmtSimpleClusterAlgo::Init() {
  mPedestals = new Double_t* [kGmtNumConnectedStripsX + kGmtNumConnectedStripsY];//[kGmtNumModules]; //TO BE MODULE WISE !! HERE
  mPedestalStdDev = new Double_t* [kGmtNumConnectedStripsX + kGmtNumConnectedStripsY];//[kGmtNumModules]; 
  mPedestalRMS = new Double_t* [kGmtNumConnectedStripsX + kGmtNumConnectedStripsY];//[kGmtNumModules]; 
  mCalculated = new Bool_t* [kGmtNumConnectedStripsX + kGmtNumConnectedStripsY];//[kGmtNumModules]; 
  mEv=0;
  mCalcOk = new Bool_t[kGmtNumModules];   for(Int_t imx=0; imx < kGmtNumModules; imx++) mCalcOk[imx]=false;
  Int_t loopMax = kGmtNumConnectedStripsX + kGmtNumConnectedStripsY;  
  for(Int_t idx=0; idx < loopMax; idx++) {
    mPedestals[idx] = new Double_t[kGmtNumModules];
    mPedestalStdDev[idx]= new Double_t[kGmtNumModules];
    mPedestalRMS[idx]= new Double_t[kGmtNumModules];
    mCalculated[idx]= new Bool_t[kGmtNumModules];
    for(Int_t imx=0; imx <  kGmtNumModules; imx++) {
	mPedestals[idx][imx] = -99.0;
	mPedestalStdDev[idx][imx] = -99.0;
	mPedestalRMS[idx][imx] = -99.0;
	mCalculated[idx][imx] = false;
    }
  }//end init arrays
  return kStOk;
};
//________________________________________________________________________________
/** 
    The simple cluster algorithm. It used the fact that neighbouring strips have neighbouring geoIds. 
    The hits are sorted according to the geoId and the list of hits is then checked for geoIds that are next to each other.
    If there is a hit in the R layer that is in the inner half of the detector, it is allowed in the phi to skip a geoId.
    The error on the cluster charge is computed from the errors on the strip charges and the error on the position is the stdDev of the ordinates.
    So no weighting by charge yet. 
    The code gets called for each disk separately, so global coordinates have to be set later by the calling cluster maker.
*/
Int_t StGmtSimpleClusterAlgo::doClustering(UInt_t module, StGmtStripCollection& strips, StGmtHitCollection& clusters) {
  strips.sortByCoord();
  Int_t rdo, arm, apv, channel;
  Double_t localX=0.0, localY=0.0; 
  Double_t maxAdcX=0.0, maxAdcY=0.0; 
  //====== FIX ME!!!!!!!! (from here down)
  Float_t defaultError = 0.001;
  Short_t prvModusle;
  //   Char_t layer,prvLayer,noLayer='z';
  Int_t layer;
  Char_t prvLayer,noLayer='z';
  Int_t prvGeoId;
  Double_t accuCharge=0; 
  Double_t accuChargeError=0;
  Int_t numStrips=0;
  prvLayer=noLayer;
  bool isLocalY, isLocalX;
  StGmtHit* newCluster=0;
  //to compute energy weighted strip id
  Double_t meanGeoId=0;
  //for R < R/2 cm the difference in geo id of the phi strips is 2 and only even numbers are used...
  bool stepTwo=false;

  //subtractPedestals(strips, module); //Event by Event pedestal calculation + pedestal substraction   
  if(mEv<5)
    calculatePedestals(strips, module);
  else {
    if(mCalcOk[module]) {//pedestal ok, we can look for clusters
      applyPedestals(strips, module); // pedestal substraction only   
      findClusters(strips, module, clusters);
    } else {
      calculatePedestals(strips, module);
      checkPedestals(strips, module); //will decide and set mCalcOk
    }
  }
  if(module==kGmtNumModules-1) mEv++;
  return kStOk;
}
//________________________________________________________________________________
void StGmtSimpleClusterAlgo::checkPedestals( StGmtStripCollection& strips, UInt_t module ) {
#if 0
  LOG_INFO << " StGmtSimpleClusterAlgo::checkPedestals " << mEv << " ok:" << mCalcOk[module]  << " mod. " << module<< endm; 
#endif
  Int_t loopMax = kGmtNumConnectedStripsX + kGmtNumConnectedStripsY;  
  for( Int_t mapIdx=0; mapIdx < loopMax; mapIdx++)    {    
    if (mPedestals[mapIdx][module]<0) continue;
    if (mPedestalStdDev[mapIdx][module]<5 || mPedestalStdDev[mapIdx][module]>130) continue;
    mCalculated[mapIdx][module] = true;
  }
  mCalcOk[module] = true;
  for( Int_t mapIdx=0; mapIdx < loopMax; mapIdx++)
    if(!mCalculated[mapIdx][module])  {
      LOG_INFO << "Pedestal calculation FAILED: Channel " << mapIdx << "\tmodule:" << module
	       << "\tPed:" << mPedestals[mapIdx][module] << "\tRMS:" << mPedestalStdDev[mapIdx][module] << endm;
      mEv=0; //reset pedestal counter
      mCalcOk[module] = false;
      //reset channel value
      mPedestals[mapIdx][module]=0.;
      mPedestalStdDev[mapIdx][module]=0.;
      mPedestalRMS[mapIdx][module]=0.;
    }
#if 0
  LOG_INFO << " StGmtSimpleClusterAlgo::checkPedestals - Finish. " << mEv << " ok:" << mCalcOk[module]  << " mod. " << module<< endm; 
#endif
}
//________________________________________________________________________________
void StGmtSimpleClusterAlgo::applyPedestals( StGmtStripCollection& strips, UInt_t module ) {
#if 0
  LOG_INFO << " StGmtSimpleClusterAlgo::applyPedestals " << mEv << " ok" << mCalcOk[module]  <<" mod. " << module<< endm; 
#endif
  Int_t loopMax = kGmtNumConnectedStripsX + kGmtNumConnectedStripsY;  
  // Now do the actual subtraction    
  for( Int_t mapIdx=0; mapIdx < loopMax; mapIdx++)    {    
    StGmtStrip* stripPtr = strips.getSortedStrip( mapIdx );
    stripPtr->setPed(mPedestals[mapIdx][module]);
    stripPtr->setPedStdDev(mPedestalStdDev[mapIdx][module]);
    stripPtr->setPedErr(mPedestalRMS[mapIdx][module]);
    for( Int_t timebin = 0; timebin < kGmtNumTimeBins; timebin++)	{
      // mSum[mapIdx][module] += stripPtr->getAdc(timebin);
      stripPtr->setPedSubtractedAdc( stripPtr->getAdc(timebin) - stripPtr->getPed(), timebin );
    } 
  }
}
//________________________________________________________________________________
void StGmtSimpleClusterAlgo::calculatePedestals(StGmtStripCollection& strips, UInt_t module) {
#if 0
  LOG_INFO << " StGmtSimpleClusterAlgo::calculatePedestals Ev:" << mEv << "\tmOk:" << mCalcOk[module]  << "\tmod:" << module<< endm; 
#endif
  Double_t mSum[kGmtNumConnectedStripsX + kGmtNumConnectedStripsY]; 
  Double_t mSqrSum[kGmtNumConnectedStripsX + kGmtNumConnectedStripsY]; 
  Int_t mCounters[kGmtNumConnectedStripsX + kGmtNumConnectedStripsY];
  Int_t loopMax = kGmtNumConnectedStripsX + kGmtNumConnectedStripsY;
  double usedtb = kGmtNumTimeBins;
  for(Int_t idx=0; idx < loopMax; idx++)    {
    mSum[idx] = 0.0;
    mSqrSum[idx] = 0.0;  
    mCounters[idx] = 0;
  }
  for(Int_t mapIdx=0; mapIdx < loopMax; mapIdx++)
    if(!mCalculated[mapIdx][module]) {//only calculate for needed ones
      StGmtStrip* stripPtr = strips.getSortedStrip(mapIdx);    
      for(Int_t timebin = 0; timebin < usedtb; timebin++) {
	int adc = stripPtr->getAdc(timebin);
	if(adc<0) continue;
	mSum[mapIdx] += adc;
	mSqrSum[mapIdx] += pow(adc,2);
	mCounters[mapIdx]++;
	// if (a<25 && !module){ LOG_INFO << "Module=" << module << "\tgeoid=" 
	//<< stripPtr->getGeoId() << "\ttb="<< timebin<< "\tadc=" << stripPtr->getAdc(timebin) << endm;a++;}
      }
    }
  for(Int_t mapIdx=0; mapIdx < loopMax; mapIdx++)
    if(!mCalculated[mapIdx][module]) {    
      // and calculate the average pedestal, sigma and RMS for each channel
      double pedtmp = mSum[mapIdx] / mCounters[mapIdx];
      double stddevtmp = sqrt((mSqrSum[mapIdx] - (double)mCounters[mapIdx] * pedtmp * pedtmp) 
				/ (double)(mCounters[mapIdx] - 1));
      //saving
      mPedestals[mapIdx][module] = (mPedestals[mapIdx][module]*mEv + pedtmp)/(mEv+1);
      mPedestalStdDev[mapIdx][module] = (mEv*mPedestalStdDev[mapIdx][module] + stddevtmp)/(mEv+1); 
      mPedestalRMS[mapIdx][module]  = sqrt(TMath::Max(0.0, mPedestals[mapIdx][module] * mPedestals[mapIdx][module] 
							- mPedestalStdDev[mapIdx][module] * mPedestalStdDev[mapIdx][module]));
    }
}
//________________________________________________________________________________
Double_t StGmtSimpleClusterAlgo::getMeanPosition( StGmtStripCollection& strips, Int_t module, Int_t isY, Int_t Maxstr, Int_t Maxtb) {
  Int_t lowstr=Maxstr-2, highstr=Maxstr+2, lowtb=Maxtb-1, hightb=Maxtb+1;
  Double_t ADCsumTB[kGmtNumTimeBins]={0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0};
  Double_t ADCsum=0.0, xysum=0.0;
  Int_t loopMax = (isY ? (kGmtNumStrips + kGmtNumConnectedStripsY) : kGmtNumConnectedStripsX);
  
  while(lowstr <= 0) lowstr++;
  while(lowtb < 0) lowtb++;
  while(highstr > loopMax) highstr--;
  while(hightb >= kGmtNumTimeBins) hightb--;
  for(Int_t element=lowstr; element <= highstr; element++)  {
    StGmtStrip* stripPtr = strips.getSortedStrip( element );
    for(Int_t timebin=lowtb; timebin <= hightb; timebin++)    {
      if( stripPtr->getAdc(timebin) > kGmtHitCut )      {
	ADCsumTB[element-lowstr] += stripPtr->getAdc(timebin);
	ADCsum += stripPtr->getAdc(timebin);
      }
    }
    xysum+=ADCsumTB[element-lowstr]*element;
  }
  return xysum/ADCsum;
}
//________________________________________________________________________________
void StGmtSimpleClusterAlgo::getMaximumElement(StGmtStripCollection& strips, Int_t module, Int_t isY, Int_t *element, Int_t *tb, Double_t *maxADC) {
  *element=0;*maxADC=0.0;*tb=-1;
  Int_t loopMax = (isY ? (kGmtNumStrips + kGmtNumConnectedStripsY) : kGmtNumConnectedStripsX);
  for( Int_t mapIdx=0+isY*kGmtNumStrips; mapIdx < loopMax; mapIdx++)  {
    StGmtStrip* stripPtr = strips.getSortedStrip( mapIdx );
    for( Int_t timebin = 0+kGmtNumTimeBinsForPed; timebin < kGmtNumTimeBins; timebin++)    {
      if( stripPtr->getAdc(timebin) > (*maxADC) && stripPtr->getAdc(timebin) > kGmtHitCut ) {// FIX ME, need to multiply by sigma of ADC
	*maxADC=stripPtr->getAdc(timebin);
	*element = mapIdx;
	*tb=timebin;
      }
    }
  }
}
//________________________________________________________________________________
void StGmtSimpleClusterAlgo::findClusters( StGmtStripCollection& strips, UInt_t module, StGmtHitCollection& clusters ) {
#if 0
  LOG_INFO << "IN FIND CLUSTER " << module << endm;
#endif
  Int_t used = -1;
  Int_t sigXIndex = -1; 
  Int_t sigYIndex = -1;
  Int_t sigXAdc = 0; 
  Int_t sigYAdc = 0;
  Int_t sigXTb = 0;
  Int_t sigYTb = 0;
  Double_t clusterLocalX = 0;
  Double_t clusterLocalY = 0;
  // will find seeds and marks nearest neighbors for exclusion
  for(Int_t idx=0; idx < kGmtNumStripsPerModule; idx++)  {
    StGmtStrip* stripPtr = strips.getSortedStrip(idx);
    if(idx < kGmtNumConnectedStripsX-1) {
      if(stripPtr->getMaxPedSubtractedAdc() > sigXAdc && 
	  stripPtr->getMaxPedSubtractedAdc() > kGmtHitCut * stripPtr->getPedStdDev()) {
	sigXAdc = stripPtr->getMaxPedSubtractedAdc();
	sigXTb = stripPtr->getMaxPedSubtractedAdcTB();
	sigXIndex = idx;
      }
    }    else    {
      if( stripPtr->getMaxPedSubtractedAdc() > sigYAdc && 
	stripPtr->getMaxPedSubtractedAdc() > kGmtHitCut * stripPtr->getPedStdDev() )       {
	sigYAdc = stripPtr->getMaxPedSubtractedAdc();
	sigYTb = stripPtr->getMaxPedSubtractedAdcTB();
	sigYIndex = idx;
      }
    }
  }
  //sigX and sigY are the 2 max hits
  if( sigXAdc > 0 && sigYAdc > 0 )    {
#if 0
    LOG_INFO << "\t sigXAdc " << sigXAdc << " sigYAdc " << sigYAdc << endm;
    LOG_INFO << "\t sigXTb " << sigXTb << " sigYTb " << sigYTb << endm;
#endif
    if(sigXTb <= sigYTb+1 && sigXTb >= sigYTb-1) {
      StGmtStrip* stripPtrX = strips.getSortedStrip(sigXIndex); stripPtrX->setIsC(1);
      StGmtStrip* stripPtrXNext = 0;
      StGmtStrip* stripPtrXPrev = 0;
      StGmtStrip* stripPtrY = strips.getSortedStrip(sigYIndex); stripPtrY->setIsC(11);
      StGmtStrip* stripPtrYNext = 0;
      StGmtStrip* stripPtrYPrev = 0;
      clusterLocalX = stripPtrX->getPedSubtractedAdc(sigXTb) * stripPtrX->getPosition();
      clusterLocalY = stripPtrY->getPedSubtractedAdc(sigYTb) * stripPtrY->getPosition();
      Double_t sumX = stripPtrX->getPedSubtractedAdc(sigXTb);
      Double_t sumY = stripPtrY->getPedSubtractedAdc(sigYTb);
      if(sigXIndex+1 < kGmtNumConnectedStripsX-1)	    {
	stripPtrXNext = strips.getSortedStrip(sigXIndex+1); stripPtrXNext->setIsC(2);
	clusterLocalX += stripPtrXNext->getPedSubtractedAdc(sigXTb) * stripPtrXNext->getPosition();
	sumX += stripPtrXNext->getPedSubtractedAdc(sigXTb);
      }
      if(sigXIndex-1 > 0) {
	stripPtrXPrev = strips.getSortedStrip(sigXIndex-1);stripPtrXPrev->setIsC(3);
	clusterLocalX += stripPtrXPrev->getPedSubtractedAdc(sigXTb) * stripPtrXPrev->getPosition();
	sumX += stripPtrXPrev->getPedSubtractedAdc(sigXTb);
      }
      if(sigYIndex+1 < kGmtNumConnectedStripsX + kGmtNumConnectedStripsY-1) {
	stripPtrYNext = strips.getSortedStrip(sigYIndex+1);stripPtrYNext->setIsC(12);
	clusterLocalY += stripPtrYNext->getPedSubtractedAdc(sigYTb) * stripPtrYNext->getPosition();
	sumY += stripPtrYNext->getPedSubtractedAdc(sigYTb);
      }
      if(sigYIndex-1 > kGmtNumConnectedStripsX)	    {
	stripPtrYPrev = strips.getSortedStrip(sigYIndex-1); stripPtrYPrev->setIsC(13);
	clusterLocalY += stripPtrYPrev->getPedSubtractedAdc(sigYTb) * stripPtrYPrev->getPosition();
	sumY += stripPtrYPrev->getPedSubtractedAdc(sigYTb);
      }
      if((stripPtrXNext || stripPtrXPrev) && (stripPtrYNext || stripPtrYPrev)) {
	if(TMath::Abs(sumX) > 1e-7 && TMath::Abs(sumY) > 1e-7) {
	  clusterLocalX /= sumX;
	  clusterLocalY /= sumY;
#if 0	      
	  LOG_INFO << "  GOT A GMT HIT!!! " << module << 
	    " clusterLocalX " << clusterLocalX << " clusterLocalY " << clusterLocalY << endm;
#endif	
	  StGmtHit *newCluster = new StGmtHit(clusters.getHitVec().size(),
					      module, 
					      sigXAdc,//maxsubadc 
					      sigYAdc,
					      sigXTb, //maxsubtb
					      sigYTb,
					      clusterLocalX, //position*pedSubAdc(maxsubTb) 
					      clusterLocalY, 
					      0.0, 
					      0.0);
	  clusters.getHitVec().push_back(newCluster);
	}
      }   
    }
  }  // sigXAdc > 0 && sigYAdc > 0
#if 0
  LOG_INFO << "END FIND CLUSTER " << clusters.getHitVec().size() << endm;
#endif
}
//________________________________________________________________________________
void StGmtSimpleClusterAlgo::subtractPedestals( StGmtStripCollection& strips, UInt_t module ) {
  Double_t mSum[kGmtNumConnectedStripsX + kGmtNumConnectedStripsY]; 
  Double_t mSqrSum[kGmtNumConnectedStripsX + kGmtNumConnectedStripsY]; 
  Int_t mCounters[kGmtNumConnectedStripsX + kGmtNumConnectedStripsY];
  Int_t loopMax = kGmtNumConnectedStripsX + kGmtNumConnectedStripsY;
  int a = 0;
  // First pass over everything in the module (including hits)
  double usedtb = kGmtNumTimeBins;
  for(Int_t idx=0; idx < loopMax; idx++) {
    mSum[idx] = 0.0;
    mSqrSum[idx] = 0.0;
    mPedestals[idx][module] = 0.0;
    mPedestalStdDev[idx][module] = 0.0;
    mPedestalRMS[idx][module] = 0.0;
    mCounters[idx] = 0.0;
  }
  for( Int_t mapIdx=0; mapIdx < loopMax; mapIdx++)    {    
    StGmtStrip* stripPtr = strips.getSortedStrip( mapIdx );    
    for( Int_t timebin = 0; timebin < usedtb; timebin++)      {
      mSum[mapIdx] += stripPtr->getAdc(timebin);
      mSqrSum[mapIdx] += pow(stripPtr->getAdc(timebin),2);
      mCounters[mapIdx]++;
#if 0
      if (a<50 && !module){ 
#if 0
	LOG_INFO << "Module=" << module << "\tgeoid=" << stripPtr->getGeoId() << "\ttb="<< timebin<< "\tadc=" << stripPtr->getAdc(timebin) << endm;
#endif
	a++;
      }
#endif
    }
  }
  for( Int_t mapIdx=0; mapIdx < loopMax; mapIdx++)    {    
    // and calculate the average pedestal, sigma and RMS for each channel
    mPedestals[mapIdx][module] = mSum[mapIdx] / mCounters[mapIdx];
    mPedestalStdDev[mapIdx][module] = sqrt( ( mSqrSum[mapIdx] 
					      - (double)mCounters[mapIdx] * mPedestals[mapIdx][module] * mPedestals[mapIdx][module]) 
					    / (double)( mCounters[mapIdx] - 1 ));
    mPedestalRMS[mapIdx][module]    = sqrt( mPedestals[mapIdx][module] * mPedestals[mapIdx][module] 
					    - mPedestalStdDev[mapIdx][module] * mPedestalStdDev[mapIdx][module]);
  }
  // Second pass over everything in the module (excluding hits)
  for( Int_t idx=0; idx < loopMax; idx++)    {
    mSum[idx] = 0.0;
    mSqrSum[idx] = 0.0;
    mCounters[idx] = 0.0;
  }
  for( Int_t mapIdx=0; mapIdx < loopMax; mapIdx++)    {    
    StGmtStrip* stripPtr = strips.getSortedStrip( mapIdx );    
    for( Int_t timebin = 0; timebin < usedtb; timebin++)	{
      if( ( stripPtr->getAdc(timebin) > ( mPedestals[mapIdx][module] - ( kGmtPedCut * mPedestalStdDev[mapIdx][module] ) ) )  
	  && ( stripPtr->getAdc(timebin) <  ( mPedestals[mapIdx][module] + ( kGmtPedCut * mPedestalStdDev[mapIdx][module] ) ) ) 
	  && ( stripPtr->getAdc(timebin) >= 0 && stripPtr->getAdc(timebin) < kGmtMaxAdc) )	    {
	mSum[mapIdx] += stripPtr->getAdc(timebin);
	mSqrSum[mapIdx] += pow(stripPtr->getAdc(timebin),2);
	mCounters[mapIdx]++;
      }
    }
  }
  for( Int_t mapIdx=0; mapIdx < loopMax; mapIdx++)    {    
    // and calculate the average pedestal, sigma and RMS for each channel
    mPedestals[mapIdx][module] = mSum[mapIdx] / mCounters[mapIdx];
    mPedestalStdDev[mapIdx][module] = sqrt( ( mSqrSum[mapIdx] 
					      - (double)mCounters[mapIdx] * mPedestals[mapIdx][module] * mPedestals[mapIdx][module]) 
					    / (double)( mCounters[mapIdx] - 1 ));
    mPedestalRMS[mapIdx][module]    = sqrt( mPedestals[mapIdx][module] * mPedestals[mapIdx][module] 
					    - mPedestalStdDev[mapIdx][module] * mPedestalStdDev[mapIdx][module]);
  }
  a=0;
  // Now do the actual subtraction    
  for( Int_t mapIdx=0; mapIdx < loopMax; mapIdx++)    {    
    StGmtStrip* stripPtr = strips.getSortedStrip( mapIdx );
    stripPtr->setPed(mPedestals[mapIdx][module]);
    stripPtr->setPedStdDev(mPedestalStdDev[mapIdx][module]);
    stripPtr->setPedErr(mPedestalRMS[mapIdx][module]);
    for( Int_t timebin = 0; timebin < kGmtNumTimeBins; timebin++)	{
      mSum[mapIdx] += stripPtr->getAdc(timebin);
      stripPtr->setPedSubtractedAdc( stripPtr->getAdc(timebin) - stripPtr->getPed(), timebin );
#if 0
      if (a<50 && !module){ 
#if 0
	LOG_INFO << "Module=" << module << "\tgeoid=" << stripPtr->getGeoId() << "\ttb="<< timebin
		 << "\tadc=" << stripPtr->getAdc(timebin) << "\tped = " << stripPtr->getPed() << endm;
#endif
	a++;
      }
#endif
    }
  }
}


