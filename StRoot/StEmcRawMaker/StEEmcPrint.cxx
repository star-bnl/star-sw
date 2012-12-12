//*-- Author : Wei-Ming Zhang
// $Id: StEEmcPrint.cxx,v 1.4 2012/12/12 22:05:30 fisyak Exp $

#ifdef __APPLE__
#include <sys/types.h>
#endif
#include <iostream>
#include <StMessMgr.h>

#include "StEEmcPrint.h"
#include "StEventTypes.h"

ClassImp(StEEmcPrint);
StEEmcPrint::StEEmcPrint() {
  mPrint = 15;
}
StEEmcPrint::~StEEmcPrint(){ }

//______________________________________________________________________
//______________________________________________________________________
void 
StEEmcPrint::print(StEmcCollection * emcCol) {
  LOG_DEBUG  << "mPrint = " << mPrint << endm;
  int detBit[4] = {1, 2, 4, 8}; 
  char uv = 'U';

  for(int iDet = kEndcapEmcTowerId; iDet<=  kEndcapSmdVStripId; iDet++){
    if(!(mPrint & detBit[iDet - kEndcapEmcTowerId])) continue; 
    if(iDet == kEndcapSmdVStripId) uv++;
    if(iDet == kEndcapEmcTowerId) {
      LOG_DEBUG  << "EEmc Tower" << endm;
    }
    else if(iDet == kEndcapEmcPreShowerId) {
      LOG_DEBUG  << "EEmc Pre/Post" << endm;
    }
    else {
      LOG_DEBUG  << "EEmc SMD-" << uv <<  endm;
    }
   
    StDetectorId detId = StDetectorId(iDet);
    StEmcDetector* StEEmcDet = emcCol->detector(detId);

    if(StEEmcDet !=0) {
      for(unsigned int sec = 1; sec <= StEEmcDet->numberOfModules(); sec++){
        StEmcModule* StEEmcMod =  StEEmcDet->module(sec);
        LOG_DEBUG  << "Sector = " << sec  << endm;	

        if(StEEmcMod != 0) {
          StSPtrVecEmcRawHit & EEmcRawHits = StEEmcMod->hits();
          LOG_DEBUG  << "Number of Hits = " << EEmcRawHits.size() << endm;

          for(unsigned int i=0; i<EEmcRawHits.size() ;i++){
            int sub = EEmcRawHits[i]->sub();
            int eta = EEmcRawHits[i]->eta();
            float e = EEmcRawHits[i]->energy();
            int adc = EEmcRawHits[i]->adc();

            if(iDet == kEndcapEmcTowerId) { 
	      LOG_DEBUG  <<Form("i=%d  Tower %2.2dT%c%2.2d: energy=%f adc=%d",i,sec,sub+'A'-1,eta,e,adc )<< endm;
            }
            else if(iDet == kEndcapEmcPreShowerId) {
              int psub = (sub%5 == 0) ? 5:sub%5;
              int pre = (sub-1)/5 + 1; 
	      LOG_DEBUG  <<Form("i=%d  pre/post(%d) %2.2d%c%c%2.2d : energy=%f adc=%d",i,pre,sec,pre+'P'-1,psub+'A'-1,eta,e,adc)<< endm;
            }
            else {
	       LOG_DEBUG  <<Form("SMD-%c  %2.2d%c%3.3d : energy=%f  adc=%d",uv,sec,uv,eta,e,adc)<< endm;
            }
          }
        }
      }
    }
  }
 
}


//______________________________________________________________________
//______________________________________________________________________
void 
StEEmcPrint::printChange(StEmcCollection * emcCol,StEmcCollection * emcColB,char *comm){
  //For towers & ecolB  zero ADC values are assumed to not match to  ecolA

  LOG_DEBUG  << comm<<"  mPrint mode= " << mPrint << endm;
  int detBit[4] = {1, 2, 4, 8}; 
  char uv = 'U';

  for(int iDet = kEndcapEmcTowerId; iDet<=  kEndcapSmdVStripId; iDet++){
    if(!(mPrint & detBit[iDet - kEndcapEmcTowerId])) continue; 
    if(iDet == kEndcapSmdVStripId) uv++;
    if(iDet == kEndcapEmcTowerId) {
      LOG_DEBUG  << "EEmc Tower" << endm;
    }
    else if(iDet == kEndcapEmcPreShowerId) {
      LOG_DEBUG  << "EEmc Pre/Post" << endm;
    }
    else  {
      LOG_DEBUG  << "EEmc SMD-" << uv <<  endm;
    }
   
    StDetectorId detId = StDetectorId(iDet);
    StEmcDetector* StEEmcDet = emcCol->detector(detId);
    StEmcDetector* StEEmcDetB = emcColB->detector(detId);

    if(StEEmcDet ==0) continue;
    if(StEEmcDetB==0) {
      LOG_DEBUG  << " colB has no data for above sub-detector"<<endm; 
      continue;
    }

    for(unsigned int sec = 1; sec <= StEEmcDet->numberOfModules(); sec++){
      StEmcModule* StEEmcMod =  StEEmcDet->module(sec);
      if(StEEmcMod == 0)  continue;

      StEmcModule* StEEmcModB =  StEEmcDetB->module(sec);
      LOG_DEBUG  << "Sector = " << sec <<endm ;	
      
      assert(StEEmcModB);//tmp

      StSPtrVecEmcRawHit & EEmcRawHits = StEEmcMod->hits();
      StSPtrVecEmcRawHit & EEmcRawHitsB = StEEmcModB->hits();
      LOG_DEBUG  << " Number of HitsA = " << EEmcRawHits.size() << ", HitsB="<<EEmcRawHitsB.size()<<endm;

      for(unsigned int i=0; i<EEmcRawHits.size() ;i++){
	int sub = EEmcRawHits[i]->sub();
	uint eta = EEmcRawHits[i]->eta();
	float e = EEmcRawHits[i]->energy();
	int adc = EEmcRawHits[i]->adc();

	int adcB=-99999;
	bool match=false;
	for(unsigned int j=0; j<EEmcRawHitsB.size() ;j++){
	  if(sub != EEmcRawHitsB[j]->sub()) continue;
	  if(eta != EEmcRawHitsB[j]->eta()) continue;
	  adcB= EEmcRawHitsB[j]->adc();
	  if(iDet == kEndcapEmcTowerId && adcB==0) continue;
	  match=true;
	  break;
	}
	
	if(!match) continue;

	if(iDet == kEndcapEmcTowerId) {
	  LOG_DEBUG  << Form("\ti=%d ecolA:  Tower %2.2dT%c%2.2d: energy=%f adc=%d <--> %d=adc old  in ecolB",i,sec,sub+'A'-1,eta,e,adc, adcB)<<endm;
	} else if(iDet == kEndcapEmcPreShowerId) {
	  int psub = (sub%5 == 0) ? 5:sub%5;
	  int pre = (sub-1)/5 + 1; 
	  LOG_DEBUG  << Form("\ti=%d  ecolA: pre/post(%d) %2.2d%c%c%2.2d : energy=%f adc=%d<--> %d=adc old in ecolB",i,pre,sec,pre+'P'-1,psub+'A'-1,eta,e,adc,adcB)<<endm;
	}
	else { 
	  LOG_DEBUG  << Form("\tSMD-%c  %2.2d%c%3.3d : energy=%f  adc=%d <--> %d=adc old in ecolB",uv,sec,uv,eta,e,adc, adcB)<<endm;
        }
      }
    }
  }
}

