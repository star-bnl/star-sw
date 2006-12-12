//*-- Author : Wei-Ming Zhang
// $Id: StEEmcPrint.cxx,v 1.1 2006/12/12 20:29:16 balewski Exp $

#include "StEEmcPrint.h"
#include <iostream>
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
  cout << "mPrint = " << mPrint << endl;
  int detBit[4] = {1, 2, 4, 8}; 
  char uv = 'U';

  for(int iDet = kEndcapEmcTowerId; iDet<=  kEndcapSmdVStripId; iDet++){
    if(!(mPrint & detBit[iDet - kEndcapEmcTowerId])) continue; 
    if(iDet == kEndcapSmdVStripId) uv++;
    if(iDet == kEndcapEmcTowerId) cout << "EEmc Tower" << endl;
    else if(iDet == kEndcapEmcPreShowerId) cout << "EEmc Pre/Post" << endl;
    else  cout << "EEmc SMD-" << uv <<  endl;
   
    StDetectorId detId = StDetectorId(iDet);
    StEmcDetector* StEEmcDet = emcCol->detector(detId);

    if(StEEmcDet !=0) {
      for(unsigned int sec = 1; sec <= StEEmcDet->numberOfModules(); sec++){
        StEmcModule* StEEmcMod =  StEEmcDet->module(sec);
        std::cout << "Sector = " << sec  << std::endl;	

        if(StEEmcMod != 0) {
          StSPtrVecEmcRawHit & EEmcRawHits = StEEmcMod->hits();
          cout << "Number of Hits = " << EEmcRawHits.size() << endl;

          for(unsigned int i=0; i<EEmcRawHits.size() ;i++){
            int sub = EEmcRawHits[i]->sub();
            int eta = EEmcRawHits[i]->eta();
            float e = EEmcRawHits[i]->energy();
            int adc = EEmcRawHits[i]->adc();

            if(iDet == kEndcapEmcTowerId) 
              printf("\ti=%d  Tower %2.2dT%c%2.2d: energy=%f adc=%d\n",
                     i,sec,sub+'A'-1,eta,e,adc );
            else if(iDet == kEndcapEmcPreShowerId) {
              int psub = (sub%5 == 0) ? 5:sub%5;
              int pre = (sub-1)/5 + 1; 
              printf("\ti=%d  pre/post(%d) %2.2d%c%c%2.2d : energy=%f adc=%d\n",
                     i,pre,sec,pre+'P'-1,psub+'A'-1,eta,e,adc);
            }
            else 
             printf("\tSMD-%c  %2.2d%c%3.3d : energy=%f  adc=%d\n",
                     uv,sec,uv,eta,e,adc);
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

  cout << comm<<"  mPrint mode= " << mPrint << endl;
  int detBit[4] = {1, 2, 4, 8}; 
  char uv = 'U';

  for(int iDet = kEndcapEmcTowerId; iDet<=  kEndcapSmdVStripId; iDet++){
    if(!(mPrint & detBit[iDet - kEndcapEmcTowerId])) continue; 
    if(iDet == kEndcapSmdVStripId) uv++;
    if(iDet == kEndcapEmcTowerId) cout << "EEmc Tower" << endl;
    else if(iDet == kEndcapEmcPreShowerId) cout << "EEmc Pre/Post" << endl;
    else  cout << "EEmc SMD-" << uv <<  endl;
   
    StDetectorId detId = StDetectorId(iDet);
    StEmcDetector* StEEmcDet = emcCol->detector(detId);
    StEmcDetector* StEEmcDetB = emcColB->detector(detId);

    if(StEEmcDet ==0) continue;
    if(StEEmcDetB==0) {
      cout << " colB has no data for above sub-detector"<<endl; 
      continue;
    }

    for(unsigned int sec = 1; sec <= StEEmcDet->numberOfModules(); sec++){
      StEmcModule* StEEmcMod =  StEEmcDet->module(sec);
      if(StEEmcMod == 0)  continue;

      StEmcModule* StEEmcModB =  StEEmcDetB->module(sec);
      cout << "Sector = " << sec  ;	
      
      assert(StEEmcModB);//tmp

      StSPtrVecEmcRawHit & EEmcRawHits = StEEmcMod->hits();
      StSPtrVecEmcRawHit & EEmcRawHitsB = StEEmcModB->hits();
      cout << " Number of HitsA = " << EEmcRawHits.size() << ", HitsB="<<EEmcRawHitsB.size()<<endl;

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
	  printf("\ti=%d ecolA:  Tower %2.2dT%c%2.2d: energy=%f adc=%d <--> %d=adc in ecolB\n",i,sec,sub+'A'-1,eta,e,adc, adcB);
	} else if(iDet == kEndcapEmcPreShowerId) {
	  int psub = (sub%5 == 0) ? 5:sub%5;
	  int pre = (sub-1)/5 + 1; 
	  printf("\ti=%d  ecolA: pre/post(%d) %2.2d%c%c%2.2d : energy=%f adc=%d<--> %d=adc in ecolB\n",i,pre,sec,pre+'P'-1,psub+'A'-1,eta,e,adc,adcB);
	}
	else 
	  printf("\tSMD-%c  %2.2d%c%3.3d : energy=%f  adc=%d <--> %d=adc in ecolB\n",
		 uv,sec,uv,eta,e,adc, adcB);
      }
    }
  }
}

