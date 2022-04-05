//*-- Author : J.Balewski, R.Fatemi
// 
// $Id: StEEmcTrigSimuMaker.cxx,v 1.8 2012/12/12 22:02:28 fisyak Exp $
// $Log: StEEmcTrigSimuMaker.cxx,v $
// Revision 1.8  2012/12/12 22:02:28  fisyak
// add sys/types.h include for APPLE
//
// Revision 1.7  2007/04/28 17:56:03  perev
// Redundant StChain.h removed
//
// Revision 1.6  2007/01/24 21:07:03  balewski
// 1) no cout or printf, only new Logger
// 2) EndcapMixer:
//    - no assert()
//    - locks out on first fatal error til the end of the job
//
// Revision 1.5  2003/09/02 17:57:55  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.4  2003/07/18 18:31:46  perev
// test for nonexistance of XXXReader added
//
// Revision 1.3  2003/04/30 20:36:37  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.2  2003/02/14 00:04:31  balewski
// remove few printouts
//
// Revision 1.1  2003/01/28 23:13:00  balewski
// star
//

#ifdef __APPLE__
#include <sys/types.h>
#endif
#include <StMessMgr.h>

#include "StEEmcTrigSimuMaker.h"
#include "St_DataSetIter.h"

#include <Stiostream.h>
#include <math.h>
#include "TFile.h"
#include "TArrayF.h"

#include "StMcEventMaker/StMcEventMaker.h"
#include "StMcEventTypes.hh"
#include "StEventTypes.h"
#include "StMcEvent.hh"
#include "StEvent.h"


ClassImp(StEEmcTrigSimuMaker)
StEEmcTrigSimuMaker::StEEmcTrigSimuMaker(const char *name):StMaker(name){
  //
}


StEEmcTrigSimuMaker::~StEEmcTrigSimuMaker(){
  //
}


//________________________________________________________

Int_t StEEmcTrigSimuMaker::Init(){
  // Create tables
  // Create Histograms    
   return StMaker::Init();
}

//____________________________________________________________
//____________________________________________________________
//____________________________________________________________
Int_t StEEmcTrigSimuMaker::Make(){

  StEvent* mEvent = (StEvent*)GetInputDS("StEvent");
  assert(mEvent); // fix your chain
  StEmcCollection* emcCollection = mEvent->emcCollection();
  StEmcDetector* twB = emcCollection->detector(kBarrelEmcTowerId);


  StEmcDetector* twE = emcCollection->detector(kEndcapEmcTowerId);
  if(twE==0) {
     LOG_WARN <<Form("%s found no E-EMC tower data in StEvent, skip event\n",GetName())<< endm;
    return kStOK;
  }


  int i;

  if(twB) {
    LOG_DEBUG<< Form(":: B_EMC Tower HITS ...\n");
    for ( i = 1; i <= (int)twB->numberOfModules(); i++) { // The B-EMC modules
      StSPtrVecEmcRawHit& emcTowerHits = twB->module(i)->hits();
      uint j;
      for ( j = 0; j < emcTowerHits.size(); j++) { 
	int adc= emcTowerHits[j]->adc();
	int mod= emcTowerHits[j]->module();
	int sub= emcTowerHits[j]->sub();
	int eta= emcTowerHits[j]->eta();
	float energy= emcTowerHits[j]->energy();
	LOG_DEBUG<< Form("j=%d, mod=%d, sub=%d, eta=%d adc=%d ener=%f\n",j,mod,sub,eta,adc,energy);
      }
    }
    LOG_DEBUG<<endm; // flush it now
  }

  if(twE) {
    LOG_DEBUG<< Form(":: E_EMC Tower HITS ... %d\n",twE->numberOfModules());
    for ( i = 0; i < (int)twE->numberOfModules(); i++) { // The E-EMC modules
      // printf("AAA %d\n",i);
      StEmcModule* stmod =   twE->module(i);
      if(stmod==0)	continue;
      StSPtrVecEmcRawHit& emcTowerHits = stmod->hits();
      uint j;
      for ( j = 0; j < emcTowerHits.size(); j++) { 
	//  printf("bbb=%d\n",j);
	int adc= emcTowerHits[j]->adc();
	int sec= emcTowerHits[j]->module()+1;
	int sub= emcTowerHits[j]->sub()+'A';
	int eta= emcTowerHits[j]->eta()+1;
	float energy= emcTowerHits[j]->energy();
	LOG_DEBUG<< Form("j=%d, sec=%d, sub=%c, eta=%d adc=%d ener=%f\n",j,sec,sub,eta,adc,energy);
      }
    } 
    LOG_DEBUG<<endm; // flush it now
  }
  
   printE();

  return kStOK;
}



//____________________________________________________________
//____________________________________________________________
//____________________________________________________________
void StEEmcTrigSimuMaker::printE(){

  StEvent* mEvent = (StEvent*)GetInputDS("StEvent");
  assert(mEvent); // fix your chain
  StEmcCollection* emcC= mEvent->emcCollection();
  
  assert(emcC);
  LOG_DEBUG<< Form("::printE() found EmcCollection\n")<<endm;
  TString str1;
  // aEEname[kEndcapEmcTowerId]="eeTower";
  //aEEname[kEndcapEmcTowerId]="eeTower";
  
  for(int det = kEndcapEmcTowerId; det<= kEndcapSmdVStripId; det++){
  
    StDetectorId id = StDetectorId(det);
    StEmcDetector* d = emcC->detector(id);
    if(d==0) {
      LOG_DEBUG<< Form("%s::printE() Found no sub-detector collection, skipping det=%d\n",GetName(),det)<<endm;
      continue;
    }

    str1="  i sec sub  ieta  energy  ADC";
    int nTot=0;
    switch (det){
    case kEndcapEmcTowerId:  
      LOG_DEBUG<< Form("Endcap Tower  hits \n")<<endm; break;
    case kEndcapEmcPreShowerId:  
      LOG_DEBUG<< Form("Endcap Preshower 1+2+post  hits \n")<<endm; break;
    case kEndcapSmdUStripId:
      LOG_DEBUG<< Form("Endcap SMD-U  hits \n")<<endm; 
      str1="  i sec strip  energy  ADC";
      break;
    case kEndcapSmdVStripId:
      LOG_DEBUG<< Form("Endcap SMD-V  hits \n")<<endm; 
      str1="  i sec strip  energy  ADC";
      break;
    default:
      assert(1==2); // your event is corrupted
    }
    
    if(d->numberOfModules() < 1)       continue;

    LOG_DEBUG<< Form("%s\n",str1.Data())<<endm;

    for(unsigned int isec=0; isec<d->numberOfModules(); isec++){
      int secID=isec+1;
      StEmcModule* stmod =  d->module(isec);
      if(stmod==0)	continue;

      StSPtrVecEmcRawHit & h = stmod->hits();
      for(unsigned int j=0; j<h.size() ;j++){
	nTot++;

	switch (det){
	case kEndcapEmcTowerId: 
	case kEndcapEmcPreShowerId:  
	  {  
	    //    printf("xxx secID=%d, id2=%d\n",secID,h[j]->module());
	    int sub='A'+h[j]->sub();
	    int keta=h[j]->eta()+1;
	    LOG_DEBUG<< Form("%3d  %2.2d   %c   %2d  %f %4d\n",nTot,secID,sub,keta,h[j]->energy(),h[j]->adc());
	  } break;
	case  kEndcapSmdUStripId:
	case  kEndcapSmdVStripId:
	  {  
	    int strip=h[j]->eta()+1;
	    LOG_DEBUG<< Form("%3d  %2.2d   %3d  %f %4d\n",nTot,secID,strip,h[j]->energy(),h[j]->adc());
	  } break;
	default:
	  assert(1==2); // your event is corrupted
	}// end of switch   
      }// loop over hits
      LOG_DEBUG<<endm; // flush it now 
    }// loop over sectors==modules
  }// looop over det
}






