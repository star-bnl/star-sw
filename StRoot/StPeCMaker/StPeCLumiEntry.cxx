//////////////////////////////////////////////////////////////////////
//
// Revision 1.0  2001/2/15           Pablo Yepes: yepes@rice.edu
//
//////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include "StPeCLumiEntry.h"
#include "StEventUtilities/StuRefMult.hh"

ClassImp(StPeCLumiEntry)

StPeCLumiEntry::StPeCLumiEntry() {
  StPeCLumiEntry::Clear();  
}

void StPeCLumiEntry::Clear(const Option_t* t) {
  runNr=0;    
  eventNr=0 ; 
  uTime=0;    
  triggerMask=0;  
  triggerWord=0;  
  triggerActionWord=0;  
  zdcEast=0; 
  zdcWest=0; 
  zdcSum=0;
  zdcEastUA=0; 
  zdcWestUA=0; 
  zdcSumUA=0;

  ctbSum=0;  
  ctbSumMips=0;  
  nPrimary=0; 
  nGlobal=0; 
  nhminus=0; 
  xVtx=-9999;  
  yVtx=-9999; 
  zVtx=-9999; 
}



StPeCLumiEntry::~StPeCLumiEntry() {
}

Int_t StPeCLumiEntry::fill ( StMuDst *muDst) {
  StPeCLumiEntry::Clear();
  StMuEvent *MuEvent = muDst->event();
  if (MuEvent) {
   eventNr = MuEvent->eventNumber();
   runNr = MuEvent->runNumber();
   uTime = (int ) MuEvent->runInfo().productionTime(); 
   uTime-= 946681200;  // 1 January 2000
   // triggerMask = 
   triggerWord = MuEvent->l0Trigger().triggerWord();
   triggerActionWord = MuEvent->l0Trigger().triggerActionWord();

   nhminus = MuEvent->refMultNeg();

   // trigger information
   StZdcTriggerDetector* zdc=0;
   StCtbTriggerDetector* ctb=0;

   zdc = & MuEvent->zdcTriggerDetector();
   ctb = & MuEvent->ctbTriggerDetector();
  
   if(zdc){
     // attenuated 
     zdcWest = zdc->adcSum(west) ;
     zdcEast = zdc->adcSum(east) ;
     zdcSum  = zdc->adcSum() ;
     // unattenuated see StEvent Manual
     zdcWestUA = zdc->adc(0) ;
     zdcEastUA = zdc->adc(4) ;
     zdcSumUA  = zdcEastUA+zdcWestUA;    
   }
   // shorter  but more risky 
   // zdcEast= event->triggerDetectorCollection()->zdc().adcSum(east);
   // zdcWest= event->triggerDetectorCollection()->zdc().adcSum(west);
   // zdcSum = event->triggerDetectorCollection()->zdc().adcSum();
  
   ctbSum=0;
   ctbSumMips=0;
   if(ctb){
    for(UInt_t i=0; i < ctb->numberOfTrays(); i++){
      for(UInt_t j=0; j < ctb->numberOfSlats(); j++){
	ctbSum += ctb->mips(i,j,0);
	ctbSumMips += int(ctb->mips(i,j,0)/5);
      }
    }
   }
  

   nGlobal = muDst->numberOfGlobalTracks ();
   nPrimary = muDst->numberOfPrimaryTracks(); 

   StThreeVectorF vtx = MuEvent->primaryVertexPosition();

   xVtx = vtx.x();
   yVtx = vtx.y();
   zVtx = vtx.z();   

    // Test Stuff 
    cout << "utime       " << uTime<< endl;
    //   cout << "trigger mask" <<event->triggerMask()  << endl;  
    cout << "tw          " << triggerWord<< endl;  
    cout << "taw         " << triggerActionWord<< endl;  
    cout << "Primaries:  " << nPrimary << endl; 
    cout << "Globals  :  " << nGlobal << endl; 
    cout << "Uncorr hminus:" << nhminus << endl; 
    cout << "StPeCEvent : primary vertex " << xVtx << " " 
	<< yVtx << " " << zVtx << endl;
    cout << "LumiMaker: ZDC W:" <<zdcWest << " E: "<< zdcEast << " Sum " << zdcSum << endl;
  }
  return 0;
}


Int_t StPeCLumiEntry::fill ( StEvent *event) {

  StPeCLumiEntry::Clear();
  //   //  eventP = event ;
  
// Set Run and Event Number
  eventNr = event->id() ;
  //   runNr   = event->runId()*1000+filenumber; 
  runNr   = event->runId(); 
  uTime   = event->time();
  uTime-= 946681200;  // 1 January 2000
  triggerMask= event->triggerMask() ;  
  triggerWord=event->l0Trigger()->triggerWord() ;  
  triggerActionWord=event->l0Trigger()->triggerActionWord();  

  
  nhminus = uncorrectedNumberOfNegativePrimaries(*event);

  // trigger information
  StZdcTriggerDetector* zdc=0;
  StCtbTriggerDetector* ctb=0;
  StTriggerDetectorCollection* trg = event->triggerDetectorCollection();
  
  if ( trg ) {
     zdc = & trg->zdc();
     ctb = & trg->ctb();
   }
  if(zdc){
    // attenuated 
    zdcWest = zdc->adcSum(west) ;
    zdcEast = zdc->adcSum(east) ;
    zdcSum  = zdc->adcSum() ;
    // unattenuated see StEvent Manual
    zdcWestUA = zdc->adc(0) ;
    zdcEastUA = zdc->adc(4) ;
    zdcSumUA  = zdcEastUA+zdcWestUA;    
  }
  // shorter  but more risky 
  // zdcEast= event->triggerDetectorCollection()->zdc().adcSum(east);
  // zdcWest= event->triggerDetectorCollection()->zdc().adcSum(west);
  // zdcSum = event->triggerDetectorCollection()->zdc().adcSum();
  
  ctbSum=0;
  ctbSumMips=0;
  if(ctb){
    for(UInt_t i=0; i < ctb->numberOfTrays(); i++){
      for(UInt_t j=0; j < ctb->numberOfSlats(); j++){
	ctbSum += ctb->mips(i,j,0);
	ctbSumMips += int(ctb->mips(i,j,0)/5);
      }
    }
  }
  

  // Get the track nodes and count number of tracks 
   StSPtrVecTrackNode& exnode = event->trackNodes();
   for( UInt_t in=0; in<exnode.size(); in++ ) {
     nGlobal += exnode[in]->entries(global);
     nPrimary += exnode[in]->entries(primary);
   }
   

   StPrimaryVertex* vtx = event->primaryVertex();
   if(vtx) {
     xVtx = vtx->position().x();
     yVtx = vtx->position().y();
     zVtx = vtx->position().z();
   }


   // Test Stuff 
   cout << "utime       " << event->time() << endl;
   cout << "trigger mask" <<event->triggerMask()  << endl;  
   cout << "tw          " << event->l0Trigger()->triggerWord()  << endl;  
   cout << "taw         " << event->l0Trigger()->triggerActionWord() << endl;  
   cout << "Primaries:  " << nPrimary << endl; 
   cout << "Globals  :  " << nGlobal << endl; 
   cout << "Uncorr hminus:" << nhminus << endl; 
   cout << "StPeCEvent : primary vertex " << xVtx << " " 
	<< yVtx << " " << zVtx << endl;
   cout << "LumiMaker: ZDC W:" <<zdcWest << " E: "<< zdcEast << " Sum " << zdcSum << endl;
   
   
   return 0 ;
}

