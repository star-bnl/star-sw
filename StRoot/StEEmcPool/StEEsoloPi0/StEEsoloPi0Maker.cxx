// *-- Author : Jan Balewski
// 
// $Id: StEEsoloPi0Maker.cxx,v 1.1 2004/04/14 17:09:09 balewski Exp $

#include <TFile.h>
#include <TH2.h>

#include "StEEsoloPi0Maker.h"

#include "TChain.h"
#include "TClonesArray.h"
#include "StL0Trigger.h"
#include "StEventInfo.h"
#include "StEventSummary.h"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

//new
#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcDbMaker/StEEmcDbMaker.h"

ClassImp(StEEsoloPi0Maker)


//________________________________________________
//________________________________________________
StEEsoloPi0Maker::StEEsoloPi0Maker(const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  assert(mMuDstMaker);
  HList=0;
}


//___________________ _____________________________
//________________________________________________
StEEsoloPi0Maker::~StEEsoloPi0Maker(){
 // Save all objects in this file
  // hfile->Write();
}

 
//________________________________________________
//________________________________________________
Int_t StEEsoloPi0Maker::Init(){
  MYDB *db=(MYDB*)GetMaker("eemcDb");
  assert(db);
  init(db,HList);
 return StMaker::Init();
}

//________________________________________________
//________________________________________________
Int_t StEEsoloPi0Maker::Finish(){
  finish();
  return kStOK;
}
 
 
 

//________________________________________________
//________________________________________________
Int_t StEEsoloPi0Maker::Make(){
  
  static int kEve=0; 
  kEve++;

  clear();

  //  printf("%s::Make() is called ..........\n",StMaker::GetName());
 
  if(getAdc()<0)   return kStOK;
  
  // print();
  doEEsoloPi0();

#if 0  

// Access to muDst .......................
    StMuEvent* muEve = mMuDstMaker->muDst()->event();
    int nPrim = mMuDstMaker->muDst()->primaryTracks()->GetEntries();  // get number of primary tracks
    StEventInfo &info=muEve->eventInfo();
  printf("\n\n ====================%d  processing eventID %d nPrim=%d ==============\n", kEve,info.id(),nPrim);


  StMuDst* dst = mMuDstMaker->muDst();
  primTrA=dst->primaryTracks();
    
  StMuEvent* muEve = dst->event();
  StL0Trigger &trig=muEve->l0Trigger();
  StEventInfo &info=muEve->eventInfo();
  StEventSummary &smry=muEve->eventSummary();

  ctb->loadHits(muEve);
  StThreeVectorF ver=smry.primaryVertexPosition();
  if(runID>100) InitRunFromMake(info.runId());

  //  printf("trgWrd=%d nPrim=%d, Vz=%f \n", trig.triggerWord(),primTrA->GetEntries(),ver.z());
  // use only minB trigger events 
   if(trig.triggerWord()!=0x2000)  return kStOK;
  
  // reject events without primtracks
     if(primTrA->GetEntries()<=0)  return kStOK;
 
  // fill eve-related tree values
  eve_vz=ver.z(); 
     if(fabs(eve_vz)>C_maxZvertex) return kStOK;

  int bx7=trig.bunchCrossingId7bit(info.runId());
  eve_id=info.id();
  eve_bx48 = trig.bunchCrossingId();
  eve_bx120 =(trig.bunchCrossingId()+off48 )%120;
  eve_sb=trig.spinBits();
#endif  
  return kStOK;
}


//________________________________________________
//________________________________________________
Int_t StEEsoloPi0Maker::getAdc(){
  
  // printf("%s::getAdc() is called ..........\n",StMaker::GetName());

 
  // Access to muDst .......................
  StMuEmcCollection* emc = mMuDstMaker->muDst()->emcCollection();
  if (!emc) {
    printf(" No EMC data for this event\n");
    return kStOK;
  }
  
  // StMuEmcHit *hit;
  
  int i, n0=0,n1=0;
  float totEner=0;
  //printf("\nTotal %d hits in Tower \n",emc->getNEndcapTowerADC());
  int sec,eta,sub,adc;
  for (i=0; i< emc->getNEndcapTowerADC(); i++) {
    emc->getEndcapTowerADC(i,adc,sec,sub,eta);
    assert(sec>0);// total corruption of muDst
    n0++;
    //printf("i=%d  Tower %2.2dT%c%2.2d   adc=%4d\n",i,sec,sub+'A'-1,eta,adc );
    
    const EEmcDbItem *x=db->getTail(sec,'A'+sub-1,eta,'T');
    if(x==0) continue; // it should never happened for muDst
    
    if(adc <x->thr) continue;// value must be >ped+ N*ped_sig
    float value=adc-x->ped;
    int ii=(x->sec-1)*60+(x->sub-'A')*12+x->eta-1;
    assert(ii>=0 && ii<EEsoloPi0::MxTw);
    //float recoEner=value/adc2gev[x->eta-1]/scaleFactor; // ideal
    
    if(x->gain<=0 ) continue; // gains not avaliable

    if(strstr(x->name,"01TA05")) continue;
    if(strstr(x->name,"11TD09")) continue;
    if(strstr(x->name,"09TB06")) continue;

    n1++;    
    float recoEner=value/x->gain/scaleFactor; // ideal
    totEner+=recoEner;
    soloMip[ii].e= recoEner;
    
    hA[0]->Fill(recoEner);

    // printf("%d %d --> %f\n",iEta, iPhi,value)
    // printf("adc=%d value=%f %f %f recoEner=%f\n",adc,value,x->gain,scaleFactor,recoEner);
  }

  hA[1]->Fill(totEner);

  // printf("  Total %d towers with ADC>thr\n",n1);
  return n1;
}



// $Log: StEEsoloPi0Maker.cxx,v $
// Revision 1.1  2004/04/14 17:09:09  balewski
// new copy of pi0finder with towers only, should work on ezTree as well (after small cleanup)
//
