// *-- Author : Jan Balewski
// 
// $Id: StEEsoloPi0Maker.cxx,v 1.5 2004/08/09 20:28:31 balewski Exp $

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
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StEvent/StTriggerId.h"


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
  
  static int n0=0,n1=0,n2=0,n3=0;
  printf("%s::Make() is called ..........n0,1,2,3= %d %d %d %d \n",StMaker::GetName(),n0,n1,n2,n3);

  n0++;

  clear();

  //............. trigger sort
  if( !getTrig()) return;
  n1++;


  //................. CTB sort
  float sum=getCtbSum();
  sum=sum;
  //tmp  if(sum<75 || sum > 800) return kStOK; 
  n2++;
    

 
  // ............. acuire EEMC data 
  if(getEEmcAdc()<0)   return kStOK;
  n3++;

  // print();

  findTowerClust();
  findTowerPi0();

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
Int_t StEEsoloPi0Maker::getEEmcAdc(){
  
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
    
    const EEmcDbItem *x=db->getTile(sec,'A'+sub-1,eta,'T');
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


//________________________________________________
//________________________________________________
bool StEEsoloPi0Maker::getTrig(){
  
  printf("%s::getTrig() is called ..........\n",StMaker::GetName());
 
  // Access to muDst .......................
  StMuEvent* muEve = mMuDstMaker->muDst()->event();
  int nPrim = mMuDstMaker->muDst()->primaryTracks()->GetEntries();  // get number of primary tracks
  StEventInfo &info=muEve->eventInfo();
  
  StMuTriggerIdCollection& trgIdColl=muEve->triggerIdCollection();

  const StTriggerId& oflTrgId=trgIdColl.nominal();

  
  vector<unsigned int> trgId=oflTrgId.triggerIds();

  printf("\n\n ==================== processing eventID %d nPrim=%d nTrig=%d==============\n", info.id(),nPrim, trgId.size());

  uint myId=15007;
  bool isGood=false;
  uint i;
  for(i = 0; i < trgId.size() ; i++){
    printf("i=%d id=%d\n",i,trgId[i]);
    if(trgId[i]==myId) isGood=true;
  }
  
  // StL0Trigger &trig=muEve->l0Trigger();
  
  
  //StEventSummary &smry=muEve->eventSummary();
  return isGood;
}

//________________________________________________
//________________________________________________
float StEEsoloPi0Maker::getCtbSum(){
  
  // printf("%s::getCtbSum() is called ..........\n",StMaker::GetName());
 
  // Access to muDst .......................
  StMuEvent* muEve = mMuDstMaker->muDst()->event();
  StCtbTriggerDetector* ctbDet = &(muEve->ctbTriggerDetector());
  
  assert(ctbDet);
  float ctbSum = 0;
  int nHit=0;
  for (uint slat = 0; slat < ctbDet->numberOfSlats(); slat++) {
    for (uint tray = 0; tray < ctbDet->numberOfTrays(); tray++) {
      float  adc = ctbDet->mips(tray,slat,0);
      ctbSum += adc;      
      if(adc > 5) nHit++;
    }
  }
  printf("CTB %d hit ADC>5  sumADC=%f (all) \n",nHit, ctbSum);
  hA[7]->Fill(ctbSum); 
  return ctbSum;
}

// $Log: StEEsoloPi0Maker.cxx,v $
// Revision 1.5  2004/08/09 20:28:31  balewski
// add trig selection
//
// Revision 1.4  2004/05/07 21:38:38  balewski
// gamma finder with SMD
//
// Revision 1.3  2004/04/29 04:16:43  perev
// typo fixed. getTile not getTail
//
// Revision 1.2  2004/04/14 19:34:01  balewski
// access to trigger data
//
// Revision 1.1  2004/04/14 17:09:09  balewski
// new copy of pi0finder with towers only, should work on ezTree as well (after small cleanup)
//
