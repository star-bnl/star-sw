// *-- Author : Jan Balewski
// 
// $Id: StEEsoloPi0Maker.cxx,v 1.8 2004/09/03 04:50:52 balewski Exp $

#include <TFile.h>

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

#include <StMessMgr.h>

ClassImp(StEEsoloPi0Maker)

//________________________________________________
//________________________________________________
StEEsoloPi0Maker::StEEsoloPi0Maker(const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  assert(mMuDstMaker);
  MCflag=0;// default off
   // towers are gain matched to fixed E_T
  float maxAdc=4095;
  float maxEtot=60;  // in GeV
  const float feta[MaxEtaBins]= {1.95,1.855,1.765,1.675,1.59,1.51,1.435,1.365,1.3,1.235,1.17,1.115};

  int i;

  mfixEmTgain=new float [MaxEtaBins];
  for (i=0;i<MaxEtaBins;i++) {
    mfixEmTgain[i]=maxAdc/maxEtot/cosh(feta[i]);
  }
  mfixSMDgain=23000;
  mfixPgain=23000;

}


//___________________ _____________________________
//________________________________________________
StEEsoloPi0Maker::~StEEsoloPi0Maker(){
 // Save all objects in this file
  // hfile->Write();
}

//________________________________________________
//________________________________________________
Int_t StEEsoloPi0Maker::InitRun(int runNo){
  initRun(runNo);
  return kStOK;
}
 
//________________________________________________
//________________________________________________
Int_t StEEsoloPi0Maker::Init(){
  eeDb=(EEDB*)GetMaker("eemcDb");
  EEsoloPi0::init();
  gMessMgr->Info() << GetName() << "has MCflag="<< MCflag<<endm;
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
  clear();  
  static int n0=0,n1=0,n2=0,n3=0;
  //  printf("%s::Make() is called ..........n0,1,2,3= %d %d %d %d \n",StMaker::GetName(),n0,n1,n2,n3);
  n0++;
  //............. trigger sort
  if( !unpackMuTrig()) return kStOK;
  n1++;

  //................. CTB sort
  float sum=getCtbSum();  sum=sum;
  //tmp  if(sum<75 || sum > 800) return kStOK; 
  n2++;
    
  // ............. acquire EEMC data 
  if(!unpackMuEemc())   return kStOK;
  n3++;

  findTowerClust();
  findTowerPi0();

  return kStOK;
}



//________________________________________________
//________________________________________________
bool StEEsoloPi0Maker::unpackMuTrig(){
  
  //  printf("%s::unpackMuTrig() is called ..........\n",StMaker::GetName());
 
  // Access to muDst .......................
  StMuEvent* muEve = mMuDstMaker->muDst()->event();
  //  int nPrim = mMuDstMaker->muDst()->primaryTracks()->GetEntries(); 
  
  StMuTriggerIdCollection& trgIdColl=muEve->triggerIdCollection();

  const StTriggerId& oflTrgId=trgIdColl.nominal();
  vector<unsigned int> trgId=oflTrgId.triggerIds();

  // printf("\n\n ==================== processing eventID %d nPrim=%d nTrig=%d==============\n", info.id(),nPrim, trgId.size());

  bool isGood=false;
  uint i;
  for(i = 0; i < trgId.size() ; i++){
    //    printf("i=%d trgId=%d\n",i,trgId[i]);
    //.......... minB trig in pp200 in 2004
    if(trgId[i]==10) isGood=true;
    if(trgId[i]==45010) isGood=true;
    if(trgId[i]==45020) isGood=true;
  }
  
#if 0 // TPC vertex, not used (yet)
  StEventInfo &info=muEve->eventInfo();
  StEventSummary &smry=muEve->eventSummary();
  StThreeVectorF ver=smry.primaryVertexPosition();

  b.zTpc=ver.z();
  if( fabs(ver.x())<0.000001 &&fabs(ver.y())<0.000001 &&fabs(ver.z())<0.000001 )  b.zTpc=999;
#endif

  return isGood;
}

//________________________________________________
//________________________________________________
bool StEEsoloPi0Maker::unpackMuEemc(){
  
  nInpEve++;
  gMessMgr->Debug() <<GetName()<<"::unpackMuDst() is called "<<endm;

  // Access to muDst .......................
  StMuEmcCollection* emc = mMuDstMaker->muDst()->emcCollection();
  if (!emc) {
    gMessMgr->Warning() <<"No EMC data for this event"<<endm;    return false;
  }

  int i, n1=0;
  //.........................  T O W E R S .....................
  for (i=0; i< emc->getNEndcapTowerADC(); i++) {
    int sec,eta,sub,val;
    //muDst  ranges:sec:1-12, sub:1-5, eta:1-12
    emc->getEndcapTowerADC(i,val,sec,sub,eta);
    assert(sec>0 && sec<=MaxSectors);// total corruption of muDst

    //tmp, for fasted analysis use only hits from sectors init in DB
    if(sec<eeDb->mfirstSecID || sec>eeDb->mlastSecID) continue;
 
    const EEmcDbItem *x=eeDb->getTile(sec,'A'+sub-1,eta,'T');
    assert(x); // it should never happened for muDst
    if(x->fail ) continue; // drop broken channels

    int iphi=(x->sec-1)*MaxSubSec+(x->sub-'A');
    int ieta=x->eta-1;
    assert(iphi>=0 && iphi<MaxPhiBins);
    assert(ieta>=0 && ieta<MaxEtaBins);
    int ispir=iphi*MaxEtaBins+ieta; // unified spiral index
    assert(ispir>=0 && ispir<EEsoloPi0::MxTw);

    float adc=-100, rawAdc=-101, ene=-102;
    
    if(MCflag) {  // M-C & real data needs different handling
      adc=val; //  this is stored in muDst
      rawAdc=adc+x->ped; // ped noise could be added
      ene=adc/ mfixEmTgain[ieta];
    } else {
      rawAdc=val;
      adc=rawAdc-x->ped;
      if(x->gain) ene=adc/x->gain;
    }

    int ii=(x->sec-1)*60+(x->sub-'A')*12+x->eta-1;
    assert(ii==ispir);
    //    if(adc>0)
    // printf("adc=%f ene/GeV=%f rawAdc=%f idG=%f,hSec=%d %s\n",adc,ene,rawAdc,mfixEmTgain[ieta],x->sec,x->name); 
    
    //aa int iT=0;// for towers   
    //aa tileAdc[iT][ieta][iphi]=adc;// store towers
    //aa tileThr[iT][ieta][iphi]=rawAdc>x->thr;
    if(rawAdc<x->thr) continue;
    n1++;
    if(x->gain<=0) continue;// drop channels w/o gains
    //aa tileEne[iT][ieta][iphi]=ene;
    float recoEner=ene/scaleFactor; // ideal
    // printf("new ii=%d ene=%f del=%f mcf=%d\n",ispir,recoEner,recoEner-soloMip[ispir].e,MCflag);
    soloMip[ispir].e= recoEner;
    
  }// end of loop over towers

  gMessMgr->Debug() <<GetName()<<"::unpackMuDst(),  found total" <<n1<<" towers with ADC>thr"<<endm;

  return true;
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
  // printf("CTB %d hit ADC>5  sumADC=%f (all) \n",nHit, ctbSum);
  hA[7]->Fill(ctbSum); 
  return ctbSum;
}




// $Log: StEEsoloPi0Maker.cxx,v $
// Revision 1.8  2004/09/03 04:50:52  balewski
// big clenup
//
// Revision 1.7  2004/08/26 04:39:40  balewski
// towards pi0
//
// Revision 1.6  2004/08/17 15:46:56  balewski
// clenup for pp200 in 2004
//
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
