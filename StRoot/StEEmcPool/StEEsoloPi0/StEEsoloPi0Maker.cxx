// *-- Author : Jan Balewski
// 
// $Id: StEEsoloPi0Maker.cxx,v 1.7 2004/08/26 04:39:40 balewski Exp $

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
  
  static int n0=0,n1=0,n2=0,n3=0;
  //  printf("%s::Make() is called ..........n0,1,2,3= %d %d %d %d \n",StMaker::GetName(),n0,n1,n2,n3);

  n0++;

  clear();

  //............. trigger sort
   if( !getTrig()) return kStOK;
  n1++;


  //................. CTB sort
  float sum=getCtbSum();
  sum=sum;
  //tmp  if(sum<75 || sum > 800) return kStOK; 
  n2++;
    


  // ............. acquire EEMC data 

  //if(getEEmcAdc()<0)   return kStOK;
  if(unpackMuDst()<0)   return kStOK;

  n3++;

  // print();

  findTowerClust();
  findTowerPi0();

  return kStOK;
}


#if 1 // old
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
    
    const EEmcDbItem *x=eeDb->getTile(sec,'A'+sub-1,eta,'T');
    if(x==0) continue; // it should never happened for muDst
    if(x->fail) continue; // ignore broken towers
    
    if(adc <x->thr) continue;// value must be >ped+ N*ped_sig
    float value=adc-x->ped;
    int ii=(x->sec-1)*60+(x->sub-'A')*12+x->eta-1;
    assert(ii>=0 && ii<EEsoloPi0::MxTw);
    //float recoEner=value/adc2gev[x->eta-1]/scaleFactor; // ideal
    
    if(x->gain<=0 ) continue; // gains not avaliable

    //    if(strstr(x->name,"01TA05")) continue;
    
    n1++;    
    float recoEner=value/x->gain/scaleFactor; // ideal
    totEner+=recoEner;
    soloMip[ii].e= recoEner;
    
    hA[0]->Fill(recoEner);
    printf("old ii=%d ene=%f\n",ii,recoEner);
    // printf("%d %d --> %f\n",iEta, iPhi,value)
    // printf("adc=%d value=%f %f %f recoEner=%f\n",adc,value,x->gain,scaleFactor,recoEner);
  }

  hA[1]->Fill(totEner);

  // printf("  Total %d towers with ADC>thr\n",n1);
  return n1;
}
#endif


//________________________________________________
//________________________________________________
bool StEEsoloPi0Maker::getTrig(){
  
  //  printf("%s::getTrig() is called ..........\n",StMaker::GetName());
 
  // Access to muDst .......................
  StMuEvent* muEve = mMuDstMaker->muDst()->event();
  //  int nPrim = mMuDstMaker->muDst()->primaryTracks()->GetEntries();  // get number of primary tracks
  // StEventInfo &info=muEve->eventInfo();
  
  StMuTriggerIdCollection& trgIdColl=muEve->triggerIdCollection();

  const StTriggerId& oflTrgId=trgIdColl.nominal();
  vector<unsigned int> trgId=oflTrgId.triggerIds();

  // printf("\n\n ==================== processing eventID %d nPrim=%d nTrig=%d==============\n", info.id(),nPrim, trgId.size());

  bool isGood=false;
  uint i;
  for(i = 0; i < trgId.size() ; i++){
    // printf("i=%d trgId=%d\n",i,trgId[i]);
    //.......... minB trig in pp200 in 2004
    if(trgId[i]==10) isGood=true;
    if(trgId[i]==45010) isGood=true;
    if(trgId[i]==45020) isGood=true;
  }
  
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
  // printf("CTB %d hit ADC>5  sumADC=%f (all) \n",nHit, ctbSum);
  hA[7]->Fill(ctbSum); 
  return ctbSum;
}


//________________________________________________
//________________________________________________
Int_t StEEsoloPi0Maker::unpackMuDst(){
  
  nInpEve++;
  gMessMgr->Debug() <<GetName()<<"::unpackMuDst() is called "<<endm;
  
  // Access to muDst .......................
  StMuEmcCollection* emc = mMuDstMaker->muDst()->emcCollection();
  if (!emc) {
    gMessMgr->Message("","W") <<"No EMC data for this event"<<endm;    return kStOK;
  }
    
  int i, n1=0,n2=0,n3=0;
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
    // if(adc>0) printf("adc=%f ene/GeV=%f rawAdc=%f idG=%f,hSec=%d %s\n",adc,ene,rawAdc,mfixEmTgain[ieta],sectID,x->name); 
    
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
    
    hA[0]->Fill(recoEner);

  }// end of loop over towers

  gMessMgr->Debug() <<GetName()<<"::unpackMuDst(),  found total" <<n1<<" towers with ADC>thr"<<endm;

#if 0 // not in use (yet)
  //.........................  P R E - P O S T .....................  
  int pNh= emc->getNEndcapPrsHits();
  for (i=0; i<pNh; i++) {
    int pre;
    int sec,eta,sub;
    //muDst  ranges: sec:1-12, sub:1-5, eta:1-12 ,pre:1-3==>pre1/pre2/post
    StMuEmcHit *hit=emc->getEndcapPrsHit(i,sec,sub,eta,pre);

    //tmp, for fasted analysis use only hits from sectors init in DB
    if(sec<eeDb->mfirstSecID || sec>eeDb->mlastSecID) continue;
 
    
    //Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method
    const EEmcDbItem *x=eeDb-> getTile(sec,sub-1+'A', eta, pre-1+'P'); 
    if(x==0) continue;
    if(x->fail ) continue; // drop broken channels
    
    // accept this hit
    int iphi=(x->sec-1)*MaxSubSec+(x->sub-'A');
    int ieta=x->eta-1;
    assert(iphi>=0 && iphi<MaxPhiBins);
    assert(ieta>=0 && ieta<MaxEtaBins);
    int iT=pre; // P,Q,R fall in to iT=1,2,3 <== there is no '+1' error, JB
    assert(iT>0 && iT<mxTile);
    
    float val=hit->getAdc();
    float adc=-100, rawAdc=-101, ene=-102;
  
    if(MCflag) {// val is Geant energy * const
      adc=val;
      rawAdc=adc+x->ped; // ped noise could be added
      ene=val/mfixPgain; // (GeV) Geant energy deposit 
    } else {
      rawAdc=val;
      adc=rawAdc-x->ped;
      if(x->gain) ene=adc/x->gain;
    }
    if(rawAdc>x->thr)  n2++;
    //if(adc>0) printf("adc=%f ene/GeV=%f  hSec=%d %s g=%f\n",adc,ene,sectID,x->name, x->gain); 
      
    tileAdc[iT][ieta][iphi]=adc;// // store P,Q,R depending on 'iT'
    tileThr[iT][ieta][iphi]=rawAdc>x->thr;
    
    if(x->gain<=0) continue;// drop channels w/o gains
    tileEne[iT][ieta][iphi]=ene;

  }
  

  //.......................  S M D ................................
  
  char uv='U';
  for(uv='U'; uv<='V'; uv++) {
    int sec,strip;
    int nh= emc->getNEndcapSmdHits(uv);
    //printf("pl=%c nH=%d  secLim=%d %d\n",uv,nh,eeDb->mfirstSecID,eeDb->mlastSecID);
    for (i=0; i<nh; i++) {
      StMuEmcHit *hit=emc->getEndcapSmdHit(uv,i,sec,strip);
      assert(sec>0 && sec<=MaxSectors);// total corruption of muDst
      
      //tmp, for fasted analysis use only hits from sectors init in DB
      if(sec<eeDb->mfirstSecID || sec>eeDb->mlastSecID) continue;
      
      const EEmcDbItem *x=eeDb->getByStrip(sec,uv,strip);
      assert(x); // it should never happened for muDst
      if(x->fail ) continue; // drop broken channels

      float val=hit->getAdc();
      float adc=-100, rawAdc=-101, ene=-102;

      // M-C & real data needs different handling
      if(MCflag) {
        adc=val;
        rawAdc=adc+x->ped; // ped noise could be added
        ene=val/mfixSMDgain; // (GeV) Geant energy deposit 
      }else {
        rawAdc=val;
        adc=rawAdc-x->ped;
        if(x->gain) ene=adc/x->gain;
      }
      if(rawAdc>x->thr)  n3++;
      //   if(adc>0)    printf("adc=%f ene/GeV=%f  hSec=%d %s\n",adc,ene,sectID,x->name); 

      smdAdc[x->plane-'U'][x->strip-1]=adc;
      if(x->gain<=0)continue; // drop channels w/o gains
      smdEne[x->plane-'U'][x->strip-1]=ene;
    }
  }
  //  printf("nTw=%d nPQR=%d,nSmd=%d\n",n1,n2,n3);

#endif  
  return n1;
}



// $Log: StEEsoloPi0Maker.cxx,v $
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
