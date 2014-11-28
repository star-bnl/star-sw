// *-- Author : Jan Balewski
// 
// $Id: StEEsoloPi0Maker.cxx,v 1.14 2011/04/11 19:35:42 fisyak Exp $

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


#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/database/StEEmcDb.h"

#include <StMessMgr.h>

ClassImp(StEEsoloPi0Maker)

//________________________________________________
//________________________________________________
StEEsoloPi0Maker::StEEsoloPi0Maker(const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  assert(mMuDstMaker);
  MCflag=0;// default off

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
  // call initRun() only once for M-C

  if( !MCflag) initRun(runNo);
  static int first=1;
  if(first) return kStOK;
  initRun(runNo);
  first=0;
  return kStOK;
}
 
//________________________________________________
//________________________________________________
Int_t StEEsoloPi0Maker::Init(){
  eeDb = (StEEmcDb*)this->GetDataSet("StEEmcDb");
  EEsoloPi0::init();
  LOG_INFO << "has MCflag="<< MCflag<<endm;
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
  // printf("%s::Make() is called ..........n0,1,2,3= %d %d %d %d \n",StMaker::GetName(),n0,n1,n2,n3);
  n0++;
  //............. trigger sort
  if( !MCflag&& !unpackMuTrig()) return kStOK;
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
bool StEEsoloPi0Maker::unpackMuTrig(){ // trigger filter, has wrongID
  
  //  printf("%s::unpackMuTrig() is called ..........\n",StMaker::GetName());
 
  // Access to muDst .......................
  StMuEvent* muEve = mMuDstMaker->muDst()->event();
  
  StMuTriggerIdCollection& trgIdColl=muEve->triggerIdCollection();

  const StTriggerId& oflTrgId=trgIdColl.nominal();
  vector<unsigned int> trgId=oflTrgId.triggerIds();

#if 0
  StEventInfo &info=muEve->eventInfo();
  int nPrim = mMuDstMaker->muDst()->primaryTracks()->GetEntries(); 
  printf("\n\n ==================== processing eventID %d nPrim=%d nTrig=%d==============\n", info.id(),nPrim, trgId.size());
#endif

  bool isGood=false;
  UInt_t i;
  for(i = 0; i < trgId.size() ; i++){
    //    printf("i=%d trgId=%d\n",i,trgId[i]);
#if 0
    //.......... minB trig in pp200 in 2004
    if(trgId[i]==10) isGood=true;
    if(trgId[i]==45010) isGood=true;
    if(trgId[i]==45020) isGood=true;
#endif 
    //.......... some trigs in ppTrans in 2006 
    if(trgId[i]==127641) isGood=true; //e-http-l2gam
    // if(trgId[i]==127652) isGood=true; //e-jp0-l2jet
    // if(trgId[i]==127551) isGood=true; //e-jp0
  }
  
#if 0 // TPC vertex, not used (yet)
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
  StMuEmcCollection* emc = mMuDstMaker->muDst()->muEmcCollection();
  if (!emc) {
    gMessMgr->Warning() <<"No EMC data for this event"<<endm;    return false;
  }

  int i, n1=0;
  //printf("aaa %d %d \n",eeDb->mfirstSecID,eeDb->mlastSecID);
  //.........................  T O W E R S .....................
  for (i=0; i< emc->getNEndcapTowerADC(); i++) {
    int sec,eta,sub,val;
    //muDst  ranges:sec:1-12, sub:1-5, eta:1-12
    emc->getEndcapTowerADC(i,val,sec,sub,eta);
    assert(sec>0 && sec<=MaxSectors);// total corruption of muDst

    //tmp, for fasted analysis use only hits from sectors init in DB
    if(sec<eeDb->getFirstSector() || sec>eeDb->getLastSector()) continue;
 
    const EEmcDbItem *x=eeDb->getTile(sec,'A'+sub-1,eta,'T');
    assert(x); // it should never happened for muDst

    if(x->fail ) continue; // drop broken channels

    int iphi=(x->sec-1)*MaxSubSec+(x->sub-'A');
    int ieta=x->eta-1;
    assert(iphi>=0 && iphi<MaxPhiBins);
    assert(ieta>=0 && ieta<MaxEtaBins);
    int irad=iphi*MaxEtaBins+ieta; // unified spiral index
    assert(irad>=0 && irad<EEsoloPi0::MxTw);

    float  ene=-102;
 
    float  rawAdc=val;
    if(rawAdc<x->thr) continue;
    float  adc=rawAdc-x->ped; 
    if(x->gain) ene=adc/x->gain;

    if(MCflag) ene/=0.8; //fug factor for sampling fraction in M-C being ~4% insted of assumed 5%

    int ii=(x->sec-1)*60+(x->sub-'A')*12+x->eta-1;
    assert(ii==irad);
    //    if(adc>0)
    // printf("adc=%f ene/GeV=%f rawAdc=%f idG=%f,hSec=%d %s\n",adc,ene,rawAdc,mfixEmTgain[ieta],x->sec,x->name); 
    
    //aa int iT=0;// for towers   
    //aa tileAdc[iT][ieta][iphi]=adc;// store towers
    //aa tileThr[iT][ieta][iphi]=rawAdc>x->thr;

    n1++;
    //aa tileEne[iT][ieta][iphi]=ene;
    float recoEner=ene/scaleFactor; // ideal
    // printf("new ii=%d ene=%f del=%f mcf=%d\n",idar,recoEner,recoEner-soloMip[irad].e,MCflag);
    soloMip[irad].e= recoEner;
    
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
  for (UInt_t slat = 0; slat < ctbDet->numberOfSlats(); slat++) {
    for (UInt_t tray = 0; tray < ctbDet->numberOfTrays(); tray++) {
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
// Revision 1.14  2011/04/11 19:35:42  fisyak
// Replace uint by UInt_t, use TMath
//
// Revision 1.13  2009/02/04 20:33:21  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.12  2007/10/19 23:18:42  balewski
// 2008 cleanup, now works only w/ regular muDst
//
// Revision 1.11  2004/10/27 18:07:50  balewski
// practical use of 'sim' flavor for M-C
//
// Revision 1.10  2004/10/21 13:31:25  balewski
// to match new name of emcCollection in muDst
//
// Revision 1.9  2004/09/29 18:04:44  balewski
// now it runs on M-C as well
//
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
