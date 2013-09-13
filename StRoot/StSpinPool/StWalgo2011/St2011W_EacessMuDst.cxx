// $Id: St2011W_EacessMuDst.cxx,v 1.7 2013/09/13 19:33:13 stevens4 Exp $
//
//*-- Author : Jan Balewski, MIT
//*-- Author for Endcap: Justin Stevens, IUCF

//MuDst
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuTriggerIdCollection.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuTrack.h>
#include <StMuDSTMaker/COMMON/StMuPrimaryVertex.h>

#include "StEEmcUtil/database/StEEmcDb.h"   
#include "StEEmcUtil/database/EEmcDbItem.h"  

#include "St2011WMaker.h"

//--------------------------------------
//--------------------------------------
int  
St2011WMaker::accessEndcapTrig(){ // return non-zero on abort 
  if (isMC){
    if(wEve->etow.maxAdc<10./60.*4096) return -1; //L2 is HT
    hE[0]->Fill("L2ewET",1.);
    wEve->l2EbitET=true;
    return 0; 
  }
  
  StMuEvent* muEve = mMuDstMaker->muDst()->event();
  StMuTriggerIdCollection *tic=&(muEve->triggerIdCollection());
  assert(tic);
  const StTriggerId &l1=tic->l1();
  vector<unsigned int> idL=l1.triggerIds();

  //printf("nTrig=%d, trigID: ",idL.size());
  for(unsigned int i=0;i<idL.size(); i++){
    char txt[100];
    sprintf(txt,"%d",idL[i]);
    hE[1]->Fill(txt,1.);
  }
  
  //check trigger ID
  if(!tic->nominal().isTrigger(parE_l2ewTrgID)) return -2;
  hE[0]->Fill("L2ewId",1.);
  
  //need to get offset for 2011 run for EEMC, done, hacked by Jan
  struct  L2weResult2011 { 
    unsigned char  trigger;     // bit0=rnd, bit1=ET>thr 
    unsigned char  highestEt;   // cluster Et with 60Gev Max.  bits=Et*256/60
    unsigned short highestRDO;
  };

  TArrayI& l2Array = muEve->L2Result();
  LOG_DEBUG <<Form("AccessL2Decision() from regular muDst: L2Ar-size=%d",l2Array.GetSize())<<endm;
  unsigned int *l2res=(unsigned int *)l2Array.GetArray();
  const int EEMCW_off=35; // valid only for 2011 run

  L2weResult2011 *l2algo= ( L2weResult2011 *) &l2res[EEMCW_off];
  wEve->l2EbitET=(l2algo->trigger&2)>0;  // bit1=ET>thr
  wEve->l2EbitRnd=(l2algo->trigger&1)>0; // bit0=rnd,

#if 0
  if(l2algo->trigger==0) return -3;
  printf(" L2-jet online results below:\n");
  for (int k=0;k<64;k++) 
    if(l2res[k]) printf("k=%2d  val=0x%04x\n",k,l2res[k]);
  printf("L2WE_Result 4-bytes: trg bitET=%d,  bitRnd=%d, highets:  ET/GeV=%.2f,  RDO=%d  hex=0x%08x\n",wEve->l2EbitET,wEve->l2EbitRnd,l2algo->highestEt/256.*60,l2algo->highestRDO,l2res[EEMCW_off]);
#endif
  
  //  hack to make the code work also for run 9 and early run 12
  if (mRunNo<11000111 || mRunNo>13000000) {  
    wEve->l2EbitET=1;
    wEve->l2EbitRnd=1;
  }

  if( (wEve->l2EbitRnd || wEve->l2EbitET)==0) return -3; // L2W-algo did not accept this event
  hE[0]->Fill("L2ewBits",1.); // confirmation bits were set properly
  
  if(wEve->l2EbitRnd) {
    hE[0]->Fill("L2ewRnd",1.);
    for (int m=0;m<90;m++){
      int val=muEve->emcTriggerDetector().highTowerEndcap(m);
      hE[7]->Fill(val);
    }
    hE[61]->Fill(wEve->bx7);
  }
  
  if(!wEve->l2EbitET)  return -3; // drop L2W-random accepts
  if(wEve->l2EbitET) hE[0]->Fill("L2ewET",1.);
    
  //.... only monitor below ....
  hE[2]->Fill(wEve->bx48);
  hE[3]->Fill(wEve->bx7);
  
  // access L0-HT data
  int mxVal=-1;
  for (int m=0;m<90;m++)	{
    int val=muEve->emcTriggerDetector().highTowerEndcap(m);
    if(mxVal<val) mxVal=val;
    if(wEve->l2EbitET) hE[6]->Fill(val);
    if(val<parE_DsmThres) continue;
    if(wEve->l2EbitET) hE[8]->Fill(m);
    //printf("Fired L0 EHT m=%d val=%d\n",m,val);
  }
  wEve->etow.maxHtDsm=mxVal;
  return 0;
}

//________________________________________________
//________________________________________________
int
St2011WMaker::accessETOW(){ 
  
  StMuEmcCollection* emc = mMuDstMaker->muDst()->muEmcCollection();
  if (!emc) {
    LOG_WARN <<"No EMC data for this event"<<endm;    return -4;
  } 
  
  wEve->etow.etowIn=1; //tag usable ETOW data
  const char *maxIdName=0;
  double maxADC=0,adcSum=0;
  int maxSec=-1,maxSub=-1,maxEta=-1;

  //loop over all towers
  for (int i=0; i< emc->getNEndcapTowerADC(); i++) {
    int sec,eta,sub,rawAdc; //muDst  ranges:sec:1-12, sub:1-5, eta:1-12
    emc->getEndcapTowerADC(i,rawAdc,sec,sub,eta);
    
    const EEmcDbItem *x=mDbE->getTile(sec,'A'+sub-1,eta,'T');
    assert(x); // it should never happened for muDst
    if(x->fail ) continue; // drop not working channels
    int isec=x->sec-1;
    int isub=x->sub-'A';
    int ieta=x->eta-1;
    
    assert(isec>=0 && isec<mxEtowSec); // check input is ok
    assert(isub>=0 && isub<mxEtowSub);
    assert(ieta>=0 && ieta<mxEtowEta);
    
    float adc=rawAdc-x->ped; // ped subtracted ADC
    if(adc<par_kSigPed*x->sigPed) continue;
  
    wEve->etow.adc[isec*mxEtowSub+isub][ieta]=adc;
    
    if(x->gain<=0) continue;// drop channels w/o gains
    float ene=adc/x->gain;
    
    //method for shifting energy scale 
    ene*=par_etowScale;//(default is par_etowScale=1)
    wEve->etow.ene[isec*mxEtowSub+isub][ieta]=ene;
    wEve->etow.stat[isec*mxEtowSub+isub][ieta]=0;

    if(maxADC<adc) { maxIdName=x->name; maxADC=adc; maxSec=isec; maxSub=isub; maxEta=ieta;}
    adcSum+=adc;
  
  }
  
  wEve->etow.maxAdc=maxADC;
  wEve->etow.maxSec=maxSec; wEve->etow.maxSub=maxSub; wEve->etow.maxEta=maxEta;
  hE[31]->Fill(maxADC);
  hE[32]->Fill(adcSum);

  if(maxADC<par_maxADC)  return -2 ;  // not enough energy

  return 0;
}

//________________________________________________
//________________________________________________
void
St2011WMaker::accessEPRS(){
  StMuEmcCollection* emc = mMuDstMaker->muDst()->muEmcCollection();
  if (!emc) {
    LOG_WARN <<"No EMC data for this event"<<endm;
  }

  int i ;
  int pNh= emc->getNEndcapPrsHits();
  for (i=0; i < pNh; i++) {
    int pre,sec,eta,sub; //muDst  ranges: sec:1-12, sub:1-5, eta:1-12 ,pre:1-3==>pre1/pre2/post

    StMuEmcHit *hit=emc->getEndcapPrsHit(i,sec,sub,eta,pre);
    float rawAdc=hit->getAdc();
    //Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method
    const EEmcDbItem *x=mDbE->getTile(sec,sub-1+'A',eta,pre-1+'P');
    assert(x); // it should never happened for muDst
    if(x->fail ) continue; // drop not working channels

    int isec=x->sec-1;
    int isub=x->sub-'A';
    int ieta=x->eta-1;
    int ipre=pre-1;
    int iphi=isec*mxEtowSub+isub;

    assert(isec>=0 && isec<mxEtowSec); // check input is ok
    assert(isub>=0 && isub<mxEtowSub);
    assert(ieta>=0 && ieta<mxEtowEta);

    float adc=rawAdc-x->ped; // ped subtracted ADC
    if(adc<par_kSigPed*x->sigPed) continue;

    wEve->eprs.adc[iphi][ieta][ipre]=adc;

    if(x->gain<=0) continue;// drop channels w/o gains

    wEve->eprs.ene[isec*mxEtowSub+isub][ieta][ipre]=adc/x->gain;
    wEve->eprs.stat[isec*mxEtowSub+isub][ieta][ipre]=0;

  }
}

//________________________________________________
//________________________________________________
void
St2011WMaker::accessESMD(){
  StMuEmcCollection* emc = mMuDstMaker->muDst()->muEmcCollection();
  if (!emc) {
    LOG_WARN <<"No EMC data for this event"<<endm;
  } 

  char uv='U';
  for(uv='U'; uv <='V'; uv++) {
    int sec,strip; 
    int nh= emc->getNEndcapSmdHits(uv);
    int i;
    for (i=0; i < nh; i++) {
      StMuEmcHit *hit=emc->getEndcapSmdHit(uv,i,sec,strip);
      float rawAdc=hit->getAdc();
      const EEmcDbItem *x=mDbE->getByStrip(sec,uv,strip);
      assert(x); // it should never happened for muDst

      if(x->fail ) continue;  // drop broken channels
      if(x->ped<0) continue;  // drop channels without peds

      float adc=rawAdc-x->ped; // ped subtracted ADC
      
      float sigPed=x->sigPed;
      //  x->print(); printf("adc=%f\n",adc);
      int isec=sec-1;
      int iuv=x->plane-'U';
      int istr=x->strip -1;

      assert(isec>=0 && isec<mxEtowSec);//never trust the input
      assert(iuv>=0 && iuv<mxEsmdPlane);
      assert(istr>=0 && istr<mxEsmdStrip);
      if(x->gain<=0)continue; // drop channels w/o gains
      if(adc<par_kSigPed*sigPed) continue; //drop noise

      wEve->esmd.adc[isec][iuv][istr]=adc;
      wEve->esmd.ene[isec][iuv][istr]=adc/x->gain;
    }
  }
}


//$Log: St2011W_EacessMuDst.cxx,v $
//Revision 1.7  2013/09/13 19:33:13  stevens4
//Updates to code for combined 2011+2012 result presented to spin PWG 9.12.13
//
//Revision 1.6  2012/07/12 20:49:21  balewski
//added spin info(star: bx48, bx7, spin4) and maxHtDSM & BTOW to Wtree
//removed dependence of spinSortingMaker from muDst
//Now Wtree can be spin-sorted w/o DB
//rdMu.C & readWtree.C macros modified
//tested so far on real data run 11
//lot of misc. code shuffling
//
//Revision 1.5  2012/06/26 20:30:23  stevens4
//Updates ZMaker for mixing barrel and endcap arms
//
//Revision 1.4  2012/06/18 18:28:00  stevens4
//Updates for Run 9+11+12 AL analysis
//
//Revision 1.3  2011/02/25 06:03:37  stevens4
//addes some histos and enabled running on MC
//
//Revision 1.2  2011/02/14 01:36:17  stevens4
//*** empty log message ***
//
//Revision 1.1  2011/02/10 20:33:22  balewski
//start
//
