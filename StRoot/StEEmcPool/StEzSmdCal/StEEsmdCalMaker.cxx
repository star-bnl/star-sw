// *-- Author : Jan Balewski
// 
// $Id: StEEsmdCalMaker.cxx,v 1.3 2004/06/29 16:37:41 balewski Exp $

#include <TFile.h>
#include <TH2.h>

#include "StEEsmdCalMaker.h"

#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include <StMessMgr.h>

#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcDbMaker/StEEmcDbMaker.h"


ClassImp(StEEsmdCalMaker)

//________________________________________________
//________________________________________________
StEEsmdCalMaker::StEEsmdCalMaker(const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  assert(mMuDstMaker);
  MCflag=0;// default off
 }


//___________________ _____________________________
//________________________________________________
StEEsmdCalMaker::~StEEsmdCalMaker(){
}
 
//________________________________________________
//________________________________________________
Int_t StEEsmdCalMaker::Init(){
  eeDb=(EEDB*)GetMaker("eemcDb");
  init();
  printf("%s has MCflag=%d\n",GetName(),MCflag);
  return StMaker::Init();
}

//________________________________________________
//________________________________________________
Int_t StEEsmdCalMaker::Finish(){
  finish();
  return kStOK;
}


//________________________________________________
//________________________________________________
Int_t StEEsmdCalMaker::Make(){
  
  clear();
  // ............. acuire EEMC data 
  if(unpackMuDst()<0)   return kStOK;

  findSectorMip();// do real analysis
 
  return kStOK;
}


//________________________________________________
//________________________________________________
Int_t StEEsmdCalMaker::unpackMuDst(){
  assert(strstr("Fix input arrays","only ezTree code is up to date,JB"));

  nInpEve++;
  gMessMgr->Message("","D") <<GetName()<<"::::getAdc() is called "<<endm;

  // Access to muDst .......................
  StMuEmcCollection* emc = mMuDstMaker->muDst()->emcCollection();
  if (!emc) {
    gMessMgr->Message("","W") <<"No EMC data for this event"<<endm;    return kStOK;
  }
    

  int i, n1=0,n2=0,n3=0;

  //.........................  T O W E R S .....................
  for (i=0; i< emc->getNEndcapTowerADC(); i++) {
    int sec,eta,sub,adc;
    //muDst  ranges:sec:1-12, sub:1-5, eta:1-12
    emc->getEndcapTowerADC(i,adc,sec,sub,eta);
    assert(sec>0 && sec<=MaxSectors);// total corruption of muDst

    //tmp, for fasted analysis use only hits from sectors init in DB
    if(sec<eeDb->mfirstSecID || sec>eeDb->mlastSecID) continue;
 
    const EEmcDbItem *x=eeDb->getTile(sec,'A'+sub-1,eta,'T');
    assert(x); // it should never happened for muDst
 
    float value=adc;
    
    // M-C & real data needs different handling
    if(MCflag) {
      ; // no action
    } else {
      if(x->fail ) continue; // drop broken channels
      value-=x->ped;
    }
    
    int iphi=(x->sec-1)*MaxSubSec+(x->sub-'A');
    int ieta=x->eta-1;
    assert(iphi>=0 && iphi<MaxPhiBins);
    assert(ieta>=0 && ieta<MaxEtaBins);
    int iT=0;// for towers

    tileAdc[iT][ieta][iphi]=value;// store towers
    n1++;

    //if(value>0)  printf(" %d %d %f\n",ieta,iphi,value);    
  }


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
    
    // accept this hit
    int iphi=(x->sec-1)*MaxSubSec+(x->sub-'A');
    int ieta=x->eta-1;
    assert(iphi>=0 && iphi<MaxPhiBins);
    assert(ieta>=0 && ieta<MaxEtaBins);
    
    float value=hit->getAdc();
    
    if(MCflag) {
      value/=1.;// to ~match M-C with with slopeGains1 for data 
    }else {
      if(x->fail ) continue; // drop broken channels
      value-=x->ped;
    }
    
    int iT=pre; // P,Q,R fall in to iT=1,2,3 <== there is no '+1' error, JB
    assert(iT>0 && iT<mxTile);
    tileAdc[iT][ieta][iphi]=value; // store P,Q,R depending on 'iT'
    n2++;
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
      // x->print();
      float value=hit->getAdc();
      assert(x->sec==15) ;// fix it

      // M-C & real data needs different handling
      if(MCflag) {
	value/=70;// to ~match M-C with with slopeGains1 for data 
      }else {
	if(x->fail ) continue; // drop broken channels
	if(x->gain<=0) continue;
	value-=x->ped;
	value/=x->gain;
      }
      //  printf("SMD hit sec=%d plane=%c strip=%d value=%f\n",x->sec,x->plane,x->strip,value);
      smdEne[x->plane-'U'][x->strip-1]=value;
      n3++;
    }
  }
  
  //  printf("nTw=%d nPQR=%d,nSmd=%d\n",n1,n2,n3);
  
  return n1;
}


// $Log: StEEsmdCalMaker.cxx,v $
// Revision 1.3  2004/06/29 16:37:41  balewski
// towards SMD calib
//
// Revision 1.2  2004/06/22 23:31:11  balewski
// few more gadgets added
//
// Revision 1.1  2004/06/12 04:09:25  balewski
// start
//
// Revision 1.1  2004/06/06 04:54:08  balewski
// dual analyzis
//
