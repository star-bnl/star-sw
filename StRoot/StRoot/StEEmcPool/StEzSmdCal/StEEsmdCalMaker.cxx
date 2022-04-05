// *-- Author : Jan Balewski
// 
// $Id: StEEsmdCalMaker.cxx,v 1.7 2009/02/04 20:33:23 ogrebeny Exp $

#include <TFile.h>
#include <TH2.h>

#include "StEEsmdCalMaker.h"

#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include <StMessMgr.h>

#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/database/StEEmcDb.h"


ClassImp(StEEsmdCalMaker)

//________________________________________________
//________________________________________________
StEEsmdCalMaker::StEEsmdCalMaker( const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  assert(mMuDstMaker);
  MCflag=0;// default off
  //  printf("constr of calib =%s=\n",GetName());
 
  //tmp, JB
  // ideal calibration used by Fast simulator has eta dependent gains for towers and plain energy deposit for SMD,pre,post
  
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
StEEsmdCalMaker::~StEEsmdCalMaker(){
}

//________________________________________________
//________________________________________________
void StEEsmdCalMaker::SetSector(int sec){
  setSector(sec);
  TString name=GetName();
  name+="-";
  name+=sec;
  //  printf("change name to %s sec=%d\n", name.Data(),sec);
  SetName(name);
}

 
//________________________________________________
//________________________________________________
Int_t StEEsmdCalMaker::Init(){
  eeDb = (StEEmcDb*)this->GetDataSet("StEEmcDb");
  //printf("%s got eeDb=%p\n",GetName(),eeDb);
  EEsmdCal::init();
  printf("%s has MCflag=%d\n",GetName(),MCflag);
  return StMaker::Init();
}

//________________________________________________
//________________________________________________
Int_t StEEsmdCalMaker::InitRun(int runNo){
  if(runNo==0) {
    gMessMgr->Message("","W")<<GetName()<<"::InitRun("<<runNo<<") ??? changed to 555, it s OK for M-C - perhaps, JB"<<endm;
    runNo=555;
  }
  initRun(runNo);
  return kStOK;
}



//________________________________________________
//________________________________________________
Int_t StEEsmdCalMaker::Finish(){
  finish(0); // do not draw
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
  
  nInpEve++;
  gMessMgr->Message("","D") <<GetName()<<"::::getAdc() is called "<<endm;
  
  // Access to muDst .......................
  StMuEmcCollection* emc = mMuDstMaker->muDst()->muEmcCollection();
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
    if(sec<eeDb->getFirstSector() || sec>eeDb->getLastSector()) continue;
 
    const EEmcDbItem *x=eeDb->getTile(sec,'A'+sub-1,eta,'T');
    assert(x); // it should never happened for muDst
    if(x->fail ) continue; // drop broken channels

    int iphi=(x->sec-1)*MaxSubSec+(x->sub-'A');
    int ieta=x->eta-1;
    assert(iphi>=0 && iphi<MaxPhiBins);
    assert(ieta>=0 && ieta<MaxEtaBins);
 
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
    
    int iT=0;// for towers
    
    // if(adc>0) printf("adc=%f ene/GeV=%f rawAdc=%f idG=%f,hSec=%d %s\n",adc,ene,rawAdc,mfixEmTgain[ieta],sectID,x->name); 
   
    tileAdc[iT][ieta][iphi]=adc;// store towers
    tileThr[iT][ieta][iphi]=rawAdc>x->thr;
    if(rawAdc>x->thr)  n1++;
    if(x->gain<=0) continue;// drop channels w/o gains
    tileEne[iT][ieta][iphi]=ene;
    
  }


  //.........................  P R E - P O S T .....................  
  int pNh= emc->getNEndcapPrsHits();
  for (i=0; i<pNh; i++) {
    int pre;
    int sec,eta,sub;
    //muDst  ranges: sec:1-12, sub:1-5, eta:1-12 ,pre:1-3==>pre1/pre2/post
    StMuEmcHit *hit=emc->getEndcapPrsHit(i,sec,sub,eta,pre);

    //tmp, for fasted analysis use only hits from sectors init in DB
    if(sec<eeDb->getFirstSector() || sec>eeDb->getLastSector()) continue;
 
    
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
      if(sec<eeDb->getFirstSector() || sec>eeDb->getLastSector()) continue;
      
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
      //   if(adc>0)	printf("adc=%f ene/GeV=%f  hSec=%d %s\n",adc,ene,sectID,x->name); 

      smdAdc[x->plane-'U'][x->strip-1]=adc;
      if(x->gain<=0)continue; // drop channels w/o gains
      smdEne[x->plane-'U'][x->strip-1]=ene;
    }
  }
  //  printf("nTw=%d nPQR=%d,nSmd=%d\n",n1,n2,n3);
  
  return n1;
}


// $Log: StEEsmdCalMaker.cxx,v $
// Revision 1.7  2009/02/04 20:33:23  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.6  2005/03/11 15:44:25  balewski
// works with muEzt, cucu200
//
// Revision 1.5  2004/10/21 13:31:31  balewski
// to match new name of emcCollection in muDst
//
// Revision 1.4  2004/07/27 21:59:47  balewski
// now runs on muDst as well
//
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
