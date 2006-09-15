// *-- Author : Jan Balewski
// 
// $Id: MuEzSoloPi0Maker.cxx,v 1.4 2006/09/15 01:45:31 balewski Exp $

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <StMessMgr.h>

#include "MuEzSoloPi0Maker.h"

#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "StMuDSTMaker/EZTREE/EztEventHeader.h"
#include "StMuDSTMaker/EZTREE/EztTrigBlob.h"
#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"
#include "StMuDSTMaker/EZTREE/StTriggerDataMother.h"
//tmp
#include "StTriggerData2005.h" // tmp

#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"

ClassImp(MuEzSoloPi0Maker)

//________________________________________________
//________________________________________________
MuEzSoloPi0Maker::MuEzSoloPi0Maker( const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  assert(mMuDstMaker);

  trgAkio=0;
  nAcceptEve=nTrigEve=nInpEve=nCorrEve=0;
  HList=0;
  trigID=0;
  maxCtbSum=0;

}


//___________________ _____________________________
//________________________________________________
MuEzSoloPi0Maker::~MuEzSoloPi0Maker(){
  delete trgAkio;
}

//___________________ _____________________________
//________________________________________________
void 
MuEzSoloPi0Maker::saveHisto(TString fname){
  TString outName=fname+".hist.root";
  TFile f( outName,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),outName.Data());

  HList->Write();
  f.Close();

}
 
//________________________________________________
//________________________________________________
Int_t 
MuEzSoloPi0Maker::Init(){

  assert(HList);
  eeDb=(StEEmcDbMaker*)GetMaker("eemcDb");
  assert(eeDb);  
  EEsoloPi0::init();
  
  gMessMgr->Message("","I") <<GetName()<<"::Init() filter trigID="<<trigID<<"  maxCtbSum="<<maxCtbSum<<endm;  
  return StMaker::Init();
}

//________________________________________________
//________________________________________________
Int_t 
MuEzSoloPi0Maker::InitRun(int runNo){
  static int first=1;
  if(first) return kStOK;
  initRun(runNo);
  first=0;
  return kStOK;
}

//________________________________________________
//________________________________________________
Int_t 
MuEzSoloPi0Maker::Finish(){
  finish();
  gMessMgr->Message("","I") <<GetName()<<"::Finish()\n    inputEve="<<nInpEve<<" trigFilterEve="<<nTrigEve<<" nCorrEve="<<nCorrEve<<" nAcceptEve="<<nAcceptEve<<endm;
  return kStOK;
}

//________________________________________________
//________________________________________________
void 
MuEzSoloPi0Maker::Clear(const Option_t*){
  eHead=0;
  eETow=0;
  eESmd=0;
  eTrig=0;
  //  delete trgAkio; //JAN: perhaps is a memory leak? But crashes
}



//________________________________________________
//________________________________________________
Int_t 
MuEzSoloPi0Maker::Make(){
  clear();
  nInpEve++;
  gMessMgr->Message("","D") <<GetName()<<"::Make() is called "<<endm;
  
  //..........  acquire EztHeader
  eHead= mMuDstMaker->muDst()->eztHeader();
  if(eHead==0) {
    gMessMgr->Message("","E") <<GetName()<<"::Make() no  EztEventHeader, skip event  "<<endm;    return kStOK;
  }


  if(trigID ) {// filter by triggerID on demand
    if (! mMuDstMaker->muDst()->event()->triggerIdCollection().nominal().isTrigger(trigID)) return kStOK;
  }
  nTrigEve++;
  //.... get data .....
  eETow=mMuDstMaker->muDst()->eztETow();
  // eESmd=mMuDstMaker->muDst()->eztESmd();
  eTrig=mMuDstMaker->muDst()->eztTrig(); 
  // printf("pp %p %p %p\n",eETow, eESmd, eTrig);
  
  //  trgAkio=new StTriggerDataMother(eTrig);
  // trgAkio->dump();
  // eETow->print();
  // eESmd->print();
  //  eHead->print();
  
  StMuEvent *muEve = mMuDstMaker -> muDst() -> event();
  assert(muEve);
  StEventInfo &info=muEve->eventInfo();
  int runId=info.runId();

  // .... process adata ......
  void *blob=eTrig->trgd->GetArray();
  StTriggerData2005 trgAkio5( (const TrgDataType2005 *)blob,runId);

  if(eETow->doTowerHeadCorruptionTest(trgAkio5.token())) {
    nCorrEve++;
    return kStOK;
  }
  
  int ctbSum=trgAkio5.ctbSum();
  if(maxCtbSum>0 && (ctbSum>maxCtbSum || ctbSum<maxCtbSum/2.))  return kStOK;
  nAcceptEve++;
  

  if(unpackMuEzTowers(trgAkio5.token())==false )  return kStOK;
  
  // printf(" ctbSum=%d \n",sum);
  hA[7]->Fill(ctbSum);
  
  findTowerClust();
  findTowerPi0();
  
  return kStOK;
} 

//________________________________________________
//________________________________________________
bool 
MuEzSoloPi0Maker::unpackMuEzTowers(int token){

  // tower
  if(eETow==0) return false; // no ETOW data 

  int icr;
  for(icr=0;icr<eETow->getNBlocks();icr++) {
    if(eETow->isCrateVoid(icr)) continue;
    assert(!eETow ->getCorruption(icr)); // zero-tolerance
    int crateID=eETow->getCrateID(icr);
    int chan;
    const UShort_t* data=eETow->data(icr);
    for(chan=0;chan<eETow->sizeData(icr);chan++) {
      const  EEmcDbItem  *x=eeDb->getByCrate(crateID,chan);
      if(x==0) continue; 
      if(x->fail ) continue; // drop broken channels
      float rawAdc=data[chan];  
      if(rawAdc<x->thr) continue; // drop small ADC values

      // accept this hit
      int iphi=(x->sec-1)*MaxSubSec+(x->sub-'A');
      int ieta=x->eta-1;
      assert(iphi>=0 && iphi<MaxPhiBins);
      assert(ieta>=0 && ieta<MaxEtaBins);
      int irad=iphi*MaxEtaBins+ieta; // unified spiral index
      assert(irad>=0 && irad<EEsoloPi0::MxTw);

      float adc=rawAdc-x->ped; 
      if(x->gain<=0) continue;
      float  ene=adc/x->gain;
      soloMip[irad].e= ene;
      // if(nTrigEve==1) printf("%s adc=%f ene=%f iphi=%d ieta=%d irad=%d\n",x->name,adc,ene,iphi,ieta,irad); 
    }
  }
  
  return true;
}

//---------------------------------------------------
// $Log: MuEzSoloPi0Maker.cxx,v $
// Revision 1.4  2006/09/15 01:45:31  balewski
// add run# to trg-data unpaker
//
// Revision 1.3  2005/03/11 15:39:49  balewski
// use corruption method from muEzt
//
// Revision 1.2  2005/03/01 20:02:15  balewski
// hack to access 2005 trigger data
//
// Revision 1.1  2005/02/05 04:56:31  balewski
// reads ezTree from muDst
//
//
  
