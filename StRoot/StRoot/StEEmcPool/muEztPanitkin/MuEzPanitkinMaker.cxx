// *-- Author : Rene Fatemi
// 
// $Id: MuEzPanitkinMaker.cxx,v 1.7 2009/12/03 22:35:03 ogrebeny Exp $

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <StMessMgr.h>

#include "MuEzPanitkinMaker.h"

#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "StMuDSTMaker/EZTREE/EztEventHeader.h"
#include "StMuDSTMaker/EZTREE/EztTrigBlob.h"
#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"
#include "StMuDSTMaker/EZTREE/StTriggerDataMother.h"

//tmp
#include "StTriggerData2005.h" // tmp

#include "EEqaSorter.h"
#include "StEEmcUtil/database/StEEmcDb.h"

#include "RawPixels.h"
ClassImp(MuEzPanitkinMaker)

//________________________________________________
//________________________________________________
MuEzPanitkinMaker::MuEzPanitkinMaker(const Char_t *self, const Char_t *muDstMakerName) : StMaker(self){
    mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
   assert(mMuDstMaker);

  trgAkio=0;
  nTrigEve=nInpEve=0;
  HList=0;
  qaSort=0;
  rawPixels=0;
  trigID=0;
  SetHistoPixels(false);
  SetSpy(false);
}


//___________________ _____________________________
//________________________________________________
MuEzPanitkinMaker::~MuEzPanitkinMaker(){
  delete trgAkio;
  delete qaSort;
  delete rawPixels;
}

//___________________ _____________________________
//________________________________________________
void MuEzPanitkinMaker::saveHisto(TString fname){
  TString outName=fname+".hist.root";
  TFile f( outName,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),outName.Data());
  HList->Write();
  f.Close();

}
 
//________________________________________________
//________________________________________________
Int_t MuEzPanitkinMaker::Init(){

  assert(HList);
  StEEmcDb *eeDb = (StEEmcDb*)this->GetDataSet("StEEmcDb");
  assert(eeDb);

  qaSort=new EEqaSorter(eeDb);
  qaSort->initHisto(HList, 1000, 1000); // nBins, maxAdc
  qaSort->initSpy(HList, 5, 1); // time constant/sec & mode,   //mode: 1=balewski@rcf, 2=eemc@evp,3=operator@evp 
  qaSort->setPath("StRoot/StEEmcPool/muEztPanitkin/","/star/u/balewski/0x/defaultPanitkinOut/");

  if(pixlesOn) {
    rawPixels=new RawPixels(HList,eeDb);
    rawPixels->setLimits(0,4095);// range for histos
    rawPixels->doRawAdc();// converions mode
    //rawPixels->doPedSub();// converions mode
    // rawPixels->doPedAndGain(); //
    
  }
  gMessMgr->Message("","I") <<GetName()<<"::Init() filter trigID="<<trigID<<endm;  
  return StMaker::Init();
}

//________________________________________________
//________________________________________________
Int_t MuEzPanitkinMaker::InitRun(int runNo){
  
  if(pixlesOn) rawPixels->initHisto(); // use DB-info to create histos
  qaSort->initRun();// DB must be initialized

  
  return kStOK;
}

//________________________________________________
//________________________________________________
Int_t MuEzPanitkinMaker::Finish(){
  gMessMgr->Message("","I") <<GetName()<<"::Finish() inputEve="<<nInpEve<<" trigFilterEve="<<nTrigEve<<endm;
  return kStOK;
}

//________________________________________________
//________________________________________________
void MuEzPanitkinMaker::Clear(const Option_t*){
  qaSort->clear();
  eHead=0;
  eETow=0;
  eESmd=0;
  eTrig=0;
  //  delete trgAkio; //JAN: perhaps is a memory leak? But crashes
}


//________________________________________________
//________________________________________________
Int_t MuEzPanitkinMaker::Make(){
  nInpEve++;
  gMessMgr->Message("","D") <<GetName()<<"::Make() is called "<<endm;
  
  //..........  acquire EztHeader
  eHead= mMuDstMaker->muDst()->eztHeader();
  if(eHead==0) {
    gMessMgr->Message("","E") <<GetName()<<"::Make() no  EztEventHeader, skip event  "<<endm;    return kStOK;
  }

  if(nInpEve==1) eHead->print();

  if(trigID ) {// filter by triggerID on demand
    if (! mMuDstMaker->muDst()->event()->triggerIdCollection().nominal().isTrigger(trigID)) return kStOK;
  }
  nTrigEve++;

  //.... get data .....
  eETow=mMuDstMaker->muDst()->eztETow();
  eESmd=mMuDstMaker->muDst()->eztESmd();
  eTrig=mMuDstMaker->muDst()->eztTrig(); 
  // printf("pp %p %p %p\n",eETow, eESmd, eTrig);

  // trgAkio=new StTriggerDataMother(eTrig);
  //trgAkio->dump();
  // eETow->print(1);
  // eESmd->print();
  // eHead->print();
  
  // assert(trgAkio->token()!=0) ; // I want to see zer-token events, JB

  StMuEvent *muEve = mMuDstMaker -> muDst() -> event();
  assert(muEve);
  StEventInfo &info=muEve->eventInfo();
  int runId=info.runId();


  // .... process adata ......
  void *blob=eTrig->trgd->GetArray();
  StTriggerData2005 trgAkio5x( (const TrgDataType2005 *)blob,runId);
  StTriggerData2005 *trgAkio5=&trgAkio5x;// this is ugly
  const unsigned char * dsm0inp= trgAkio5->getDsm0_EEMC();
  unsigned short int  * dsm1inp= trgAkio5->getDsm1_EEMC();
  unsigned short int  * dsm2inp= trgAkio5->getDsm2_EMC();
  unsigned short int  * dsm3inp= trgAkio5->getDsm3();

  //  test1(eETow,6);
  // .... process adata ......

  qaSort->sort(eETow,eESmd,eHead->getRunNumber(),
	       trgAkio5->token(),trgAkio5->version(), 
	       dsm0inp, dsm1inp, dsm2inp, dsm3inp);
  // it is assumed cratesOFF were removed by qaSort()

  if(pixlesOn) {
    rawPixels->sort(eETow);  
    rawPixels->sort(eESmd);
  }
  
  if(eeSpyOn) qaSort->spy(eETow, eESmd, eHead->getRunNumber(), eHead->getEventNumber());


  return kStOK;
} 

//---------------------------------------------------
// $Log: MuEzPanitkinMaker.cxx,v $
// Revision 1.7  2009/12/03 22:35:03  ogrebeny
// Fixed compiler warnings, mostly char* -> const char*
//
// Revision 1.6  2009/02/24 04:07:46  ogrebeny
// Fixed part of the trigger histograms
//
// Revision 1.5  2009/02/04 20:33:27  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.4  2006/09/15 01:45:36  balewski
// add run# to trg-data unpaker
//
// Revision 1.3  2005/07/15 15:37:36  balewski
// *** empty log message ***
//
// Revision 1.2  2005/05/05 22:22:08  balewski
// added spy for JP
//
// Revision 1.1  2005/04/28 20:54:46  balewski
// start
//
//
 
