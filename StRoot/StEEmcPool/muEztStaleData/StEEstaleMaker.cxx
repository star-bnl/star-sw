// *-- Author : Jan Balewski
// 
// $Id: StEEstaleMaker.cxx,v 1.1 2004/11/02 14:37:11 balewski Exp $

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <StMessMgr.h>

#include "StEEstaleMaker.h"

#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "StEvent/StTriggerData2003.h"
#include "StEvent/StTriggerData2004.h"

#include "StMuDSTMaker/EZTREE/EztEventHeader.h"
#include "StMuDSTMaker/EZTREE/EztTrigBlob.h"
#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"

ClassImp(StEEstaleMaker)

//________________________________________________
//________________________________________________
StEEstaleMaker::StEEstaleMaker( const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  assert(mMuDstMaker);

  trgAkio=0;
  nInpEve=0;
  HList=0;
  timeFirst=0;
  
  //... clear past events
  int i;
  for (i=0;i<mxTkn;i++) {
    past[i].tw=0;
    past[i].token=i;
    past[i].n=0;
  }

}


//___________________ _____________________________
//________________________________________________
StEEstaleMaker::~StEEstaleMaker(){
  delete trgAkio;
}

//___________________ _____________________________
//________________________________________________
void StEEstaleMaker::saveHisto(TString fname){
  TString outName=fname+".hist.root";
  TFile f( outName,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),outName.Data());
  HList->Write();
  f.Close();

}
 
//________________________________________________
//________________________________________________
Int_t StEEstaleMaker::Init(){

  assert(HList);

  h[0]=new TH1F("tkn","Token frequency",4098,-0.5,4097.5);
  h[1]=new TH1F("twCh","Tower chi2 vs. events ID",5000,0.5,5000.5);
  h[2]=new TH1F("twCn","Tower channels with delAdc<10 vs. events ID",5000,0.5,5000.5);
  h[3]=( TH1F*) new TH2D("twChTm","Tower chi2/DOF vs. time ; time (minutes); chi2/DOF",400,0,80,20,0,10);
  int i;
  for(i=0;i<=3;i++)
    HList->Add(h[i]);

  return StMaker::Init();
}

//________________________________________________
//________________________________________________
Int_t StEEstaleMaker::Finish(){
  return kStOK;
}


//________________________________________________
//________________________________________________
Int_t StEEstaleMaker::Make(){
  nInpEve++;

  gMessMgr->Message("","D") <<GetName()<<"::Make() is called "<<endm;

  //..........  acuire EztHeader
  EztEventHeader* header= mMuDstMaker->muDst()->eztHeader();
  if(header==0) {
    gMessMgr->Message("","E") <<GetName()<<"::Make() no  EztEventHeader, skip event  "<<endm;
    return kStOK;
  }

  int token= header->getToken();
  if(timeFirst==0) timeFirst=header->getTimeStamp();
  h[0]->Fill(token);
  assert(token>=0 && token<mxTkn);
  EztEmcRawData* etow=mMuDstMaker->muDst()->eztETow();

  if(past[token].tw) { // calculate chi2
    EztEmcRawData* tw0=past[token].tw;
   
    float chi2r=getTwChi2(tw0,etow);
    float mnts=(header->getTimeStamp()-timeFirst)/60.;
    ((TH2D*) h[3])->Fill(mnts,chi2r);

    delete tw0;
    past[token].tw=0;    
      
  } 
    

  //add/replace events with this token
  past[token].tw=( EztEmcRawData* ) etow->Clone();   
  past[token].n++;

  return kStOK;

  
  //  header->print();

  // ............. acuire TRIGGER data 

  unpackTrigEzt();

  //..........  do the job


  return kStOK;
}


//________________________________________________
//________________________________________________
float StEEstaleMaker::getTwChi2(EztEmcRawData* tw0,EztEmcRawData* tw1){
  
 
  double sum=0;
  int nOk=0;
 
  int totChan=0;
  int ib;
  assert(tw0->getNBlocks()==tw1->getNBlocks());
  for(ib=0;ib<tw0->getNBlocks();ib++) {
    if( tw0->sizeHeader(ib)<=0) continue;
    if( tw1->sizeHeader(ib)<=0) continue;
    assert(tw0->sizeData(ib)==tw0->sizeData(ib));

    nOk++;
    //    const UShort_t* head=raw->header(ib); // not used here
    const UShort_t* data0=tw0->data(ib);
    const UShort_t* data1=tw1->data(ib);
    int nd=tw0->sizeData(ib);
    for(int chan=0;chan<nd;chan++) {
      float del=data0[chan]-data1[chan];
      if(fabs(del)>10) continue;
      sum+=del*del; 
      totChan++;
    }
  }
  
  h[1]->Fill(nInpEve,sum);
  h[2]->Fill(nInpEve,totChan);

  float chi2r=-1;
  if( totChan>0) chi2r=sum/totChan;
  
  return chi2r;

}


//________________________________________________
//________________________________________________
void StEEstaleMaker::unpackTrigEzt(){
  
  gMessMgr->Message("","D") <<GetName()<<"::unpackTrigEzt() is called "<<endm;

  EztTrigBlob * trigBlob=mMuDstMaker->muDst()->eztTrig(); 
  assert(trigBlob);
  // trigBlob->print(0);
 
  time_t  timeStamp=trigBlob->getTimeStamp();  
  //  printf("event time stamp=%d %s\n", (int)timeStamp, ctime((const time_t *)&timeStamp));
  
  const int timeStamp2003=1041397201; //==Wed Jan  1 00:00:01 2003
  const int timeStamp2004=1072933201; //==Thu Jan  1 00:00:01 2004
  const int timeStamp2005=1104555601; //==Sat Jan  1 00:00:01 2005


  delete trgAkio;// clear old event
  void *blob=trigBlob->trgd->GetArray();

  if( timeStamp>timeStamp2005) {
    printf("now TRigger decoder for 2005, STOP\n"); assert(1==2);
  } else if( timeStamp>timeStamp2004) {
    trgAkio=   new StTriggerData2004( (const TrgDataType2004 *)blob);
  } else if( timeStamp>timeStamp2003) {
    trgAkio=   new StTriggerData2003( (const TrgDataType2003 *)blob);
  } else {
    printf("now TRigger decoder before 2003, STOP\n"); assert(1==2);
  }

  //  trgAkio->dump();// lot of print out per eve
}


//---------------------------------------------------
// $Log: StEEstaleMaker.cxx,v $
// Revision 1.1  2004/11/02 14:37:11  balewski
// exampl eof stale data monitor
//
 
