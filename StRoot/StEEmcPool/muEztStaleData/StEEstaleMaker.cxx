// *-- Author : Jan Balewski
// 
// $Id: StEEstaleMaker.cxx,v 1.4 2004/11/29 19:37:24 balewski Exp $

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <StMessMgr.h>

#include "StEEstaleMaker.h"

#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "StMuDSTMaker/EZTREE/EztEventHeader.h"
#include "StMuDSTMaker/EZTREE/EztTrigBlob.h"
#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"

ClassImp(StEEstaleMaker)

//________________________________________________
//________________________________________________
StEEstaleMaker::StEEstaleMaker( const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  assert(mMuDstMaker);

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
  assert(etow);

#if 0
  // test1(etow,0);
  test1(mMuDstMaker->muDst()->eztESmd(),0);
  header->print();
  // ............. acuire TRIGGER data 
  //..........  do the job
  return kStOK;
#endif


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
void StEEstaleMaker::test1(EztEmcRawData* tw, int flag){
  int nOk=0;
  int ib;
  for(ib=0;ib<tw->getNBlocks();ib++) {
    if( tw->sizeHeader(ib)<=0) continue;
    printf("ib=%d sizeH=%d sizeD=%d\n",ib,tw->sizeHeader(ib),tw->sizeData(ib));
    
    nOk++;
    const UShort_t* head=tw->header(ib); 
    int i;
    for(i=0;i<tw->sizeHeader(ib);i++) printf("  head[%d]=0x%04x ",i,head[i]);
    printf("\n");
    if(flag==0) continue;
    const UShort_t* data=tw->data(ib);
    int nd=tw->sizeData(ib);
    for(int chan=0;chan<nd;chan++) {
      if(chan>17) {printf(" .... etc ...\n"); break; }
      int adc=data[chan];
      printf("ib=%d ch=%d adc=%d\n",ib,chan,adc);
      if(chan>flag) break;
    }
  }
  
  return ;

}


//---------------------------------------------------
// $Log: StEEstaleMaker.cxx,v $
// Revision 1.4  2004/11/29 19:37:24  balewski
// fix to match EZTREE evolution
//
// Revision 1.3  2004/11/12 20:10:39  balewski
// *** empty log message ***
//
// Revision 1.2  2004/11/10 03:20:30  balewski
// trig fixed
//
// Revision 1.1  2004/11/02 14:37:11  balewski
// exampl eof stale data monitor
//
 
