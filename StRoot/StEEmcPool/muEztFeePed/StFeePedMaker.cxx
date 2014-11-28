// *-- Author : Jan Balewski
// 
// $Id: StFeePedMaker.cxx,v 1.1 2005/01/11 22:14:31 balewski Exp $

#include <TFile.h>
#include <TH1.h>
#include <TF1.h>
#include <StMessMgr.h>

#include "StFeePedMaker.h"

#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "StMuDSTMaker/EZTREE/EztEventHeader.h"
#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"

ClassImp(StFeePedMaker)

//________________________________________________
//________________________________________________
StFeePedMaker::StFeePedMaker( const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  assert(mMuDstMaker);

  nInpEve=0;
  HList=0;
  
}


//___________________ _____________________________
//________________________________________________
StFeePedMaker::~StFeePedMaker(){
 
}

//___________________ _____________________________
//________________________________________________
void 
StFeePedMaker::saveHisto(TString fname){
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
StFeePedMaker::Init(){
  
  assert(HList);
  // init histo
  int i,j;
  
  for(i=0;i<MaxTwCrates;i++) {
    int crate=i+MinTwCrateID;
    for (j=0;j<MaxTwCrateCh;j++){
      char tt1[100], tt2[100];
      sprintf(tt1,"cr%d_ch%3.3d",crate,j);
      sprintf(tt2,"Pedestals for Crate %d and FEE ch %d",crate,j);
      TH1F* h=new TH1F(tt1,tt2,400,-0.5,395.5);
      HList->Add(h);
      int k=MaxTwCrateCh*i+j;
      // printf("%d %3d %s k=%d\n",i,j,h->GetName(),k);
      hped[k]=h;
    }
  }
  printf("Initialized %d tower histos\n",MxTwFeeCh);

  return StMaker::Init();
}

//________________________________________________
//________________________________________________
Int_t 
StFeePedMaker::Finish(){
  return kStOK;
}


//________________________________________________
//________________________________________________
Int_t 
StFeePedMaker::Make(){
  nInpEve++;

  gMessMgr->Message("","D") <<GetName()<<"::Make() is called "<<endm;

  //..........  acuire EztHeader
  EztEventHeader* header= mMuDstMaker->muDst()->eztHeader();
  if(header==0) {
    gMessMgr->Message("","E") <<GetName()<<"::Make() no  EztEventHeader, skip event  "<<endm;
    return kStOK;
  }

  int token= header->getToken();
  EztEmcRawData* eETow=mMuDstMaker->muDst()->eztETow();
  assert(eETow);

  // test for corruption, accept only healthy ETOW events
  int lenCount=0xa4;
  int errFlag=0;
  int trigComm=0x4; // physics, 9=laser/LED, 8=??
  
  int nOK=0;
  int icr;
  for(icr=0;icr<eETow->getNBlocks();icr++) {
    if(eETow->isCrateVoid(icr)) continue;
    if(eETow->purgeCrateOFF(icr)) continue;
    int crID=icr+1;
    eETow->tagHeadValid(icr,token, crID,lenCount,trigComm,errFlag);
    UShort_t isSick=eETow->getCorruption(icr);
    if(isSick) continue;
    nOK++;
  }

  //  printf(" # of healthy crates=%d\n",nOK);
  if(nOK!=MaxTwCrates)   return kStOK; // drop sick events
  //  eETow->print(1);

  //..............  Accumulate histograms ......

  for(icr=0;icr<eETow->getNBlocks();icr++) {
    if(eETow->isCrateVoid(icr)) continue;
    int i;
    const UShort_t* data=eETow->data(icr);
    for(i=0;i<eETow->sizeData(icr);i++) {
      int chan=i;
      if(chan>=MaxTwCrateCh) continue; // ignore not existing channels
      float adc=data[i];
      //if(adc<=0) continue;
      int k=icr*MaxTwCrateCh+chan;
      // printf("crID=%d ch=%d rawAdc=%.1f k=%d\n",icr+1,i,adc,k);
      assert(k>=0 && k<MxTwFeeCh);
      hped[k]->Fill(adc);
    } // channels
  } // blocks  
  
  return kStOK;
}


//________________________________________________
//________________________________________________
Int_t 
StFeePedMaker::fitPed(TH1F *h, int Xlow, int Xhigh) {
  int sumMin=100;
  if (h->GetEntries() < sumMin)  return -1;

  // localize pedestal peak in first 100 channels
  float *y=h->GetArray();

  float ym=0,sum=0;
  int i;
  int j=-1;
  for(i=2;i<=100;i++){ // skip first channel
    sum+=y[i];
    if(ym>y[i]) continue;
    ym=y[i];
    j=i;
  }

  if (sum < sumMin)  return -2;

  float xm=h->GetBinCenter(j);
  float x1=xm-Xlow;
  float x2=xm+Xhigh;

  // printf("fit ped to %s for x=[%.1f , .%.1f] \n",h->GetName(),x1,x2); 

  TF1*  fitF = new TF1("pedFun","gaus");
  fitF->SetLineColor(kGreen);;
  fitF->SetLineWidth(1);;
  h->Fit(fitF,"RQ+W","",x1,x2);
  //fitF->Print();
  return 0;
}


//---------------------------------------------------
// $Log: StFeePedMaker.cxx,v $
// Revision 1.1  2005/01/11 22:14:31  balewski
// start
//
