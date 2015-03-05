/*
 *
 * \class StFpsQaMaker
 *
 */

#include "StFpsQaMaker.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/StEvent/StFmsCollection.h"
#include "StRoot/StEvent/StFmsHit.h"
#include "StRoot/StFmsDbMaker/StFmsDbMaker.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TString.h"
#include "TFile.h"
#include "TCanvas.h"

#include <string.h>
#include <time.h>

StFpsQaMaker::StFpsQaMaker(const Char_t* name) : StMaker(name),mFmsDbMkr(0),mFmsCollectionPtr(0), mRun(0) {};

StFpsQaMaker::~StFpsQaMaker(){};

Int_t StFpsQaMaker::Init(){
  mFmsDbMkr = static_cast< StFmsDbMaker*>(GetMaker("fmsDb"));
  if(!mFmsDbMkr){
    LOG_FATAL << "Error finding StFmsDbMaker"<< endm;
    return kStFatal;
  }
  int yday=mRun/1000;
  sprintf(mFilename,"%d/%d.root",yday,mRun);
  printf("StFpsQaMaker::Init - Opening %s\n",mFilename);
  mFile=new TFile(mFilename,"RECREATE");
  char name[100];
  mDataSize[0] = new TH1F("TotalSize","TotalSize",100,-1.0,4.0);
  mDataSize[1] = new TH1F("DataSize","DataSize",100,-1.0,3.0);
  for(int i=0; i<mNPREPOST*2+1; i++){
    int x=i-mNPREPOST;
    sprintf(name,"Xing=%d",x);
    mXing[i] = new TH1F(name,name,100,0.0,300.0);
  }
  if(mPed==0){
    mAdc2[0] = new TH2F("Adc2", "Adc2", 252,0.0,252.0,64,0.0,4096.0);
    mAdc2[1] = new TH2F("Adc2z","Adc2z",252,0.0,252.0,50,0.0,200.0);
    for(int i=0; i<mNID; i++){
      sprintf(name,"ADC%03d",i);    
      mAdc[i][0]=new TH1F(name,name,128,0.0,4096.0);
      sprintf(name,"ADC%03dz",i);    
      mAdc[i][1]=new TH1F(name,name,150,0.0,300.0);
    }
  }else{
    mAdc2[0] = new TH2F("Adc2", "Adc2", 252,0.0,252.0, 100,64.0,4096.0);
    mAdc2[1] = new TH2F("Adc2z","Adc2z",252,0.0,252.0, 100,50.0,150.0);
    for(int i=0; i<mNID; i++){
      sprintf(name,"ADC%03d",i);    
      mAdc[i][0]=new TH1F(name,name,128,0.0,4096.0);
      sprintf(name,"ADC%03dz",i);    
      mAdc[i][1]=new TH1F(name,name,100,50.0,150.0);
    }
  }
  return kStOK;
};

Int_t StFpsQaMaker::Make() {
  StEvent* eventPtr=0;
  mFmsCollectionPtr=0;
  eventPtr= (StEvent*)GetInputDS("StEvent");  
  if(!eventPtr) { LOG_INFO << "No StEvent found" << endm;}
  else{ mFmsCollectionPtr=eventPtr->fmsCollection();} 
  if(!mFmsCollectionPtr){
    LOG_INFO << "No StFmsCollection found" << endm;
    return kStErr;
  }
  int nhit=mFmsCollectionPtr->numberOfHits();
  StSPtrVecFmsHit hits = mFmsCollectionPtr->hits(); 
  //printf("StFpsQaMaker found %d hits\n",nhit);
  int nfpsdata=0;
  int nfpsdatatot=0;
  for (unsigned int i=0; i<nhit; i++){
    int det = hits[i]->detectorId();
    if(det==15){
      nfpsdatatot++;
      int xing=hits[i]->tdc(); if(xing>65536/2) xing-=65536;
      int adc=hits[i]->adc();
      int qt=hits[i]->qtSlot();
      int ch=hits[i]->qtChannel();      
      int slatid = mFmsDbMkr->fpsSlatidFromQT(qt,ch);
      int q,l,s;
      mFmsDbMkr->fpsQLSfromSlatId(slatid,&q,&l,&s);       
      if(q>0 && l>0 && s>0 && abs(xing)<=mNPREPOST) {
	mXing[xing+mNPREPOST]->Fill((float)adc);
	if(xing==0){
	  nfpsdata++;
	  mAdc2[0]->Fill((float)slatid,(float)adc);
	  mAdc2[1]->Fill((float)slatid,(float)adc);
	  mAdc[slatid][0]->Fill((float)adc);
	  mAdc[slatid][1]->Fill((float)adc);
	}
      }
      //hits[i]->print();      
    }
  }
  mDataSize[0]->Fill(log10(nfpsdatatot));
  mDataSize[1]->Fill(log10(nfpsdata));
  //printf("NFMSHIT=%4d NFPSHITTOT=%4d NFPSHIT(xing=0)=%d\n",nhit,nfpsdatatot,nfpsdata); 
  return kStOK;
};

Int_t StFpsQaMaker::Finish(){
  /*
  mDataSize[0]->Write();
  mDataSize[1]->Write();
  for(int i=0; i<mNPREPOST*2+1; i++) mXing[i]->Write();
  mAdc2->Write();
  for(int i=0; i<mNID; i++) mAdc[i]->Write();
  */
  mFile->Write();
  mFile->Close();
  printf("StFpsQaMaker::Finish - Closing %s\n",mFilename);
  return kStOK;
};

ClassImp(StFpsQaMaker);

/*
 * $Id: StFpsQaMaker.cxx,v 1.2 2015/02/28 02:55:35 akio Exp $
 * $Log: StFpsQaMaker.cxx,v $
 * Revision 1.2  2015/02/28 02:55:35  akio
 * fix a bug
 *
 * Revision 1.1  2015/02/25 20:03:26  akio
 * new fps qa maker
 *
 */
