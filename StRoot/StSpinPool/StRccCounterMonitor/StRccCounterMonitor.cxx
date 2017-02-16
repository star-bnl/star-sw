#include <stdlib.h>

#include "StRccCounterMonitor.h"
#include "StMaker.h"
#include "StEventTypes.h"
#include "StEvent/StTriggerData.h"

#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TTree.h"

static const int NCRATE=14;
static const char* CRATE[NCRATE]={"L1","BC1","MXQ","MIX","BCW","BCE","FEQ",
				  "BBC","BBQ","FMS","QT1","QT2","QT3","QT4"};
static const int MAX=pow(2.0,16)*2.0;
static const int NBIN=pow(2.0,12);

unsigned int EVT;
unsigned int TCU;
unsigned int DIFF[NCRATE];

ClassImp(StRccCounterMonitor)

StRccCounterMonitor::StRccCounterMonitor(int run, const char *name):StMaker("rccqa",name){
  LOG_INFO << "Constructing StRccCounterMonitor with name=" << name << endm;
  mDebug=0;
  mRun=run;
  mEventCounter=0;
}

Int_t StRccCounterMonitor::Init(){
  int yearday=mRun/1000;
  sprintf(mFilename,"%d/rccqa.%d.root",yearday,mRun);
  mFile =  new TFile(mFilename, "RECREATE"); 
  if(mFile){
    cout << "Creating " << mFilename << endl;
  }else{
    cout << "Could not create " << mFilename << endl;
    return kStErr;
  }
  for(int i=0; i<NCRATE; i++){
    mHist[i] = new TH1F(CRATE[i],CRATE[i],NBIN,0.0,MAX);
  }
  mTree = new TTree("RCC","RCC");
  mTree->Branch("evt",&EVT,"evt/i");  
  mTree->Branch("tcu",&TCU,"tcu/i");  
  mTree->Branch("diff",&DIFF,"d[14]/i");  
  return kStOK;
}

int StRccCounterMonitor::Finish(){
  mFile->Write();
  mFile->Close();
  printf("StRccCounterMonitor::Finish - Closing %s\n",mFilename);
  return kStOK;
}


Int_t StRccCounterMonitor::Make(){
  cout << "StRccCounterMonitor Make() starting..."<<endl;
  mEventCounter ++ ;

  TObjectSet *os = (TObjectSet*)GetDataSet("StTriggerData");
  StTriggerData* trg;
  if (os) {
    trg = (StTriggerData*)os->GetObject();   
    if(trg){
      //cout << "StRccCounterMonitor got StTriggerData"<<endl;
    }else{
      cout << "StRccCounterMonitor could not get StTriggerData from DataSet."<<endl;
      return kStErr;
    }
  }else{
    cout << "StRccCounterMonitor could not get StTriggerData DataSet."<<endl;
    return kStErr;
  }
  
  EVT=trg->eventNumber();
  unsigned int tcu = TCU = trg->tcuCounter();
  if(mDebug==1) {
    static int first=0;    
    if(first==0){
      cout << "RCCMon        TCU ";
      for(int i=0; i<NCRATE; i++){
	cout << Form("%8s  ",CRATE[i]);
      } 
      cout << endl;
    }
    first=1;
    cout << Form("RCCMon %10d ",tcu);
  }
  for(int i=0; i<NCRATE; i++){
    unsigned int rcc = trg->rccCounter(i+1);
    unsigned int diff = rcc-tcu;
    //int diff = rcc-tcu;
    if(rcc==0){
      diff=0;
    }else if(rcc < tcu) {
      const long long one=1;
      const long long m=one<<32;
      long long r=rcc;
      long long t=tcu;
      diff=(unsigned int)(r+m-t);
      //cout << "-- OVER!!!--";
    }
    DIFF[i]=diff;
    if(mDebug>1) cout << Form("%3s=%10u  tcu=%10u diff=%10u\n",CRATE[i],rcc,tcu,diff);
    if(mDebug==1) cout << Form("%8u  ",diff);
    //if(mDebug==1 && (i==2 || i>=10) ) cout << Form("%8u  ",diff);
    mHist[i]->Fill(float(diff));
  }
  mTree->Fill();
  if(mDebug==1) cout << endl;
  return kStOK;
}
