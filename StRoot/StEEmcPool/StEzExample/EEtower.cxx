// $Id: EEtower.cxx,v 1.2 2004/06/08 17:10:41 balewski Exp $
 
#include <assert.h>
#include <stdlib.h>

#include <TClonesArray.h>
#include <TObjArray.h> 
#include <TH1.h> 
#include <TH2.h> 
#include <TFile.h> 

#include "EEtower.h"

ClassImp(EEtower)
//--------------------------------------------------
//--------------------------------------------------
EEtower::EEtower(){
  setThres(0.3,0);
  printf("EEtower() constructed\n");
  nInpEve=0; 
  HList=0; 
  eeDb=0;
}

//--------------------------------------------------
//--------------------------------------------------
EEtower::~EEtower() {/* noop */}


//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void EEtower::init( ){
  int i;
  float Emax=2;
  memset(hA,0,sizeof(hA));

  hA[0]=new TH1F ("tE","Eneregy (GeV) from any tower",100,0.,Emax); 
  hA[1]=new TH1F ("sE","Total  Eneregy in event (GeV) (sum from all tower)",200,0.,Emax*10); 
  hA[4]=new TH1F ("tN","No. of towers with energy above th1",30,-0.5,29.5);

  // add histos to the list (if provided)
  if(HList) {
    for(i=0;i<32;i++) {
      if(hA[i]==0) continue;
      HList->Add(hA[i]);
    }  
  }

}

//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void EEtower::clear(){ // called for every event
  memset(towerE,0,sizeof(towerE));
}

//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void EEtower::finish(){
  printf("\n  EEtower::finish() nInpEve=%d\n",nInpEve);
}

//-------------------------------------------------
//-------------------------------------------------
void EEtower::print(){
  printf("\n  EEtower::print()\n  dump event:\n");
  int i,j;
  printf("phiBin,  towerE for %d eta bins \n",MaxPhiBins);
  for(j=0;j<MaxPhiBins;j++) {
    printf("%4d: ",j);
    for(i=0;i<MaxEtaBins;i++) 
      printf("%8.3f ",towerE[i][j]);
     printf("\n");
  }
}


//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void EEtower::task1(){
  //  printf("\n  EEtower::task1()\n");
  int i,j;
  int nd=0;
  float totE=0;
  for(j=0;j<MaxPhiBins;j++) {
    for(i=0;i<MaxEtaBins;i++) {
      float e=towerE[i][j];
      totE+=e;
      if(e>th1) nd++;
      hA[0]->Fill(e);
    }
  }

  hA[1]->Fill(totE);
  hA[4]->Fill(nd);
}

  
//-------------------------------------------------
//-------------------------------------------------
void EEtower:: saveHisto(TString fname){
  TString outName=fname+".hist.root";
  TFile f( outName,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),outName.Data());
  HList->Write();
  f.Close();
}

