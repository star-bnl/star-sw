// $Id: EEsmdCalHisto.cxx,v 1.1 2004/06/12 04:09:21 balewski Exp $
 
#include <assert.h>
#include <stdlib.h>

#include <TClonesArray.h>
#include <TObjArray.h> 
#include <TH1.h> 
#include <TH2.h> 
#include <TFile.h> 

#include "EEsmdCal.h"

//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::initTileHist(char cut, char *title) {
  int iCut=cut -'a';
  assert(iCut>=0 && iCut<kCut);
  char tt1[100], tt2[500];

  char *cTile[kTile]={"Tower","Pres1","Pres2","Post"};
  char cT[kTile]={'T','P','Q','R'};

  int colA[kCut]={kBlack,kMagenta,kGreen,kRed,kBlue};

  int iT=0;
  for(iT=0;iT<kTile;iT++) {
    for(char iSub=0; iSub<MaxSubSec; iSub++){
      for(int iEta=0; iEta<MaxEtaBins; iEta++){
	char sub=iSub+'A';
	int eta=iEta+1;
	int iPhi=iSect*MaxSubSec+iSub;
	char core[100];
	sprintf(core,"%02d%c%c%02d",sectID,cT[iT],sub,eta);
	sprintf(tt1,"%c%s",cut,core);
	sprintf(tt2,"%s(%c) %s , %s; ADC-ped",cTile[iT],cut,core,title);
	//	printf("tt1=%s, tt2=%s\n",tt1,tt2);
	TH1F *h=new TH1F(tt1,tt2,220,-20,200.);
	h->SetLineColor(colA[iCut]);
	HList->Add(h);
	hT[iCut][iT][iEta][iPhi]=h;
      }
    }
  }
}


//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::initSmdHist(char cut, char *title) {
  int iCut=cut -'a';
  assert(iCut>=0 && iCut<kCut);
  char tt1[100], tt2[500];

  int colA[kCut]={kBlack,kMagenta,kBlue,kRed,kGreen};


  int iuv,istrip;
  for(iuv=0;iuv<MaxSmdPlains;iuv++) {
    for(istrip=0;istrip<MaxSmdStrips;istrip++) {
	char core[100];
	sprintf(core,"%02d%c%03d",sectID,iuv+'U',istrip+1);
	sprintf(tt1,"%c%sp",cut,core);
	sprintf(tt2,"SMD(%c) %s+1 , %s; ADC-ped/gain, pair sum",cut,core,title);
	//printf("tt1=%s, tt2=%s\n",tt1,tt2);
	TH1F *h=new TH1F(tt1,tt2,100,0.,5.);
	h->SetLineColor(colA[iCut]);
	HList->Add(h);
	hSp[iCut][iuv][istrip]=h;

	if(cut!='a') continue;
	// special case : single strip inclusive spectra
	sprintf(tt1,"%c%s",cut,core);
	sprintf(tt2,"SMD(%c) %s , %s; ADC-ped/gain",cut,core,title);
	//printf("tt1=%s, tt2=%s\n",tt1,tt2);
	h=new TH1F(tt1,tt2,100,0.,5.);
	h->SetLineColor(colA[iCut]);
	HList->Add(h);
	hSs[iuv][istrip]=h;
    }
  }

}


//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::initAuxHisto(){
  int i;
  float Emax=2;
  memset(hA,0,sizeof(hA));

  hA[0]=new TH1F ("tE","Eneregy (GeV) from any tower",100,0.,Emax); 
  hA[1]=new TH1F ("sE","Total  Eneregy in event (GeV) (sum from all tower)",200,0.,Emax*10); 
  hA[4]=new TH1F ("tN","No. of towers with energy above th1",30,-0.5,29.5);

  hA[9]=new TH1F ("myStat","type of events ",30,.5,30.5);

  // use 10-16 for SMD

  TH1F *h;
  TH2F *h2;
  char tt1[100], tt2[500];
 
  for(i=0;i<MaxSmdPlains;i++) {
    
    sprintf(tt1,"fr%02d%c",sectID,i+'U');
    sprintf(tt2,"MIP pattern match frequency SMD plane %02d%c; strip ID",sectID,i+'U');
    h=new TH1F(tt1,tt2,MaxSmdStrips,-0.5,MaxSmdStrips-0.5);
    hA[10+i]=h;
    
    sprintf(tt1,"mm%02d%c",sectID,i+'U');
    sprintf(tt2,"No. of MIP pattern matched/event,  SMD plane %02d%c",sectID,i+'U');
    h=new TH1F(tt1,tt2,20,-0.5,19.5);
    hA[12+i]=h;

  }

  //..................
  sprintf(tt1,"ep%02dUorV",sectID);
  sprintf(tt2,"MIP #DeltaEof 2-strips , SMD %02dUorV; strip ID; MIP #DeltaE ",sectID);
  h2=new TH2F(tt1,tt2,30,0,300,20,-.5,3.5);
  hA[20]=(TH1F*)h2;
  
  //..................
  sprintf(tt1,"xy%02d",sectID);
  sprintf(tt2,"Accepted MIP position , SMD plane %02dUorV; X(cm); Y(cm) ",sectID);
  h2=new TH2F(tt1,tt2,100,-40,160,100,-250,-50);
  hA[21]=(TH1F*)h2;
  
  sprintf(tt1,"xy%02dc",sectID);
  sprintf(tt2,"X/cm vs. Y/cm for acntral MIP's , SMD plane %02dU+V",sectID);
  h2=new TH2F(tt1,tt2,200,-40,160,200,-250,-50);
  hA[22]=(TH1F*)h2;
  
  //..................
  sprintf(tt1,"eq%02dUV",sectID);
  sprintf(tt2,"MIP #DeltaE from 4-strips, SMD %02dU+V; eta bin;  #DeltaE a.u.",sectID);
  h2=new TH2F(tt1,tt2,12,.5,12.5,20,-.5,7.5);
  hA[23]=(TH1F*)h2;
  

  // add histos to the list (if provided)
  if(HList) {
    for(i=0;i<32;i++) {
      if(hA[i]==0) continue;
      HList->Add(hA[i]);
    }  
  }

}

//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::fillTailHisto_a(){
  // Fill inclusive spectra for one sector 
  int iCut=0;
  
  int iT=0;
  for(iT=0;iT<kTile;iT++) {
    for(char iSub=0; iSub<MaxSubSec; iSub++){
      for(int iEta=0; iEta<MaxEtaBins; iEta++){
	int iPhi=iSect*MaxSubSec+iSub;
	TH1F *h=hT[iCut][iT][iEta][iPhi];
	h->Fill(tileAdc[iT][iEta][iPhi]);
      }
    }
  }
}

//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::fillTailHisto1(char cut, int iEta, int iPhi){
  // Fill inclusive spectra for one sector 
  int iCut=cut-'a';
  assert(iCut>=0 && iCut<kCut);

  if(!tileThr[1][iEta][iPhi]) return;// no data in ?

  int iT=0;
  for(iT=0;iT<kTile;iT++) {
    TH1F *h=hT[iCut][iT][iEta][iPhi];
    h->Fill(tileAdc[iT][iEta][iPhi]);
  }
}



//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::fillSmdHisto_a(){
  // Fill inclusive spectra for one sector 
  int iCut=0;

  int iuv,istrip;
  for(iuv=0;iuv<MaxSmdPlains;iuv++) {
    float *val=smdE[iSect][iuv];
    TH1F **hs=hSs[iuv]; // single strip spectra
    TH1F **hp=hSp[iCut][iuv]; // pairs of strip spectra
    // note, to loop over N-2 strips to get right the sum of pairs 
    for(istrip=0;istrip<MaxSmdStrips-1;istrip++) {
      hs[istrip]->Fill(val[istrip]);
      hp[istrip]->Fill(val[istrip]+val[istrip+1]);
    }
  }
}


//-------------------------------------------------
//-------------------------------------------------
void EEsmdCal::saveHisto(TString fname){
  TString outName=fname+".hist.root";
  TFile f( outName,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),outName.Data());
  HList->Write();
  f.Close();
}


