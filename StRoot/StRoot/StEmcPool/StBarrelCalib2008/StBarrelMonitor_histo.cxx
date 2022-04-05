// *-- Author : Jan Balewski
// 
// $Id: StBarrelMonitor_histo.cxx,v 1.2 2010/04/15 19:13:30 mattheww Exp $

#include <stdio.h>
#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include <TList.h>
#include <TLine.h>
#include <StMessMgr.h>
 
#include "StBarrelMonitorMaker.h"
//________________________________________________
//________________________________________________
void StBarrelMonitorMaker::initHistos() {

  memset(hTile,0,sizeof(hTile));
  initHistosTiles(kBTow); //int ibp
  initHistosTiles(kBPrs); //int ibp

  initAuxBprsHistos();
 
  LOG_INFO<<Form("initHistos done")<<endm;
}

//________________________________________________
//________________________________________________
void StBarrelMonitorMaker::initAuxBprsHistos(){
  
  char tt1[100], tt2[500];
  
  if(par_calibPass & kPassPedSub){
    int icr;
    for(icr=0;icr<mxBprsCrate;icr++) {
      sprintf(tt1,"bprsPedResCr%d",icr);
      sprintf(tt2,"bprs pedResidua, crate=%d; rawADC-ped ",icr);
      hBprsA[icr]=new TH1F(tt1,tt2,200,-50,50);
      HList->Add(  hBprsA[icr]); 
    }
  }

  hTonko0=new TH2D("bprsTnk0","entries per caps, all events; BPRS soft ID, capID",mxBtow,0.5,mxBtow+0.5,mxBcap,-0.5,mxBcap-0.5);
  hTonko1=new TH2D("bprsTnk1","rawAdc sum per cap , all events; BPRS soft ID, capID",mxBtow,0.5,mxBtow+0.5,mxBcap,-0.5,mxBcap-0.5);
  hTonko2=new TH2D("bprsTnk2","rawADc^2 sum per cap , all events; BPRS soft ID, capID",mxBtow,0.5,mxBtow+0.5,mxBcap,-0.5,mxBcap-0.5);
  HList->Add( hTonko0);
  HList->Add( hTonko1);
  HList->Add( hTonko2);
  
}
//________________________________________________
//________________________________________________
TH1F *StBarrelMonitorMaker::addBprsEveHisto( int *cap){
  
  char tt1[100], tt2[500];
  sprintf(tt1,"bprsRawAdc_eve%03d",nInpEve);
  sprintf(tt2,"bprs raw ADC, capsID:%d:%d:%d:%d: eve=%d; softID",cap[0],cap[1],cap[2],cap[3],nInpEve);
  //  printf("SAVE=%s=\n",tt2);
  TH1F *h=new TH1F(tt1,tt2,mxBtow,0.5,mxBtow+0.5);
  HList->Add(  h); 
  return h;
}

//________________________________________________
//________________________________________________
void StBarrelMonitorMaker::initHistosTiles(int ibp){
  
  char tt1[100], tt2[500];
  LOG_INFO<<Form("initHistos=%s",cTile4[ibp])<<endm;
  assert(ibp>=0 && ibp<mxBTile);

  int nb=200;
  float adc1=-50;
  if(!(par_calibPass & kPassPedSub)){
    adc1=0;
    if(ibp==kBPrs) adc1=100;
  }

  float adc2=adc1+nb;
  
  sprintf(tt1,"%s_c0",cTile4[ibp]);
  
  sprintf(tt2,"%s pedestal residua ; %s softID; rawADC-ped",cTile4[ibp],cTile4[ibp]);
  if(!(par_calibPass & kPassPedSub) ) sprintf(tt2,"%s rawADC ; %s softID; rawADC",cTile4[ibp],cTile4[ibp]);
  
  TH2F *h=new TH2F(tt1,tt2,mxBtow,0.5,mxBtow+0.5, nb,adc1,adc2);
  HList->Add(  h);
  hTile[ibp]=h;

  // init cap dependent histo for bprs, one set only
  if(ibp==kBPrs && par_bprsHisto==2) {
    sprintf(tt1,"bprs3D_c0");
     if(!(par_calibPass & kPassPedSub)) sprintf(tt2,"bprs rawADC 3D ; BPRS soft ID; rawADC; capID ");
     else 
       sprintf(tt2,"bprs ADC 3D ; BPRS soft ID; rawADC-capPed; capID ");
    hBprs3D=new TH3F(tt1,tt2,mxBtow,0.5,mxBtow+0.5, nb,adc1,adc2,mxBcap,-0.5,mxBcap-0.5);
     HList->Add(  hBprs3D); 
  }


}

#if 0
 Lx=h->GetListOfFunctions();    assert(Lx);
 ln=new TLine(par_isoMinT3x3adc,0,par_isoMinT3x3adc,100000);
 ln->SetLineColor(kRed);  Lx->Add(ln);
 ln=new TLine(par_isoMaxT3x3adc,0,par_isoMinT3x3adc,100000);
 ln->SetLineColor(kRed);  Lx->Add(ln);
 hC[6]=h;

#endif
