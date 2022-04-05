// $Id: EEsmdCalHisto.cxx,v 1.18 2009/12/03 22:35:03 ogrebeny Exp $
 
#include <assert.h>
#include <stdlib.h>

#include <TClonesArray.h>
#include <TObjArray.h> 
#include <TH1.h> 
#include <TH2.h> 
#include <TLine.h> 
#include <TFile.h> 

#include "EEsmdCal.h"
#include "StEEmcUtil/database/EEmcDbItem.h"

#ifdef StRootFREE
  #include "EEmcDb/EEmcDb.h"
#else
  #include "StEEmcUtil/database/StEEmcDb.h"
#endif

//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::initTileHistoAdc(char cut, const char *title, int col) {
  int iCut=cut -'a';
  assert(iCut>=0 && iCut<kCut);
  char tt1[100], tt2[500];

  const char *cTile[mxTile]={"Tower","Pres1","Pres2","Post"};
  const char cT[mxTile]={'T','P','Q','R'};

  int iT=0;
  for(iT=0;iT<mxTile;iT++) {
    for(char iSub=0; iSub<MaxSubSec; iSub++){
      for(int iEta=0; iEta<MaxEtaBins; iEta++){
	char sub=iSub+'A';
	int eta=iEta+1;
	int iPhi=iSect*MaxSubSec+iSub;
	char core[100];
	sprintf(core,"%02d%c%c%02d",sectID,cT[iT],sub,eta);
	sprintf(tt1,"%c%s",cut,core);
	sprintf(tt2,"%s(%c) %s , %s; ADC-ped",cTile[iT],cut,core,title);
	//printf("tt1=%s, tt2=%s\n",tt1,tt2);
	TH1F *h;
	if(cT[iT]=='T') 
	  h=new TH1F(tt1,tt2,220,-20,200.);
	else 
	  h=new TH1F(tt1,tt2,400,-50,350.);
	h->SetLineColor(col);
	HList->Add(h);
	hT[iCut][iT][iEta][iPhi]=h;
      }
    }
  }
}




//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::initTileHistoEne(char cut, const char *title, int col) {
  int iCut=cut -'a';
  assert(iCut>=0 && iCut<kCut);
  char tt1[100], tt2[500];

  const char *cTile[mxTile]={"Tower","Pres1","Pres2","Post"};
  const char cT[mxTile]={'T','P','Q','R'};
  const char *cUnits[mxTile]={"(GeV)_EM", "(MEV)_MIP", "(MEV)_MIP", "(MEV)_MIP"};
  
  int iT=0;
  for(iT=0;iT<mxTile;iT++) {
    for(char iSub=0; iSub<MaxSubSec; iSub++){
      for(int iEta=0; iEta<MaxEtaBins; iEta++){
	char sub=iSub+'A';
	int eta=iEta+1;
	int iPhi=iSect*MaxSubSec+iSub;
	char core[100];
	sprintf(core,"%02d%c%c%02d",sectID,cT[iT],sub,eta);
	sprintf(tt1,"%c%s",cut,core);
	sprintf(tt2,"%s(%c) %s , %s; ADC-ped/gain %s",cTile[iT],cut,core,title,cUnits[iT]);
	//printf("tt1=%s, tt2=%s\n",tt1,tt2);
	TH1F *h=new TH1F(tt1,tt2,200,-0.5,9.5);
	h->SetLineColor(col);
	HList->Add(h);
	hT[iCut][iT][iEta][iPhi]=h;
      }
    }
  }
}



//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::addTwMipEbarsToHisto (int col, char mxC) {
  // search all existing tower histo (with '05T' in name) and add 
  // bars for MIP limits

  assert(dbMapped>0);

  char core[100];
  sprintf(core,"%02dT",sectID);
  float yMax=1000;
  TIterator *it=HList->MakeIterator();
  TH1 *h;
  while( (h=(TH1*) it->Next())) {
    const char *name=h->GetName();
    if(strstr(name,core)==0) continue;
    if(name[0]>mxC) continue; // to skip energy-like histos declared later
    //printf("%s\n",h->GetName());
    int iSub=name[4]-'A';
    int iEta=atoi(name+5)-1;
    int iPhi=iSect*MaxSubSec+iSub;
    const EEmcDbItem *x=dbT[kT][iEta][iPhi];
    assert(x);
    if(x->gain<=0) continue;
    TList *L=h->GetListOfFunctions();

    float adcC=towerMipE[iEta]*x->gain;
    if(name[0]>'e') adcC=towerMipE[iEta]; //ugly hack,JB, last two not ADC

    TLine *ln=new TLine(adcC,0,adcC,yMax);
    ln->SetLineColor(kRed); ln->SetLineStyle(2);
    L->Add(ln);

    float adc=adcC*twMipRelEneLow;
    ln=new TLine(adc,0,adc,yMax);
    ln->SetLineColor(col);
    L->Add(ln);

    adc=adcC*twMipRelEneHigh;
    ln=new TLine(adc,0,adc,yMax);
    ln->SetLineColor(col);
    L->Add(ln);
  }
}

//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::addPresMipEbarsToHisto (int col, char cT) {
  // search all existing tower histo (with '05X' in name) and add 
  // bars for MIP limits

  int iT=1+cT-'P';
  assert(iT>kT && iT<mxTile);
  assert(dbMapped>0);

  char core[100];
  sprintf(core,"%02d%c",sectID,cT);
  float yMax=1000;
  TIterator *it=HList->MakeIterator();
  TH1 *h;
  while( (h=(TH1*) it->Next())) {
    const char *name=h->GetName();
    if(strstr(name,core)==0) continue;
    // printf("%s\n",h->GetName());
    int iSub=name[4]-'A';
    int iEta=atoi(name+5)-1;
    int iPhi=iSect*MaxSubSec+iSub;
    const EEmcDbItem *x=dbT[iT][iEta][iPhi];
    assert(x);
    if(x->gain<=0) continue;
    
    float adcC=presMipE[iEta]*x->gain;
    if(name[0]>'e') adcC=presMipE[iEta]*1000; //ugly hack2,JB, last two not ADC

    TLine *ln=new TLine(adcC,0,adcC,yMax);
    ln->SetLineColor(col); ln->SetLineStyle(2);
    TList *L=h->GetListOfFunctions();
    L->Add(ln);
  }
}


//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::initSmdHist(char cut, const char *title, int col) {
  int iCut=cut -'a';
  assert(iCut>=0 && iCut<kCut);
  char tt1[100], tt2[500];

  int iuv,istrip;
  for(iuv=0;iuv<MaxSmdPlains;iuv++) {
    for(istrip=0;istrip<MaxSmdStrips;istrip++) {
	char core[100];
	// single strip (ADC-ped) spectra
	sprintf(core,"%02d%c%03d",sectID,iuv+'U',istrip+1);
	sprintf(tt1,"%c%s",cut,core);
	sprintf(tt2,"SMD(%c) %s , %s; ADC-ped",cut,core,title);
	//printf("tt1=%s, tt2=%s\n",tt1,tt2);
	TH1F *h=new TH1F(tt1,tt2,400,-50,350);
	//TH1F *h=new TH1F(tt1,tt2,4400,-400,4000);//tmp
	h->SetLineColor(col);

	HList->Add(h);
	hSs[iCut][iuv][istrip]=h;
    }
  }

}

//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::initSmdEneHist(char cut, const char *title, int col) {
  int iCut=cut -'a';
  assert(iCut>=0 && iCut<kCut);
  char tt1[100], tt2[500];

  int iuv,istrip;
  for(iuv=0;iuv<MaxSmdPlains;iuv++) {
    for(istrip=0;istrip<MaxSmdStrips;istrip++) {
	char core[100]; 
	sprintf(core,"%02d%c%03d",sectID,iuv+'U',istrip+1);
	sprintf(tt1,"%c%s",cut,core);
	sprintf(tt2,"SMD(%c) %s , %s; normal incident MIP energy (MeV)",cut,core,title);
	//printf("tt1=%s, tt2=%s\n",tt1,tt2);
	TH1F *h=new TH1F(tt1,tt2,200,-0.1,9.9);
	h->SetLineColor(col);

	HList->Add(h);
	hSs[iCut][iuv][istrip]=h;
    }
  }

}

//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::addSmdMipEbarsToHisto (int col, char cU) {
  // search all existing tower histo (with 'x05U' in name) and add 
  // bars for MIP limits
  
  assert(cU=='U' || cU=='V');
  assert(dbMapped>0);

  char core[100];
  sprintf(core,"%02d%c",sectID,cU);
  float yMax=1000;
  TIterator *it=HList->MakeIterator();
  TH1 *h;
  while( (h=(TH1*) it->Next())) {
    const char *name=h->GetName();
    if(strstr(name,core)!=name+1) continue;
    // printf("%s-%c\n",h->GetName(),h->GetName()[0]);
    //printf("%s\n",h->GetTitle());
    int iU=name[3]-'U';
    int iStr=atoi(name+4)-1;
    assert(iU>=0 && iU<MaxSmdPlains);
    assert(iStr>=0 && iStr<MaxSmdStrips);
    const EEmcDbItem *x=dbS[iU][iStr];
    //   printf("iU=%c iStr=%d\n",iU,iStr);
    if(x==0) continue;

    if((h->GetName()[0])=='a') {// one more hack
      char tt3[500];
      sprintf(tt3,"%s, tube=%s",h->GetTitle(),x->tube);
      h->SetTitle(tt3);
      // printf("%s %p %s\n",h->GetTitle(),h,h->GetName());
    }

    if(x->gain<=0) continue;
    TList *L=h->GetListOfFunctions();    
    float adcC=smdAvrMipE*x->gain;
    if((h->GetName()[0])>'b')  adcC=smdAvrMipE*1000.; // now in MeV

    TLine *ln=new TLine(adcC,0,adcC,yMax);
    ln->SetLineColor(col); ln->SetLineStyle(2);
    L->Add(ln);
    
    // printf("%s iU=%c iStr=%d adcC=%f\n",name,iU+'U',iStr,adcC);

  }
}


//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::histoGains(){

  //.................... SMD ................
  int iuv,istrip;
  for(iuv=0;iuv<MaxSmdPlains;iuv++) {
    for(istrip=0;istrip<MaxSmdStrips;istrip++) {
      const EEmcDbItem *x=dbS[iuv][istrip];
      if(x==0) continue;
      //if(x->fail) continue; // use any non-zero gain
      if(x->gain<=0) continue;
      hA[16+iuv]->Fill(x->strip,x->gain);
      // dig out MAPMT pixel
      const char *tube=x->tube+2;
      assert(tube[0]=='S');
      int box=atoi(tube+1);
      int pmt=atoi(tube+3);
      int pix=atoi(tube+6);
      int xx=(pmt-1)*16+pix;
      hA[5+box-1]->Fill(xx,x->gain);
      //  printf("%d %d %d %d", box,pmt,pix,xx); x->print();      assert(1==3);
    }
  }


  //.............. Pre/post
  int iT,iEta,iPhi;
  for(iT=kP;iT<=kR;iT++) 
    for(iEta=0;iEta<MaxEtaBins;iEta++)
      for(iPhi=0;iPhi<MaxPhiBins;iPhi++){
	const EEmcDbItem *x=dbT[iT][iEta][iPhi];
	if(x==0) continue;
	// if(x->fail) continue;  // use any non-zero gain
	if(x->gain<=0) continue;
	// dig out the MAPMT pixel
	assert(x->sec==sectID);
	const char *tube=x->tube+2;
	assert(tube[0]=='P');
	int pmt=atoi(tube+3);
	int pix=atoi(tube+6);
	int xx=(pmt-1)*16+pix;
	hA[5+3]->Fill(xx,x->gain);
      }


  //.............. Towers
  iT=0;
  for(iEta=0;iEta<MaxEtaBins;iEta++)
    for(iPhi=0;iPhi<MaxPhiBins;iPhi++){
      const EEmcDbItem *x=dbT[iT][iEta][iPhi];
      if(x==0) continue;	
      if(x->gain<=0) continue;
      assert(x->sec==sectID);
      int ispir=(x->eta-1)*MaxSubSec +  x->sub-'A';
      hA[4]->Fill(ispir,x->gain);
    }

}


//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::initAuxHisto(){
  int i;
  //  float Emax=2;
  memset(hA,0,sizeof(hA));

  char tt1[100], tt2[500], tt0[100]; 
  TH1F *h;
  TH2F *h2;

  // tower gains
  sprintf(tt1,"ug%02dT",sectID);
  sprintf(tt2,"used tower gains sec=%d ; x=spiral=(eta-1)*5+subs-A; gain [ch/GeV]",sectID);
  h=new TH1F(tt1,tt2, 60,-0.5,59.5);
  hA[4]=h;    

  for(i=0;i<4;i++) {
    // gains vs. MAPMT box/pmt/ch, histo 5,6,7,8    
    if(i<3) { 
      sprintf(tt0,"%02d_S%d",sectID,i+1);
    } else {
      sprintf(tt0,"%02d_P1",sectID);
    }
    sprintf(tt1,"ug%s",tt0);
    sprintf(tt2,"used gains MAPMT box %s ; chann=(pmt ID-1)*16+pix ; gain [ch/GeV]",tt0);

    h=new TH1F(tt1,tt2, MaxMapmtCrateCh,0.5, MaxMapmtCrateCh+0.5);
    hA[5+i]=h;    
  }

  sprintf(tt1,"my%02dStat",sectID);
  hA[9]=new TH1F (tt1,"type of events ",30,.5,30.5);

  for(i=0;i<MaxSmdPlains;i++) {    
    sprintf(tt1,"fr%02d%c",sectID,i+'U');
    sprintf(tt2,"freq. of MIP, UxV only, plane %02d%c; strip ID",sectID,i+'U');
    h=new TH1F(tt1,tt2,MaxSmdStrips,-0.5,MaxSmdStrips-0.5);
    hA[10+i]=h;
    
    sprintf(tt1,"mm%02d%c",sectID,i+'U');
    sprintf(tt2,"freq of 00xx00  pattern per plane %02d%c",sectID,i+'U');
    h=new TH1F(tt1,tt2,20,-0.5,19.5);
    hA[12+i]=h;

    sprintf(tt1,"fr%02d%cm",sectID,i+'U');
    sprintf(tt2,"freq. of best MIP, plane %02d%c; strip ID",sectID,i+'U');
    h=new TH1F(tt1,tt2,MaxSmdStrips,-0.5,MaxSmdStrips-0.5);
    hA[14+i]=h;
    
    sprintf(tt1,"ug%02d%c",sectID,i+'U');
    sprintf(tt2,"used gains for plane %02d%c; strip ID; gain [ch/GeV]",sectID,i+'U');
    h=new TH1F(tt1,tt2,MaxSmdStrips,0.5,MaxSmdStrips+0.5);
    hA[16+i]=h;    
  }

  //..................
  sprintf(tt1,"ep%02dUorV",sectID);
  sprintf(tt2,"#Sigma E of 2-strips, normal angle, best MIP , SMD %02dUorV; strip ID; MIP #Sigma E (MeV) ",sectID);
  h2=new TH2F(tt1,tt2,30,0,300,100,-.1,10.);
  hA[20]=(TH1F*)h2;
  
  //..................
  sprintf(tt1,"xy%02d",sectID);
  sprintf(tt2,"MIP position , UxV only, sect=%02d; X(cm); Y(cm) ",sectID);
  h2=new TH2F(tt1,tt2,500,-250,250,500,-250,250);
  hA[21]=(TH1F*)h2;
  
  sprintf(tt1,"xy%02dm",sectID);
  sprintf(tt2,"MIP position, best MIP, sect=%02d; X(cm); Y(cm) ",sectID);
  h2=new TH2F(tt1,tt2,250,-250,250,250,-250,250);
  hA[22]=(TH1F*)h2; 
   
  //..................
  sprintf(tt1,"eq%02dUV",sectID);
  sprintf(tt2,"#Sigma from 4-strips, best MIP, plane %02dU+V; eta bin;  #DeltaE MeV",sectID);
  h2=new TH2F(tt1,tt2,12,.5,12.5,50,-.1,7.5);
  hA[23]=(TH1F*)h2;

  //................
  sprintf(tt1,"ca%02d",sectID);
  sprintf(tt2,"# UxV candidates per tower, sector=%d; x=spiral=iPhi+60*iEta",sectID);
  int mxTw=MaxEtaBins*MaxPhiBins;
  hA[24]=new TH1F(tt1,tt2,mxTw,-0.5,mxTw-0.5);    
  

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
void EEsmdCal::fillOneTailHisto(char cut, int iEta, int iPhi){
  // Fill inclusive spectra for one sector 
  int iCut=cut-'a';
  assert(iCut>=0 && iCut<kCut);

  int iT=0;
  for(iT=0;iT<mxTile;iT++) {
    TH1F *h=hT[iCut][iT][iEta][iPhi];
    h->Fill(tileAdc[iT][iEta][iPhi]);
  }
}

//--------------------------------------------------
//--------------------------------------------------
void EEsmdCal::fillSmdHisto_a(){
  // Fill inclusive spectra for one sector 
  int iCut='a'-'a';

  int iuv,istrip;
  for(iuv=0;iuv<MaxSmdPlains;iuv++) {
    float *adc=smdAdc[iuv];
    TH1F **hs=hSs[iCut][iuv]; // single strip spectra
    // note, to loop over N-2 strips to get right the sum of pairs 
    for(istrip=0;istrip<MaxSmdStrips;istrip++) {
      hs[istrip]->Fill(adc[istrip]);
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
