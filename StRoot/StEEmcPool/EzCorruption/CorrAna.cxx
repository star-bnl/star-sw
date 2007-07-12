// $Id: CorrAna.cxx,v 1.3 2007/07/12 19:24:31 fisyak Exp $
 
#include <assert.h>
#include <stdlib.h>

#include <TClonesArray.h>
#include <TObjArray.h> 
#include <TH1.h> 
#include <TH2.h> 
#include <TFile.h> 
#include "TMath.h"
#include "CorrAna.h"


ClassImp(CorrAna)
//--------------------------------------------------
//--------------------------------------------------
CorrAna::CorrAna(){
  setThres(0.3,0);
  printf("CorrAna() constructed\n");
  nInpEve=0; 
  HList=0; 
  //  eeDb=0;
}

//--------------------------------------------------
//--------------------------------------------------
CorrAna::~CorrAna() {/* noop */}


//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void CorrAna::init( ){  
 
  printf("CorrAna() init\n");

  if (mode==1) {
    printf("CorrAna() init:loading Histograms\n");
    char title[10];
    char code[100];
    
    
    for (int i=0;i<MaxBTwCrate;i++){
      sprintf(title,"BEMC ADC for Crate %d",i+16);
      sprintf(code,"BadcCr%d",i+16);
      hBadc[i]=new TH2F(code,title,4096,0.0,4096.0,160,0.0,160);
      sprintf(title,"Corrupt BEMC ADC for Crate %d",i+16);
      sprintf(code,"CBadcCr%d",i+16);
      cBadc[i]=new TH2F(code,title,4096,0.0,4096.0,160,0.0,160);
    }
    
    for (int i=0;i<MaxTwCrates;i++){
      sprintf(title,"EEMC ADC for Crate %d",i);
      sprintf(code,"EadcCr%d",i);
      hEadc[i]=new TH2F(code,title,4096,0.0,4096.0,128,0.0,128);
      sprintf(title,"Corrupt EEMC ADC for Crate %d",i);
      sprintf(code,"CEadcCr%d",i);
      cEadc[i]=new TH2F(code,title,4096,0.0,4096.0,128,0.0,128);
    }
    
    for (int i=0;i<16;i++){
      sprintf(title,"EEMC SMD ADC for Crate %d",i+84);
      sprintf(code,"ESadcCr%d",i+84);
      hESadc[i]=new TH2F(code,title,4096,0.0,4096.0,192,0.0,192);
      sprintf(title,"Corrupt EEMC SMD ADC for Crate %d",i+84);
      sprintf(code,"CESadcCr%d",i+84);
      cESadc[i]=new TH2F(code,title,4096,0.0,4096.0,192,0.0,192);
    }
    
    
    hBdiag[0]= new TH1F("BbadCr","Bad BEMC Crate #'s in Corrupt Events",50,0,50);
    hBdiag[1]= new TH1F("Bfail","Corrupt Header Words in BEMC tower",5,-0.5,4.5);
    hEdiag[0]= new TH1F("EbadCr","Bad EEMC Crate #'s in Corrupt Events",7,0,7);
    hEdiag[1]= new TH1F("Efail","Corrupt Header Words in EEMC tower",5,-0.5,4.5);
    hESdiag[0]= new TH1F("ESbadCr","Bad EEMC Mapmt Crate #'s in Corrupt Events",100,50,150);
    hESdiag[1]= new TH1F("ESfail","Corrupt Header Words in EEMC Mapmt",5,-0.5,4.5);
    
    hDiag= new TH1F("Corr","Correlation of Corruption between ESMD,ETOW,BTOW",3,-0.5,2.5);
    HList->Add(hDiag);
    
    if(HList) {
      for (int i=0;i<MaxBTwCrate;i++){
	HList->Add(hBadc[i]);
	HList->Add(cBadc[i]);
      }
      for (int i=0;i<MaxTwCrates;i++){
	HList->Add(hEadc[i]);
	HList->Add(cEadc[i]);
      }
      for (int i=0;i<16;i++){
	HList->Add(hESadc[i]);
	HList->Add(cESadc[i]);
      }
      for (int i=0;i<2;i++){
	HList->Add(hBdiag[i]);
	HList->Add(hEdiag[i]);
	HList->Add(hESdiag[i]);
      }
    }   
  }
}


//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void CorrAna::clear(){ // called for every event
  memset(crateE,0,sizeof(crateE));
  memset(crateES,0,sizeof(crateES));
  memset(crateB,0,sizeof(crateB));
  memset(corruptE,0,sizeof(corruptE));
  memset(corruptES,0,sizeof(corruptES));
  memset(corruptB,0,sizeof(corruptB));
  memset(badEcrate,0,sizeof(badEcrate));
  memset(badEScrate,0,sizeof(badEScrate));
  memset(badBcrate,0,sizeof(badBcrate));
  memset(Blist,0,sizeof(Blist));
  memset(Elist,0,sizeof(Elist));
  memset(ESlist,0,sizeof(ESlist));
  allBad=0;
}

//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void CorrAna::finish(){
  printf("\n  CorrAna::finish() nInpEve=%d\n",nInpEve);
}

//-------------------------------------------------
//-------------------------------------------------
void CorrAna::print(){
  printf("\n  CorrAna::print()\n  dump event:\n");
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
void CorrAna::taskDiag(){
  printf("\n  CorrAna::taskDiag()\n");
  hDiag->Fill(allBad);
}

//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void CorrAna::taskEgood(){
  printf("\n  CorrAna::taskEgood()\n");
  int i,j;
  for (j=0;j<MaxTwCrates;j++){
    for (i=0;i<MaxTwCrateCh;i++){
      int adc=crateE[j][i];
      hEadc[j]->Fill(adc,i);
    }
  }
}

//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void CorrAna::taskEbad(){
  printf("\n  CorrAna::taskEbad()\n");
  int i,j;
  for (j=0;j<MaxTwCrates;j++){
    for (i=0;i<MaxTwCrateCh;i++){
      int adc=corruptE[j][i];
      cEadc[j]->Fill(adc,i);
    }
  }

  for (j=0;j<=MaxTwCrates;j++){
    if (badEcrate[j]==1){ 
      hEdiag[0]->Fill(j);
    }
  }

  for (j=0;j<5;j++){
    UChar_t test=(int)TMath::Power(2,j); // probably 1<<j would work as well, JB
    if (test&Esanity) hEdiag[1]->Fill(j);
  }
}

//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void CorrAna::taskESgood(){
  printf("\n  CorrAna::taskESgood()\n");
  int i,j;
  for (j=0;j<16;j++){
    for (i=0;i<MaxMapmtCrateCh;i++){
      int adc=crateES[j][i];
      hESadc[j]->Fill(adc,i);
    }
  }
}

//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void CorrAna::taskESbad(){
  printf("\n  CorrAna::taskESbad()\n");
  int i,j;
  for (j=0;j<MaxEsmdCrate;j++){
    for (i=0;i<MaxMapmtCrateCh;i++){
      int adc=corruptES[j][i];
      cESadc[j]->Fill(adc,i);
    }
  }

  for (j=0;j<=MaxEsmdCrate;j++){
    if (badEScrate[j]==1){ 
      hESdiag[0]->Fill(j+84);
    }
  }

  for (j=0;j<5;j++){
    UChar_t test=(int)TMath::Power(2,j);
    if (test&ESsanity) hESdiag[1]->Fill(j);
  }

}



//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void CorrAna::taskBbad(){
  printf("\n  CorrAna::taskBbad()\n");
  int i,j;
  for (j=0;j<MaxBTwCrate;j++){
    for (i=0;i<MaxBTwCrateCh;i++){
      int adc=corruptB[j][i];
      cBadc[j]->Fill(adc,i);
    }
  }
  for (j=0;j<=MaxBTwCrate;j++){
    // printf("BEMC badcrate #%d = %d\n",j,badBcrate[j]);
    if (badBcrate[j]==1){ 
      hBdiag[0]->Fill(j+16);
    }
  }

  for (j=0;j<5;j++){
    UChar_t test=(int)TMath::Power(2,j);
    // printf("Test=%d,BSanity=%d,And=%d\n",test,Bsanity,(test&Bsanity));
    if ((test&Bsanity)==test) {
      hBdiag[1]->Fill(j);
      // printf("Filling for bit %d\n",j);
    }
      
  }

}

//-------------------------------------------------
//-------------------------------------------------
//-------------------------------------------------
void CorrAna::taskBgood(){
  printf("\n  CorrAna::taskBgood()\n");
  int i,j;
  for (j=0;j<MaxBTwCrate;j++){
    for (i=0;i<MaxBTwCrateCh;i++){
      int adc=crateB[j][i];
      hBadc[j]->Fill(adc,i);
    }
  }
}


 
//-------------------------------------------------
//-------------------------------------------------
void CorrAna:: saveHisto(TString fname){
  TString outName=fname+".hist.root";
  TFile f( outName,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),outName.Data());
  HList->Write();
  f.Close();
}

/*****************************************************************
 * $Log: CorrAna.cxx,v $
 * Revision 1.3  2007/07/12 19:24:31  fisyak
 * Add includes for TString and TMath for ROOT 5.16
 *
 * Revision 1.2  2004/07/26 22:54:25  rfatemi
 * Corruption Update
 *
 * Revision 1.1  2004/07/24 22:51:08  balewski
 * first
 *
 * Revision 1.1  2004/06/06 04:54:10  balewski
 * dual analyzis
 *
 *
 ********************************************************************/
