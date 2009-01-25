// $Id: EEqaSorterA.cxx,v 1.3 2009/01/25 01:36:54 ogrebeny Exp $
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include <TObjArray.h>
#include <TClonesArray.h>

#include <TH2.h>
#include <TFile.h>

#include "EEqaSorterA.h"
#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"

ClassImp(EEqaSorterA)

//-------------------------------------------
//-------------------------------------------
 EEqaSorterA:: EEqaSorterA( TObjArray*L) {
   // printf("\n\n  EEqaSorterA:: EEqaSorterA() \n\n");
  HList=L; 

  hotTwThres=40;// used to count hot towers
  memset(feePed,0,sizeof(feePed)); // clear ped values
} 
 


//-------------------------------------------
//-------------------------------------------
void  EEqaSorterA::sort( EztEmcRawData  *t,  EztEmcRawData  *s, int ver ) {
  eETow=t;
  eESmd=s;
  // eESmd->print(1);
  sortDaqTower1();
  sortDaqMapmt0(ver);
  sortDaqTowerHot();
}


//-------------------------------------------
//-------------------------------------------
void  EEqaSorterA::Finish(){
}

//-------------------------------------------
//-------------------------------------------
void  EEqaSorterA::sortDaqTower1(){
  /* Goals of this method:
     make histos of adc vs channel for each tower crate
     and count ADC values = n*256 in all data blocks 
  */
  if(eETow==0) return;
  int icr;
  for(icr=0;icr<eETow->getNBlocks();icr++) {
    if(eETow->isCrateVoid(icr)) continue;
    int crateID=icr+1;
    int i;
    const UShort_t* data=eETow->data(icr);
    for(i=0;i<eETow->sizeData(icr);i++) {
      int chan=i;
      float adc=data[i];
      hCrate[crateID-1]->Fill(adc,chan);
    }
  }// end of loop over crates
 }


//-------------------------------------------
//-------------------------------------------
void  EEqaSorterA::sortDaqTowerHot(){
  /* Goals of this method:
     increment (hot) towers above some threshold 
  */

  if(eETow==0) return;
  int icr;

  for(icr=0;icr<eETow->getNBlocks();icr++) {
    if(eETow->isCrateVoid(icr)) continue;
    int crateID=icr+1;
    int i;
    const UShort_t* data=eETow->data(icr);
    int *pedA= feePed +(crateID-1)*MaxTwCrateCh;
    for(i=0;i<eETow->sizeData(icr);i++) {
      int chan=i;
      float adc=data[i]-24+ pedA[i];
      // printf("cr=%d, ch=%d  ped=%d\n",icr,i,ped[i]);
      if(adc<hotTwThres) continue;
      hCrateHot[crateID-1]->Fill(chan);
    }
  }// end of loop over crates

}


//-------------------------------------------
//-------------------------------------------
void  EEqaSorterA::sortDaqMapmt0( int ver){
  /* Goal of this method:
     make histos of adc vs channel for each MAPMT crate
  */
  if(eESmd==0) return;
  int icr;
  for(icr=0;icr<eESmd->getNBlocks();icr++) {
    if(eESmd->isCrateVoid(icr)) continue;
    int crateID=icr+64;
    // in 2004 there was only 16 MAPMT crates for sectors 5-8
    if(ver<0x22) {
      if(icr>=16) break;
      crateID=icr+84;
    }
    //printf("ddd %d %d\n",icr,crateID);
    int i;
    const UShort_t* data=eESmd->data(icr);
    for(i=0;i<eESmd->sizeData(icr);i++) {
      int chan=i; 
      float adc=data[i];
      hCrate[crateID-1]->Fill(adc,chan);
    }
  }// end of loop over crates
  
}


//--------------------------------------------------
//--------------------------------------------------
void EEqaSorterA::initCrateHisto(int nBin, int mxADC){
  char *sectL[]={"11TD-1TC", "1TD-3TC", "3TD-5TC", "5TD-7TC", "7TD-9TC", "9TD-11TC"};
  
  printf(" EEqaSorterA::initCrateHisto(nb=%d, max=%d)\n", nBin,mxADC);
  // init histo
  assert(HList);
  int nOK=0;
  
  hCrate = new TH2F *[MaxAnyCrate];

  int icr;
  for(icr=0;icr<MaxAnyCrate;icr++) { 
    //if(icr!=63) continue;
    int crateID=icr+1;
    hCrate[icr]=0;
    int mxChan=0;
    char text[100];
    char *physDet=0;
    if(crateID>=MinTwCrateID && crateID<=MaxTwCrateID) {
      // Towers
      mxChan=MaxTwCrateCh;
      physDet=sectL[crateID-1];
    } else if (crateID>=MinMapmtCrateID && crateID<=MaxMapmtCrateID ){
      //MAPMT
      mxChan=MaxMapmtCrateCh;
      int sec=1+(7+crateID/4)%MaxSectors;
      int box=1+crateID%4;
      sprintf(text,"%dS%d",sec,box);
      if(box==4) sprintf(text,"%dP1",sec);
      physDet=text;
    }
    
    if(mxChan==0) continue; // skip nonexisting crates
    nOK++;
    char tt1[100], tt2[100];
    sprintf(tt1,"cr%d",crateID);
    sprintf(tt2," %s",physDet);//Chan vs. ADC
    TH2F* h=new TH2F(tt1,tt2,nBin,-0.5, mxADC-0.5, mxChan, -0.5, mxChan-0.5);
    HList->Add(h);
    // printf("Histo Init: icr=%d crID=%d %s \n",icr,crateID,h->GetTitle());
    hCrate[icr]=h;
  }
  

  hCrateHot = new TH1F *[MaxTwCrateID];
  for(icr=0;icr<MaxTwCrateID;icr++) { 
    int crateID=icr+1; 
    hCrateHot[icr]=0;
    int mxChan=MaxTwCrateCh;
    char tt1[100], tt2[100];
    sprintf(tt1,"cr%dHot",crateID);
    sprintf(tt2,"%s thr=feePed+%d",sectL[crateID-1],hotTwThres);
    TH1F* h=new TH1F(tt1,tt2, mxChan, -0.5, mxChan-0.5);
    h->SetFillColor(kBlue);
    HList->Add(h);
    // printf("cr=%d %s \n",crateID,h->GetTitle());
    hCrateHot[icr]=h;
  }


  printf("Initialized %d 2D carte-histos\n",nOK);
}  


//--------------------------------------------------
//--------------------------------------------------
int EEqaSorterA::usePed4( TString fName){
  //......................... load ped for L-0
  printf(" EEqaSorterA::usePed4('%s') ...\n",fName.Data());
  
  FILE *fd=fopen(fName.Data(),"r");
  if(fd==0) goto abandon;
  assert(fd);
  int cr,ch;
  for(cr=0;cr<MaxTwCrateID;cr++) 
    for(ch=0;ch<MaxTwCrateCh; ch++) {
      int xcr,xch, ped4;
      float xped;
      int ret=fscanf(fd,"%d %d %f %d",&xcr,&xch,&xped,&ped4);
#if 0
      printf("ret=%d %d %d %f %d\n",ret,xcr,xch,xped,ped4);
      assert(ret==4);
      assert(xcr==cr+1);
      assert(xch==ch);
#endif
      if(ret!=4) goto abandon;
      if(xcr!=cr+1) goto abandon;
      if(xch!=ch) goto abandon;
      feePed[cr*MaxTwCrateCh + ch]=4*ped4;
    }
  fclose(fd);
  printf(" EEqaSorterA::usePed4(...) Loaded\n");

  return 0;
 abandon: // any error has happened (this is a new approach for me, JB)
  memset(feePed,0,sizeof(feePed)); // clear FeePed values
  printf(" EEqaSorterA::usePed4('%s') FAILED\n",fName.Data());
  return 1;
}



// $Log: EEqaSorterA.cxx,v $
// Revision 1.3  2009/01/25 01:36:54  ogrebeny
// *** empty log message ***
//
// Revision 1.2  2009/01/23 00:14:50  ogrebeny
// Inherited EEmcDb from StEEmcDbMaker to fix run-time bug http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1378
//
// Revision 1.1  2005/04/28 20:54:46  balewski
// start
//
// Revision 1.8  2004/03/13 22:03:13  balewski
// new plots from Hal added
//
// Revision 1.6  2004/02/26 04:22:24  balewski
// more Hal's plots
//
// Revision 1.5  2004/02/17 03:09:18  balewski
// *** empty log message ***
//
// Revision 1.4  2004/01/29 17:23:14  balewski
// fix for BTOW
//
// Revision 1.3  2004/01/27 16:29:39  balewski
// reset added
//
