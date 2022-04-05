// $Id: EEsectorDst.cxx,v 1.8 2007/05/30 02:38:48 balewski Exp $

#include <cassert>
#include <TClonesArray.h>
#include <StMessMgr.h>

#include "EEtwHitDst.h"
#include "EEsmdHitDst.h"
#include "EEsectorDst.h"

ClassImp(EEsectorDst)

//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
EEsectorDst::EEsectorDst(int id){
  //  printf("EEsectorDst(ID=%d) constructed\n",id);
  ID=id;
  Pre1Hits  = new TClonesArray("EEtwHitDst",1000);  
  Pre2Hits  = new TClonesArray("EEtwHitDst",1000);
  TwHits    = new TClonesArray("EEtwHitDst",1000);
  SmdUHits  = new TClonesArray("EEsmdHitDst",1000);
  SmdVHits  = new TClonesArray("EEsmdHitDst",1000);
  PostHits  = new TClonesArray("EEtwHitDst",1000);
}

//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
EEsectorDst::~EEsectorDst() {
  delete Pre1Hits;
  delete Pre2Hits;
  delete TwHits;
  delete SmdUHits;
  delete SmdVHits;
  delete PostHits;
}

//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
void EEsectorDst::clear(){
  Pre1Hits->Clear();
  Pre2Hits->Clear();
  SmdUHits->Clear();
  SmdVHits->Clear();
  TwHits->Clear();
  PostHits->Clear();
}


//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
void EEsectorDst::print(int k){

  LOG_INFO<<Form("EEsectorDst(ID=%d)::print() nPre1Hit=%d  nPre2Hit=%d nSmdUHit=%d  nSmdVHit=%d nTowerHit=%d  nPostHit=%d \n",ID,Pre1Hits->GetEntries(),Pre2Hits->GetEntries(),SmdUHits->GetEntries(),SmdVHits->GetEntries(),TwHits->GetEntries(),PostHits->GetEntries())<<endm;
  
  if(k<0) return;
  
  LOG_INFO<<Form("EEsectorDst(Tower) nHit=%d\n",TwHits->GetEntries())<<endm;
  int i;
  
  TClonesArray *hitA=getTwHits();
  for(i=0;i<hitA->GetEntries();i++){ 
    EEtwHitDst *hit=(EEtwHitDst*)hitA->At(i);
    hit->print();
  }

 LOG_INFO<<Form ("EEsectorDst(pre/post/SMD) not implemented\n")<<endm;
}


//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
void EEsectorDst::addTwHit(char sub, int eta, float ener, TClonesArray *hitA) {
 
  TClonesArray &hits = *hitA;
  int len=hits.GetEntries();
  EEtwHitDst *hit= new(hits[len]) EEtwHitDst;
  hit->set(sub,eta,ener);
  //  printf("in , added ~tw hit=%d ener=%f\n",len,ener);
}
 

//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
void EEsectorDst::addSmdHit(int strip, float ener, TClonesArray *hitA) {
 
  TClonesArray &hits = *hitA;
  int len=hits.GetEntries();
  EEsmdHitDst *hit= new(hits[len]) EEsmdHitDst;
  //printf("in , added ~smd hit=%d ener=%f\n",len,ener);
  
  hit->set(strip,ener);
}
 
//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
void EEsectorDst::sumRawMC(EEsectorDst *outSec, float minE) {

  //  const float fac[4]={1,1,1,1}; //tmp, needs to be adjusted
  //  for  Tower=sum over layers

  const int mx=60;  
  const int mx2=300;  
  float sum1[mx];  // hit accumulator for geant depth 1 == layer 1
  float sum2[mx];  //  depth 2 == layer 2
  float sumU[mx2]; //  depth - == smd U
  float sumV[mx2]; //  depth - == smd V
  float sum3[mx];  //  depth 3 == layer 3-23
  float sum4[mx];  //  depth 4 == layer 24

  sumRawMCtw(Pre1Hits,sum1,mx);
  sumRawMCtw(Pre2Hits,sum2,mx);

  sumRawMCsmd(SmdUHits ,sumU,mx2); 
  sumRawMCsmd(SmdVHits ,sumV,mx2); 
  sumRawMCtw(TwHits  ,sum3,mx); 
  sumRawMCtw(PostHits,sum4,mx);
  
  int j;
  
  // extract non-zero elements, build tower response
  float grandSum=0;


  //  for(j=0;j<mx2;j++) grandSum+=sumU[j];
  // for(j=0;j<mx2;j++) grandSum+=sumV[j];
  
  // copy tower hits above energy threshold
  for(j=0;j<mx;j++) {
    char sub='A'+j/12;
    int eta=1+j%12;
    float ener1=sum1[j]; // layer 1
    float ener2=sum2[j]; // layer 2
    float ener3=sum3[j]; // layer 3-23 
    float ener4=sum4[j]; // layer 24

    float ener14=ener1+ener2+ener3+ener4; // all layers

    grandSum+=ener14;
    
    if(ener1>minE)  //Pre1
      addTwHit(sub,eta,ener1,outSec->Pre1Hits); 
    
    if(ener2>minE)  //Pre2
      addTwHit(sub,eta,ener2,outSec->Pre2Hits); 
    
    if(ener14>minE)  //Tower
      addTwHit(sub,eta,ener14,outSec->TwHits); 
    
    if(ener4>minE)  //Post
      addTwHit(sub,eta,ener4,outSec->PostHits); 
    
  }// end of loop over 60 towers in subsect

  //  printf(" sector=%d towerOnlySum=%f\n",ID,grandSum);
  // copy smd-U&V  hits above energy threshold
  for(j=0;j<mx2;j++) {
    int strip=j+1;

    float ener=sumU[j];
    grandSum+=ener;
    if(ener>minE)
      addSmdHit(strip,ener,outSec->SmdUHits); 
    
    ener=sumV[j];
    grandSum+=ener;
    if(ener>minE)
      addSmdHit(strip,ener,outSec->SmdVHits); 
    
  }// end of loop over 288 SMD strip
  
  //  printf(" sector=%d grandSum=%f\n",ID,grandSum);
  
}

//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
void EEsectorDst::sumRawMCtw(TClonesArray *inH, float* sum, int mx) {
  //printf("\n \ncall  EEsectorDst::sumRawMCtw\n");

  int j;
  for(j=0;j<mx;j++) sum[j]=0;
  
  int nOK=0;
  // acumulate hits
  int ih;
  for(ih=0;ih<inH->GetEntries();ih++) {
    EEtwHitDst *hit=(EEtwHitDst*)inH->At(ih);
    char sub;
    int eta;
    float ener;

    hit->get(sub,eta,ener);
    //printf("    index=%d sub=%c etaBin=%d ener=%f \n",index, sub, eta,ener);
    
    nOK++;

    int index=(sub-'A')*12+ (eta-1);
    assert(index>=0 && index<mx);
    sum[index]+=ener; 
  }
  // printf("sumRawMCtw() accepted %d of %d raw GEANT hits\n",nOK,inH->GetEntries());
}


//---------------------------------------------------
//---------------------------------------------------
//---------------------------------------------------
void EEsectorDst::sumRawMCsmd(TClonesArray *inH, float* sum, int mx) {
  
  int j;
  for(j=0;j<mx;j++) sum[j]=0;
  
  int nOK=0;
  int ih;
  for(ih=0;ih<inH->GetEntries();ih++) { // acumulate hits
    EEsmdHitDst *hit=(EEsmdHitDst*)inH->At(ih);
    int strip;
    float ener;

    hit->get(strip,ener);
    //    hit->print();
    nOK++;

    int index=strip-1;
    assert(index>=0 && index<mx);
    sum[index]+=ener; 
  }
  //  printf("sumRawMCsmd() accepted %d of %d raw GEANT hits\n",nOK,inH->GetEntries());
}

// $Log: EEsectorDst.cxx,v $
// Revision 1.8  2007/05/30 02:38:48  balewski
// replace printf -->LOG_XXX
//
// Revision 1.7  2003/11/12 19:59:06  balewski
// I forgot what has changed
//
// Revision 1.6  2003/10/02 20:52:45  balewski
// more functionality for print()
//
// Revision 1.5  2003/09/11 19:40:56  zolnie
// updates for gcc3.2
//
// Revision 1.4  2003/07/01 14:13:13  balewski
// no clue
//
// Revision 1.3  2003/02/20 20:13:15  balewski
// fixxy
// xy
//
// Revision 1.2  2003/02/20 05:15:14  balewski
// reorganization
//
// Revision 1.1  2003/01/28 23:16:07  balewski
// start
//
// Revision 1.8  2002/11/30 20:03:15  balewski
// consistent with FeeRawTTree
//
// Revision 1.7  2002/10/03 00:30:46  balewski
// tof taken away
//
// Revision 1.6  2002/10/01 06:03:16  balewski
// added smd & pre2 to TTree, tof removed
//
// Revision 1.5  2002/09/27 19:10:37  balewski
// aaa
//
// Revision 1.4  2002/09/25 16:47:55  balewski
// cleanup , cut in geant time for twoer-like detectors
//
// Revision 1.3  2002/09/25 01:36:13  balewski
// fixed TOF in geant
//
// Revision 1.2  2002/09/20 21:58:13  balewski
// sum of MC hits over activ detectors
// produce total tower energy with weight 1 1 1 1
//
// Revision 1.1.1.1  2002/09/19 18:58:54  zolnie
// Imported sources
//
// Revision 1.1.1.1  2002/08/29 19:32:01  zolnie
// imported sources
//
// Revision 1.2  2002/08/28 01:43:42  zolnie
// version alpha - 2
//
// Revision 1.1  2002/08/26 19:46:12  zolnie
// Initial revision
//





