/**************************************************************
 * $Id: EEfeeTP.cxx,v 1.3 2011/10/16 17:41:59 pibero Exp $
 **************************************************************/

#include <iostream>
#include <assert.h>


#include "EEfeeTP.h"


//--------------------------------------------------
//
//--------------------------------------------------
EEfeeTP::EEfeeTP( int xcrate, const char *TPname, int lenX, int xcha0L, int xcha0H) {
  // if TP spans one block of channels set xcha0H<0
  assert(lenX%2==0);
  lenCh=lenX;
  cha0L=xcha0L;
  cha0H=xcha0H; 
  crateID=xcrate;
  strncpy(name,TPname,mxTxt);
  nT=0;
}


//--------------------------------------------------
//--------------------------------------------------
void 
EEfeeTP::clear() {
  memset(adc12,0,sizeof(adc12));
  memset(ped4,0,sizeof(ped4));
  memset(chanID,0,sizeof(chanID));
  memset(adc10p,0,sizeof(adc10p));
  memset(adc6,0,sizeof(adc6));

  nT=0; // could be done just once if masked were used in InitRun
  HT6b=0;
  TPsum6b=0;
  HTchId=-2;
}

//--------------------------------------------------
//--------------------------------------------------
void 
EEfeeTP::compute(int *adcA, int *ped4A, int *maskA, int highTowerMask, int patchSumMask){
  // input: arrays for the same crate

  int i;
  int len=lenCh;
  if(cha0H>0) len=len/2; // there will be to subsets of channels
  //...................first pick up the raw ADC from the data block
  for (i=0; i<len; i++){
    if(        maskA[cha0L+i]) continue;
    adc12[nT]= adcA[cha0L+i] & 0xfff; // take only lower 12 bits
    ped4  [nT]=ped4A[cha0L+i];
    chanID[nT]=      cha0L+i;
    nT++;
  }
  
  if(cha0H>0) { // read another half of channels
    for (i=0; i<len; i++){
      if(        maskA[cha0H+i]) continue;
      adc12[nT]= adcA[cha0H+i] & 0xfff; // take only lower 12 bits
      ped4  [nT]=ped4A[cha0H+i];
      chanID[nT]=      cha0H+i;
      nT++;
    }
  }
  
  //.......................Emulate FEE math
  HTchId=-1;
  HT6b=0;
  int TPsum8b=0;
  
  for (i=0; i<nT; i++){
    int adc10=adc12[i]>>2; // shift ADC by 2 bits
    int x10=adc10+ped4[i]; // ped adjusted 10 bit ADC
    if ( x10<0) x10=0;
    x10 &= 0x3ff; // clear higher bits, wrap around
    adc10p[i]=x10; // store it for QA
    TPsum8b+=x10 >>2; // shift 10-bit ped adjusted ADC by 2 bits and sum up

    // .. reduce HT energy to 6 bits, use the highest bit as overflow bit
    int y=x10;
    if(( y>>7) & 0x7 ) y |=(1<<7); // set the overflow bit
    y=( y >> 2 )  & 0x3f; // take bits [7,..,2]
    adc6[i]=y;  // store it for QA
    
    if( HT6b < y ) { // search for high tower using 6-bit ADCs
      HT6b=y;
      HTchId=chanID[i]; // keep track which tower is HT, for QA
    }              
    
  }
  
  TPsum6b  =TPsum8b - nT + 1; // subtract offset = # towers - 1
  // take lowest 6 bits of TPsum8b w/ over & underflow protection
  if (TPsum6b <0 ) TPsum6b=0;
  if (TPsum6b > 62) TPsum6b=62;

  if (!highTowerMask) HT6b = 0;
  if (!patchSumMask) TPsum6b = 0;
}


//--------------------------------------------------
//--------------------------------------------------
void 
EEfeeTP::print( int k) const {
  printf("EEfeeTP: name=%s nT=%d\n",name,nT);
  int i;
  printf(" channel: "); for(i=0;i<nT;i++) printf(" %4d",chanID[i]); printf("\n");
  printf(" rawADC : "); for(i=0;i<nT;i++) printf(" %4d",adc12[i]);printf("\n");
  printf("    ped4: "); for(i=0;i<nT;i++) printf(" %4d",ped4[i]);printf("\n");
  printf(" adc10+p: "); for(i=0;i<nT;i++) printf(" %4d",adc10p[i]);printf("\n");
  printf("   adc6 :  "); 
  for(i=0;i<nT;i++) {
    printf("%4d",adc6[i]);
    if(HTchId==chanID[i]) printf("*"); else printf(" ");
  }
  printf("\nOUTPUT:  HT6b=%d  TPsum6b=%d \n",getOutHT(),getOutTPsum());
}
 

/*
 * $Log: EEfeeTP.cxx,v $
 * Revision 1.3  2011/10/16 17:41:59  pibero
 * Implement EEMC FEE HT & TP masks
 *
 * Revision 1.2  2009/11/19 15:48:40  balewski
 * add (char*) to many strings to make SL5 happ, few other adjustments
 *
 * Revision 1.1  2009/10/12 18:04:26  pibero
 * Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
 *
 * Revision 1.2  2009/02/24 03:56:19  ogrebeny
 * Corrected const-ness
 *
 * Revision 1.1  2007/08/17 01:15:37  balewski
 * full blown Endcap trigger simu, by Xin
 *
 */

