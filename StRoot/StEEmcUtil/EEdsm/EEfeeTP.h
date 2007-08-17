#ifndef EEfeeTP_h
#define EEfeeTP_h
/**************************************************************
 * $Id: EEfeeTP.h,v 1.1 2007/08/17 01:15:37 balewski Exp $
 * compute Endcap FEE response for a single trigger patch (TP)
 **************************************************************/
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 


class EEfeeTP  {
  enum {mxT=10,mxTxt=100};
  int  lenCh, cha0L, cha0H, crateID; // addres of towers in ETOW data block
  char name[mxTxt];
  int  nT; // # of active towers in this TP

  // .... working variables
  int adc12[mxT], ped4[mxT]; // input
  int chanID[mxT]; // oryginal channel mapping
  int adc10p[mxT]; // 10 bit ADC corrected for FEE pedestal
  int adc6[mxT]; // 6 bit ADC w/ overflow bit
  int HT6b; // final 6-bit HT value
  int TPsum6b; // final 6-bit TPsum value
  int HTchId; // channel ID for HT 

public:

  EEfeeTP( int xcrate, char *TPname, int lenH, int xcha0L, int xcha0H=-1);

  void  compute(int *adcA, int *ped4A, int *maskA);
  void  print(int k=0);
  void  clear();
  //..... transient values
  int   getTranHTchId() {return HTchId;}
  int   getCrateID(){return crateID;} 
  int   getNtow(){return nT;} 
  //..... output
  int   getOutTPsum(){return TPsum6b;} 
  int   getOutHT() {return HT6b;}
  int   getOut12bit() { return (getOutTPsum()<<6) + getOutHT();}
  
}; 
#endif

/*
 * $Log: EEfeeTP.h,v $
 * Revision 1.1  2007/08/17 01:15:37  balewski
 * full blown Endcap trigger simu, by Xin
 *
 *
 **************************************************************/


