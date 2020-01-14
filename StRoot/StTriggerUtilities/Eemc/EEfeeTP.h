#ifndef EEfeeTP_h
#define EEfeeTP_h
/**************************************************************
 * $Id: EEfeeTP.h,v 1.4 2020/01/13 20:45:49 zchang Exp $
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

  EEfeeTP( int xcrate, const char *TPname, int lenH, int xcha0L, int xcha0H=-1);

  void  compute(int *adcA, int *ped4A, int *maskA, int highTowerMask, int patchSumMask);
  void  print(int k=0) const;
  void  clear();
  //..... transient values
  int   getTranHTchId() const {return HTchId;}
  int   getCrateID() const {return crateID;} 
  int   getNtow() const {return nT;} 
  //..... output
  int   getOutTPsum() const {return TPsum6b;} 
  int   getOutHT() const {return HT6b;}
  int   getOut12bit() const { return (getOutTPsum()<<6) + getOutHT();}
  //zchang
  void test(int tp, int ht) { TPsum6b = tp; HT6b = ht; }  
}; 
#endif

/*
 * $Log: EEfeeTP.h,v $
 * Revision 1.4  2020/01/13 20:45:49  zchang
 * removing old run13 dsm algo files
 *
 * Revision 1.3  2011/10/16 17:41:59  pibero
 * Implement EEMC FEE HT & TP masks
 *
 * Revision 1.2  2009/11/19 15:48:40  balewski
 * add (char*) to many strings to make SL5 happ, few other adjustments
 *
 * Revision 1.1  2009/10/12 18:04:27  pibero
 * Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
 *
 * Revision 1.2  2009/02/24 03:56:19  ogrebeny
 * Corrected const-ness
 *
 * Revision 1.1  2007/08/17 01:15:37  balewski
 * full blown Endcap trigger simu, by Xin
 *
 *
 **************************************************************/


