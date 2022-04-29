#ifndef EEdsm0_h
#define EEdsm0_h
/**************************************************************
 * $Id: EEdsm0.h,v 1.1 2009/10/12 18:04:25 pibero Exp $
 **************************************************************/
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 

typedef unsigned char uchar;

class EEdsm0  {
  enum {nw=16,nc=10,mxTh=3,mxOu=2};
  uchar data[nw];
  short value[nc]; // unpacked 12bit input values
  int type;
  int mYear;// HTTP algo added in 2006
  int HTthr[mxTh], TPthr[mxTh];
  int outHTTP2bit[mxOu];
  int outHT2bit[mxOu];
  int outTP2bit[mxOu];
  int outTPsum[mxOu]; // 10 or 9 bits
  int out16bit[mxOu];

 public:
  
  EEdsm0();
  virtual ~EEdsm0();  
  void  setYear(int y, int*HTth, int*TPth);
  void  setType(int t) {type=t;}
  void  print(int k=0) const;
  void  clear();
  void  compute(); // for 2006+
  void  unpack(); // 16 inputs --> 10 values

  //.... input
  void  setBite(int b, uchar val); // from trigger data block
  void  setInp12bit(int ch, short val); // HT+TPsum from one FEE TP  
  int   getInp12bit(int ch) const; // HT+TPsum from one FEE TP  
  int   getInpHT6bit(int ch) const { return  getInp12bit(ch) & 0x3f;} 
  int   getInpTP6bit(int ch) const { return getInp12bit(ch)>>6; }
  int   getNc() const {return nc;} 

  //....... output
  int  getOutTPsum(int k=0) const { return outTPsum[k];} // default type 1
  int  getOutHT2bit(int k=0) const { return outHT2bit[k];} // default type 1
  int  getOutTP2bit(int k=0) const { return outTP2bit[k];} // default type 1 
  int  getOutHTTP2bit(int k=0) const { return outHTTP2bit[k];} // default type 1
  int  getOut16bit(int k=0) const { return out16bit[k];} // default type 1
};
#endif

/*
 * $Log: EEdsm0.h,v $
 * Revision 1.1  2009/10/12 18:04:25  pibero
 * Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
 *
 * Revision 1.4  2009/02/24 03:56:18  ogrebeny
 * Corrected const-ness
 *
 * Revision 1.3  2007/08/17 01:15:35  balewski
 * full blown Endcap trigger simu, by Xin
 *
 * Revision 1.2  2005/02/01 22:13:37  perev
 * Compatibility to redhat
 *
 * Revision 1.1  2004/11/29 20:12:59  balewski
 * first
 *
 * Revision 1.1  2004/02/17 03:09:17  balewski
 * *** empty log message ***
 *
 * Revision 1.1  2003/12/29 02:18:38  balewski
 * star
 *
 * Revision 1.1  2003/05/22 19:39:00  balewski
 * analysis of DSM data
 *
 * 
 * decode INPUT data for one board of EE-DSM Layer-0
 *
 **************************************************************/


