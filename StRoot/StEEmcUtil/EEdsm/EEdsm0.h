#ifndef EEdsm0_h
#define EEdsm0_h
/**************************************************************
 * $Id: EEdsm0.h,v 1.2 2005/02/01 22:13:37 perev Exp $
 **************************************************************/
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 

typedef unsigned char uchar;

class EEdsm0  {
  static const int nw=16;
  static const int nc=10;
  uchar data[nw];
  
 private:
 public:
  
  EEdsm0();
  virtual ~EEdsm0();
  void  print(int k=0);
  void  clear();
  void setBite(int b, uchar val);
  uint getChan(int ch); // only lower 12 bits are used, upper=0
  uint getHT(int ch){ return getChan(ch) & 0x3f;} 
  uint getTP(int ch){ return getChan(ch)>>6; }
  int maxHT(); // returns ch
  int maxTP(); // returns ch
  int getNc(){return nc;}
  
};
#endif

/*
 * $Log: EEdsm0.h,v $
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


