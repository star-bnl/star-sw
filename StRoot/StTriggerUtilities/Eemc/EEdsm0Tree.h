// -*- mode:c++ -*-

#ifndef EEdsm0Tree_h
#define EEdsm0Tree_h
/**************************************************************
 * $Id: EEdsm0Tree.h,v 1.2 2009/11/18 15:50:59 pibero Exp $
 * Emulates functionality of  Endcap DSM0-tree
 **************************************************************/
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 
class EEdsm0;

class EEdsm0Tree  {  // DSM0 tree emulators

 public:
   enum { Nee0=9,Nee0out=12, mxTxt=16};

 private:
  EEdsm0 *ee0; //  Level-0 boards
  int ee0outTPsum[Nee0out]; // Level-0 emulated output
  int ee0outTP2bit[Nee0out]; // Level-0 emulated output
  int ee0outHT2bit[Nee0out]; // Level-0 emulated output
  int ee0outHTTP2bit[Nee0out]; // Level-0 emulated output
  int ee0out16bit[Nee0out]; // Level-0 emulated output
  char name[mxTxt];
  

 public:
  
  EEdsm0Tree(const char *);
  ~EEdsm0Tree();
  void  setYear(int y, int*HTth, int*TPth);
  void  print(int k=0) const;
  void  clear();
  //... input
  void setInp12bit(int HankCh, short val); // HT+TPsum from one FEE TP  
  int  getInp12bit(int HankCh) const;// HT+TPsum from one FEE TP  
  int  getInpHT6bit(int HankCh) const { return getInp12bit(HankCh) & 0x3f; }
  int  getInpTP6bit(int HankCh) const { return getInp12bit(HankCh) >>6; }
  void compute();

  //... output
  int getOutTPsum(int ch /*0...11*/) const { return ee0outTPsum[ch];} // halfPatches, for both DSM1 boards
  int getOutHT2bit(int ch /*0...11*/) const { return ee0outHT2bit[ch];} // halfPatches, for both DSM1 boards
  int getOutTP2bit(int ch /*0...11*/) const { return ee0outTP2bit[ch];} // halfPatches, for both DSM1 boards
  int getOutHTTP2bit(int ch /*0...11*/) const { return ee0outHTTP2bit[ch];} // halfPatches, for both DSM1 boards
  int getOut16bit(int ch /*0...11*/) const { return ee0out16bit[ch];} // halfPatches, for both DSM1 boards

};

#endif

/*
 * $Log: EEdsm0Tree.h,v $
 * Revision 1.2  2009/11/18 15:50:59  pibero
 * Address several compiler warnings of the type:
 *
 * warning: deprecated conversion from string constant 'char*'
 *
 * Revision 1.1  2009/10/12 18:04:25  pibero
 * Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
 *
 * Revision 1.2  2009/02/24 03:56:18  ogrebeny
 * Corrected const-ness
 *
 * Revision 1.1  2007/08/17 01:15:36  balewski
 * full blown Endcap trigger simu, by Xin
 *
 *
 **************************************************************/


