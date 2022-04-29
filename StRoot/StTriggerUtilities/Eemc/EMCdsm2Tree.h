// -*- mode:c++ -*-

#ifndef EMCdsm2Tree_h
#define EMCdsm2Tree_h
/**************************************************************
 * $Id: EMCdsm2Tree.h,v 1.2 2009/11/18 15:50:59 pibero Exp $
 * Emulates functionality of  Endcap DSM1-tree
 **************************************************************/
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 
class BEdsm2; 

class EMCdsm2Tree  {  // DSM0 tree emulators
 public:
  enum { Nbe2=4, Nbe2Cha=2, mxTxt=16}; // # of DSM2 for Endcap & Barrel
 private:
  BEdsm2 *be2; //  Level-2 Endcap[0], Barrel[1,2,3] boards
  char name[mxTxt];

  int mYear; // DSM algo changes w/ years
  int BEsumthr8bit, EEsumthr6bit, JPSIthrSelc2bit, BarreSide2bit, EtotThr8bit;
  int OutEndcapJP2bit;
  int OutEndcapHT2bit;
  int OutEndcapSum1bit; 
  int OutEndcapHTTP1bit;
  int OutEndcapTP1bit;
  int OutBarreJP2bit;
  int OutBarreHT2bit;
  int OutBarreSum1bit;
  int OutBarreJPSi1bit;
  int OutBarreHTTP1bit;
  int OutBarreTP1bit;
  int OutEtot1bit;

  int intBarreSum;
  int intEndcapSum;
  int intEtot;


 public:
  
  EMCdsm2Tree(const char *);
  void setYear(int x, int BEsumthr, int EEsumthr, int JPSIthrSelc, int BarreSide, int EtotThr);
  ~EMCdsm2Tree(); 
  void  print(int k=0) const;
  void  clear();
  void setInput16bit(int ibrd, int ch,  ushort val); // words
  void compute();
  //  int getNboards() { return Nee1;}
  //int getInpTPsum(int ch /*0...11*/) const;// halfPatches, for both DSM1 boards

  //...... input..........
  int getInpHT2bit(int ibr, int ch) const; // ibr=0=Endcap
  int getInpHT2bit_2(int ibr, int ch) const; // ibr=0=Endcap
  int getInpTP1bit(int ibr, int ch) const; // ibr=0=Endcap
  int getInpHTTP1bit(int ibr, int ch) const; // ibr=0=Endcap
  int getInpEsum5bit(int ibr, int ch) const; // ibr=0=Endcap
  int getInpJP2bit(int ibr, int ch) const; // ibr=0=Endcap

  //........output........
  int getOutEndcapJP2bit() const {return OutEndcapJP2bit;}
  int getOutEndcapHT2bit() const {return OutEndcapHT2bit;}
  int getOutEndcapSum1bit() const {return OutEndcapSum1bit;}
  int getOutEndcapHTTP1bit() const {return OutEndcapHTTP1bit;}
  int getOutEndcapTP1bit() const {return OutEndcapTP1bit;}
  int getIntEndcapSum() const {return intEndcapSum;}

  int getOutBarreJP2bit() const {return OutBarreJP2bit;}
  int getOutBarreHT2bit() const {return OutBarreHT2bit;}
  int getOutBarreSum1bit() const {return OutBarreSum1bit;}
  int getOutBarreJPSi1bit() const {return OutBarreJPSi1bit;}
  int getOutBarreHTTP1bit() const {return OutBarreHTTP1bit;}
  int getOutBarreTP1bit() const {return OutBarreTP1bit;}
  int getIntBarreSum() const {return intBarreSum;}

  int getOutEtot1bit() const {return OutEtot1bit;}
  int getIntEtot() const {return intEtot;}
};

#endif

/*
 * $Log: EMCdsm2Tree.h,v $
 * Revision 1.2  2009/11/18 15:50:59  pibero
 * Address several compiler warnings of the type:
 *
 * warning: deprecated conversion from string constant 'char*'
 *
 * Revision 1.1  2009/10/12 18:04:27  pibero
 * Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
 *
 * Revision 1.2  2009/02/24 03:56:19  ogrebeny
 * Corrected const-ness
 *
 * Revision 1.1  2007/08/17 01:15:38  balewski
 * full blown Endcap trigger simu, by Xin
 *
 *
 **************************************************************/


