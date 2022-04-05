// -*- mode:c++ -*-

#ifndef EEdsm1Tree_h
#define EEdsm1Tree_h
/**************************************************************
 * $Id: EEdsm1Tree.h,v 1.2 2009/11/18 15:50:59 pibero Exp $
 * Emulates functionality of  Endcap DSM1-tree
 **************************************************************/
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 
class EEdsm1;

class EEdsm1Tree  {  // DSM0 tree emulators

 public:
  enum { Nee1=2, Nee1BoardInpCha=6, mxTxt=16}; // # of DSM, # of 1x1 JP

 private:
  EEdsm1 *ee1; //  Level-1 boards
  char name[mxTxt];
  int mYear; // DSM algo changes w/ years

 public:
  
  EEdsm1Tree(const char *);
  void setYear(int x, int *JPth, int TPthrSelc, int HTTPthrSelc);
  ~EEdsm1Tree(); 
  void print(int k=0) const;
  void clear();
  void compute();
  int  getNboards() const { return Nee1;}

  //...... Input
  void setInp16bit(int brd, int ch, ushort val); // words
  int  getInpTPsum(int ch /*ch=0...11*/) const;// halfPatches, for both DSM1 boards
  int  getInpHT2bit(int ch ) const;
  int  getInpTP2bit(int ch ) const;
  int  getInpHTTP2bit(int ch ) const;
  int  getInp16bit(int ch ) const;
  
  //...    Output  
  int getOutEsum5bit(int ibr) const; /*ibr=0,1*/
  int getOutHTTP1bit(int ibr) const;
  int getOutTP1bit(int ibr) const; 
  int getOutJP2bit(int ibr) const;
  int getOutHT2bit(int ibr) const;
  int getOut16bit(int ibr) const; 

};

#endif

/*
 * $Log: EEdsm1Tree.h,v $
 * Revision 1.2  2009/11/18 15:50:59  pibero
 * Address several compiler warnings of the type:
 *
 * warning: deprecated conversion from string constant 'char*'
 *
 * Revision 1.1  2009/10/12 18:04:26  pibero
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


