#ifndef EEdsm1_h
#define EEdsm1_h
/**************************************************************
 * $Id: EEdsm1.h,v 1.2 2012/12/12 22:54:03 fisyak Exp $
 **************************************************************/
#ifdef __APPLE__
#include <sys/types.h>
#endif
#include <stdlib.h>

class EEdsm1  {
  enum{ nc=6, nJP=3, mxTh=3};
  ushort data[nc];
  int type;
  int mYear;// unpacking changed in 2006
  int JPthr[mxTh];
  int TPthrSelect, HTTPthrSelect; 
  int intJP11bit[nJP]; // internal
  int intJPsum13bit;
  int outJPsum5bit; 
  int outHTTP1bit;
  int outTP1bit;
  int outJP2bit;
  int outHT2bit;
  int out16bit;

 public:
  
  EEdsm1();
  virtual ~EEdsm1();
  void  print(int k=0) const;
  void  clear();
  void  compute();  
  void  setWord(int ch, ushort val);
  void  setYear(int y, int *JPth, int TPthrSelc, int HTTPthrSelc);
  void  setType(int t) {type=t;}

  int   getNc() const {return nc;}

  //.... input.....
  int getInpTPsum(int ch) const;
  int getInpHT2bit(int ch) const;
  int getInpHTTP2bit(int ch) const;// year2006+
  int getInpTP2bit(int ch) const;  // year2006+ 
  int getInp16bit(int ch) const;

  //..... output
  int getOutEsum5bit() const{ return outJPsum5bit;}
  int getOutHTTP1bit() const{ return outHTTP1bit;}
  int getOutTP1bit() const{ return outTP1bit;}
  int getOutJP2bit() const{ return outJP2bit;}
  int getOutHT2bit() const{ return outHT2bit;}  
  int getOut16bit() const{ return out16bit;}
};
#endif


/* container for STAR trigger data
 * $Log: EEdsm1.h,v $
 * Revision 1.2  2012/12/12 22:54:03  fisyak
 * Add include for APPLE
 *
 * Revision 1.1  2009/10/12 18:04:26  pibero
 * Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
 *
 * Revision 1.5  2009/02/24 03:56:18  ogrebeny
 * Corrected const-ness
 *
 * Revision 1.4  2007/08/17 01:15:36  balewski
 * full blown Endcap trigger simu, by Xin
 *
 * Revision 1.3  2006/04/05 18:34:10  balewski
 * new DSM bit assignment in 2006,
 * possibly lost backward compatibility
 * use tagged 2005 version if needed
 *
 * Revision 1.2  2005/02/01 22:13:40  perev
 * Compatibility to redhat
 *
 * Revision 1.1  2004/11/29 20:12:59  balewski
 * first
 *
 * Revision 1.2  2004/04/23 20:16:56  balewski
 * trig patch fix
 *
 * Revision 1.1  2004/02/17 03:09:17  balewski
 * *** empty log message ***
 *
 * Revision 1.1  2003/05/22 19:39:00  balewski
 * analysis of DSM data
 *
 * 
 * decode INPUT data for one board of EE-DSM Layer-1
 *
 **************************************************************/

