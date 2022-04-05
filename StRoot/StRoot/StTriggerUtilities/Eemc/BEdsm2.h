#ifndef BEdsm2_h
#define BEdsm2_h
/**************************************************************
 * $Id: BEdsm2.h,v 1.1 2009/10/12 18:04:25 pibero Exp $
 **************************************************************/
typedef unsigned short ushort;

class BEdsm2  {
  static const int nc=2; // # of used channels to DSM
  static const int njp=6; // # of EEMC JP
  ushort data[nc];
  int mYear;// unpacking changed in 2006
  

 private:
 public:
  
  BEdsm2();
  virtual ~BEdsm2();
  void  print(int k=0) const;
  void  clear();
  void  setWord(int ch, ushort val); 
  void  setYear(int y){mYear=y;}
  int   getNc() const {return nc;} 
  ushort getJPthr(int jp) const; // JP # Falk[0-5], out in 2006+
  //..... input
  int getInpHT2bit(int i3p) const; // i3p #[0,1]
  int getInpHT2bit_2(int i3p) const; // i3p #[0,1]
  int getInpTP1bit(int i3p) const;  // i3p #[0,1], year2006+, selected TP threshold passing sign
  int getInpHTTP1bit(int i3p) const; // i3p #[0,1]
  int getInpEsum5bit(int i3p) const; // i3p #[0,1]
  int getInpJP2bit(int i3p) const; // i3p #[0,1] 
};
#endif


/* container for STAR trigger data
 * $Log: BEdsm2.h,v $
 * Revision 1.1  2009/10/12 18:04:25  pibero
 * Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
 *
 * Revision 1.2  2009/02/24 03:56:18  ogrebeny
 * Corrected const-ness
 *
 * Revision 1.1  2007/08/17 01:15:35  balewski
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

