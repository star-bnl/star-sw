#ifndef EEdsm1_h
#define EEdsm1_h
/**************************************************************
 * $Id: EEdsm1.h,v 1.3 2006/04/05 18:34:10 balewski Exp $
 **************************************************************/
#include <stdlib.h>

class EEdsm1  {
  static const int nc=6;
  ushort data[nc];
  int type;
  int mYear;// unpacking changed in 2006

 private:
 public:
  
  EEdsm1();
  virtual ~EEdsm1();
  void  print(int k=0);
  void  clear();
  void setWord(int ch, ushort val);
  void setYear(int y) { mYear=y;}
  int getNc(){return nc;}
  ushort getTPsum(int ch);
  ushort getHTthr(int ch);
  ushort getHTTPthr(int ch);// year2006+
  ushort getTPthr(int ch);  // year2006+
  void setType(int t) {type=t;}
  
};
#endif


/* container for STAR trigger data
 * $Log: EEdsm1.h,v $
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

