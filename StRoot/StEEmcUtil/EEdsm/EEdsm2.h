#ifndef EEdsm2_h
#define EEdsm2_h
/**************************************************************
 * $Id: EEdsm2.h,v 1.2 2005/02/01 22:13:40 perev Exp $
 **************************************************************/
typedef unsigned short ushort;

class EEdsm2  {
  static const int nc=2; // # of used channels to DSM
  static const int njp=6; // # of EEMC JP
  ushort data[nc];
  
 private:
 public:
  
  EEdsm2();
  virtual ~EEdsm2();
  void  print(int k=0);
  void  clear();
  void setWord(int ch, ushort val);
  int getNc(){return nc;}
  ushort get3JPsum(int i3p); // i3p #[0,1]
  ushort get3JPHTthr(int i3p); // i3p #[0,1] 
  ushort getJPthr(int jp); // JP # Falk[0-5]
  
};
#endif


/* container for STAR trigger data
 * $Log: EEdsm2.h,v $
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

